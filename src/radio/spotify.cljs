(ns radio.spotify
  (:require
   [cljs.nodejs :refer [process]]
   ["sqlite3" :refer [Database]]
   [taoensso.timbre :refer [info error]]
   [clojure.string :as str]
   ["spotify-web-api-node" :as api]
   [cljs.core.async :refer [go <! >! chan close!]]
   [cljs.core.async.interop :refer-macros [<p!]]
   [radio.voice :refer [disconnect! connect!]]
   ["http" :as h]))

(defn send-message [{:keys [client text]} message]
  (info "In send-message")
  (let [channel (-> client .-channels .-cache (.get text))]
    (info "Channel:" text)
    (info "Sending message:" message)
    (.send channel message)))

(defn pop-head! [^js client playlist]
  (let [output-chan (chan)]
    (go
      (let [track (<p! (-> (.getPlaylistTracks client playlist (clj->js {:limit 1 :offset 0}))
                           (.then (fn [coll]
                                    (info "Got playlist tracks:" coll)
                                    (let [track (first (mapv #(str "spotify:track:" (:id (:track %)))
                                                             (-> (js->clj coll :keywordize-keys true) :body :items)))]
                                      (when track
                                        (info "Removing track from top of playlist:" track)
                                        (-> (.removeTracksFromPlaylist client playlist (clj->js [{:uri track :positions [0]}]))
                                            (.then (fn [x] (info "Successfully removed tracks from top of playlist") true))
                                            (.catch (fn [e] (error "Error removing:" e) false))))
                                      track)))
                           (.catch (fn [e]
                                     (error "Error fetching tracks:" e)
                                     false))))]
        (>! output-chan track)
        (close! output-chan)))
    output-chan))

(defn play-next [{:keys [spotify]}]
  (let [playing? (chan)]
    (go
      (let [{:keys [^js client
                    device
                    playlist]} spotify
            track              (<! (pop-head! client playlist))]
        (if track
          (let [url (str "https://open.spotify.com/track/" (nth (str/split track ":") 2))]
            (info "Got track to play:" track)
            (try
              (.play client (clj->js {:device_id device
                                      :uris      [track]}))
              (>! playing? true)
              (catch js/Error e
                (error "Error:" e))))
          (>! playing? false))))
    playing?))

(defn get-current-song [{:keys [spotify]}]
  (let [{:keys [^js client]} spotify
        reply-channel (chan)]
    (go
      (-> (.getMyCurrentPlayingTrack client)
          (.then (fn [x]
                   (go
                     (let [now-playing (or (-> (js->clj x :keywordize-keys true)
                                               :body :item :external_urls :spotify)
                                           false)]
                       (info "Raw data:" x)
                       (info "Now playing:" now-playing)
                       (>! reply-channel now-playing)
                       (close! reply-channel)))))))
    reply-channel))

(defn dispatch-event
  "Dispatch an event to the appropriate handler."
  [{:keys [state] :as config} evt]
  (info "Got event:" evt)
  (info "Current state:" @state)
  (let [{:keys [stopped? voice]} @state]
    (case evt
      "playing" (go (when stopped?
                      (swap! state assoc :stopped? false)
                      (connect! (assoc config :voice voice))))

      "paused"  (when-not stopped?
                  (go (when-not (<! (play-next config))
                        (send-message config "Queue empty, leaving voice channel.")
                        (swap! state assoc :stopped? true)
                        (disconnect! config))))
      true)))

(defn parse-chunks [chunks]
  (try (js->clj
        (js/JSON.parse
         (js->clj
          (str (apply str (map js->clj chunks)))))
        :keywordize-keys true)
       (catch js/Error _ {})))

(defn handle-event [config ^js req ^js res]
  (let [chunks (atom [])]
    (.on req "data" (fn [chunk] (swap! chunks conj chunk)))
    (.on req "end" (fn []
                     (let [{:keys [evt]} (parse-chunks @chunks)]
                       (go (dispatch-event config evt))
                       (.end res))))))

(defn server! [{:keys [port] :as config}]
  (assoc config
         :server
         (-> h
             (.createServer (fn [^js req ^js res] (handle-event config req res)))
             (.listen port))))

(defn state! [config]
  (assoc config :state (atom {:stopped? true
                              :voice    nil
                              :capital  nil
                              :search   nil
                              :shuffle? false
                              :auto     false})))

(defn pause [{:keys [spotify]}]
  (let [{:keys [^js client]} spotify
        response-chan (chan)]
    (try
      (-> (.pause client)
          (.then (fn [x] (go (info "Successfully paused playback.")
                             (>! response-chan true)
                             (close! response-chan)))))
      (catch js/Error e
        (go
          (error "Error:" e)
          (>! response-chan false)
          (close! response-chan))))
    response-chan))

(defn search-tracks [^js client track-name]
  (info "Searching for:" track-name)
  (-> (.searchTracks client track-name)
      (.then (fn [data]
               (let [items (-> (js->clj data :keywordize-keys true) :body :tracks :items)]
                 (info "Got items:" items)
                 items)))))

(defn levenshtein-distance
  "Calculate the Levenshtein (edit) distance iteratively using
  a grid representation"
  [seqa seqb]
  (let [va   (vec seqa)
        vb   (vec seqb)
        clen (inc (count va))
        rlen (inc (count vb))]
    (loop [r  1
           c  1
           rx (vec (range clen))
           ry (assoc rx 0 1)]
      (cond
        (= r rlen) (peek rx)
        (= c clen) (recur (inc r) 1 ry (assoc ry 0 (inc r)))
        :else      (let [k  (if (= (va (dec c)) (vb (dec r))) 0 1)
                         ry (assoc ry c (min (+ (rx c) 1)          ; above
                                             (+ (ry (dec c)) 1)    ; left
                                             (+ (rx (dec c)) k)))] ; diagonal
                     (recur r (inc c) rx ry))))))

(defn get-closest-match-index
  "Find the closest matching string in a colletcion."
  [coll target]
  (let [distances (mapv #(levenshtein-distance target %) coll)
        closest   (apply min distances)]
    (->> distances
         (keep-indexed (fn [idx val] (when (= val closest) idx)))
         first)))

(defn parse-url
  "Given a Spotify share link, return the basename."
  [url]
  (when url
    (->> url
         str/trim
         (re-find #"/([^/\?]+)(?=$|\?.*$)")
         second)))

(defn get-playlist-tracks
  ([^js client playlist]
   (get-playlist-tracks client playlist false))
  ([^js client playlist parse?]
   (let [output-chan (chan)
         playlist    (if parse? (parse-url playlist) playlist)]
     (info "Got playlist:" playlist)
     (go
       (loop [trackcoll []
              offset    0]
         (let [tracks      (<p! (-> (.getPlaylistTracks client playlist (clj->js {:limit 50 :offset offset}))
                                    (.then (fn [x] (mapv #(str "spotify:track:" (:id (:track %)))
                                                         (-> (js->clj x :keywordize-keys true) :body :items))))))
               track-count (count tracks)]
           (info "Track count:" track-count)
           (info "Offset:" offset)
           (info "Trackcoll:" trackcoll)
           (if (< track-count 50)
             (do (>! output-chan (concat trackcoll tracks))
                 (close! output-chan))
             (recur (concat trackcoll tracks)
                    (+ offset track-count))))))
     output-chan)))

(defn promisify-db-all [db query]
  (js/Promise.
   (fn [resolve reject]
     (.all db query (fn [err rows]
                      (if err
                        (reject err)
                        (resolve rows)))))))

(defn q [s]
  (str "```" s "```"))

(defn trackcoll->matcher [coll]
  (map #(str (-> % :artists first :name) " - " (:name %)) coll))

(defn trackcoll->str [coll]
  (let [count (atom 0)]
    (str/join "\n" (map #(str "#" (swap! count inc) ": "
                              (-> % :artists first :name)
                              " - "
                              (:name %))
                        coll))))

(defn search-for-track [{:keys [state spotify]} track-name callback]
  (go
    (let [{:keys [^js client]} spotify]
      (try
        (let [tracks (<p! (search-tracks client track-name))]
          (swap! state assoc :search tracks)
          (info "Got tracks:" tracks)
          (callback (q (str "Found songs:\n\n"
                            (trackcoll->str tracks)))))
        (catch js/Error e
          (error "Error:" e))))))

(defn remove-tracks [^js client playlist-id tracks]
  (let [response-chan (chan)]
    (go
      (do
        (doseq [t (partition-all 100 (map #(assoc {} :uri %) tracks))]
          (<p! (-> (.removeTracksFromPlaylist client playlist-id (clj->js t))
                   (.then (fn [_] (info "Successfully removed: " (count t) "tracks") true))
                   (.catch (fn [e] (error "Error removing:" e) false)))))
        (>! response-chan true)
        (close! response-chan)))
    response-chan))

(defn clear-playlist-recursive [^js client playlist-id offset]
  (info "In clear playlist recursive")
  (let [response-chan (chan)]
    (go (let [tracks     (<! (get-playlist-tracks client playlist-id false))]
          (info "Track uris:" tracks)
          (when-not (empty? tracks)
            (<! (remove-tracks client playlist-id tracks)))
          (info "Got past when.")
          (>! response-chan {:ok true})
          (info "Got past put.")
          (close! response-chan)
          (info "Response channel closed for recursive delete")))
    response-chan))

(defn clear-playlist [client playlist-id]
  (let [response-chan (chan)]
    (go (>! response-chan (<! (clear-playlist-recursive client playlist-id 0)))
        (close! response-chan))
    response-chan))

(defn stop [{:keys [state] :as config}]
  (swap! state assoc :stopped? true)
  (pause config))

(defn add-to-playlist [^js client playlist coll]
  (let [response-chan (chan)]
    (go
      (info "Tracks to add:" coll)
      (-> (.addTracksToPlaylist client playlist (clj->js (vec coll)))
          (.then (fn [x] (go (do (info "Added!")
                                 (>! response-chan true)
                                 (close! response-chan)))))
          (.catch (fn [e] (go (do (error "Error:" e)
                                  (>! response-chan false)
                                  (close! response-chan)))))))
    response-chan))

(defn enqueue [{:keys [spotify]} tracks]
  (go
    (let [{:keys [^js client playlist]} spotify]
      (<! (add-to-playlist client playlist tracks)))))

(defn search-album [^js client query]
  (info "Searching for:" query)
  (-> (.search client query (clj->js ["album"]))
      (.then (fn [data]
               (let [items (map :id (-> (js->clj data :keywordize-keys true) :body :albums :items))]
                 (info "Got items:" items)
                 items)))
      (.catch (fn [e]
                (error "Error: e")
                false))))

(defn add-album [{:keys [spotify] :as config} album callback]
  (let [{:keys [^js client]} spotify]
    (go
      (if-let [album-uri (first (<p! (search-album client album)))]
        (try
          (info "Parsed album:" album-uri)
          (-> (.getAlbum client album-uri)
              (.then (fn [x]
                       (callback (str "Album added: https://open.spotify.com/album/" album-uri))
                       (enqueue config
                                (mapv #(str "spotify:track:" (:id %)) (-> (js->clj x :keywordize-keys true)
                                                                          :body :tracks :items))))))
          (catch js/Error e
            (error "Error:" e)))
        (callback "Couldn't find anything, sorry")))))

(defn merge-into-playlist [{:keys [spotify]} playlist-source callback {:keys [randomise?]}]
  (let [{:keys [^js client playlist]} spotify]
    (go
      (try
        (let [tracks      (let [x (<! (get-playlist-tracks client playlist-source true))]
                            (if randomise? (shuffle x) x))
              track-count (count tracks)]
          (callback (str "Playlist added, " track-count " tracks: " playlist-source))
          (when (pos? track-count)
            (doseq [t (partition-all 40 tracks)]
              (<! (add-to-playlist client playlist t)))))
        (catch js/Error e
          (callback (q "Couldn't get any information about the playlist, sorry."))
          (error "Error:" e))))))

(defn search-for-track-add-lev [{:keys [spotify] :as config} track-name callback]
  (go
    (try
      (let [{:keys [^js client]} spotify]
        (if-let [tracks (<p! (search-tracks client track-name))]
          (let [track-candidates (trackcoll->matcher tracks)
                closest-match (get-closest-match-index track-candidates track-name)
                track (:id (nth tracks closest-match))]
            (enqueue config [(str "spotify:track:" track)])
            (callback (str "Added track: https://open.spotify.com/track/" track) true))
          (callback (str "No track found for that search query.") true)))
      (catch js/Error e
        (error "Error:" e)))))

(defn search-for-track-add-first [{:keys [spotify] :as config} track-name callback]
  (go
    (try
      (let [{:keys [^js client]} spotify]
        (if-let [track (:id (first (<p! (search-tracks client track-name))))]
          (do
            (enqueue config [(str "spotify:track:" track)])
            (callback (str "Added track: https://open.spotify.com/track/" track) true))
          (callback (str "No track found for that search query.") true)))
      (catch js/Error e
        (error "Error:" e)))))

(defn track->uri [{:keys [spotify]} track-name]
  (let [{:keys [^js client]} spotify
        response-chan        (chan)]
    (go
      (try
        (let [tracks (<p! (search-tracks client track-name))]
          (>! response-chan tracks))
        (catch js/Error e
          (error "Error:" e))))
    response-chan))

(defn get-random-top-10 [{:keys [db] :as config}]
  (go
    (try
      (-> (promisify-db-all db "select song from songs order by count desc limit 1000")
          (.then
           (fn [songs]
             (let [clj-songs      (js->clj songs)
                   shuffled-songs (take 10 (shuffle (map (fn [x] (-> x vals first)) clj-songs)))]
               (println "Shuffled songs:" shuffled-songs) ; Debugging line
               (go
                 (doseq [song shuffled-songs]
                   (search-for-track-add-first config song (fn [])))))))
          (.catch #(println "Error:" %)))
      (catch js/Error e
        (error "Error getting random top 10:" e)))))

(defn handle-refresh-token
  [^js client]
  (.refreshAccessToken client
                       (fn [err new-token]
                         (if err
                           (error "Error occurred while refreshing access token:" err)
                           (do
                             (info "Access token refreshed.")
                             (.setAccessToken client (.-access_token ^js (.-body new-token))))))))

(defn set-interval-immediate
  [f interval]
  (f)                          ;; Execute the function immediately
  (js/setInterval f interval)) ;; Schedule the function to run periodically

(defn db!
  [config]
  (assoc config :db (new Database (str (aget (.-env process) "HOME") "/.cldj.db"))))

(defn client!
  [{:keys [spotify] :as config}]
  (if-let [{:keys [id secret refresh-token]} spotify]
    (let [client (new api (clj->js {:clientId     id
                                    :clientSecret secret
                                    :redirectUri  "https://callbackurl"}))]
      (.setRefreshToken client refresh-token)
      (set-interval-immediate #(handle-refresh-token client) (* 30 60 1000))
      (assoc-in config [:spotify :client] client))
    config))
