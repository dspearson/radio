(ns radio.slash
  (:require
   ["discord.js" :refer [REST SlashCommandBuilder SlashCommandStringOption]]
   [cljs.core.async :refer [<! go]]
   [radio.config :refer [version]]
   [radio.spotify :refer [add-album clear-playlist get-current-song enqueue
                          pause get-random-top-10 merge-into-playlist play-next q stop
                          search-for-track search-for-track-add-first send-message]]
   [radio.voice :refer [connect! disconnect!]]
   [taoensso.timbre :refer [error info]]))

(defn find-voice-channel [^js interaction]
  (let [member (.-member interaction)
        voice-channel (.-channel (.-voice member))]
    voice-channel))

(defn reply
  [^js interaction reply ephemeral?]
  (.reply interaction (clj->js {:content reply :ephemeral ephemeral?})))

(def command-config {:find {:binding     "find"
                            :description "Search query"
                            :options     {:name        "query"
                                          :description "Search query string"
                                          :required    true}
                            :handler     (fn [config query interaction]
                                           (search-for-track config query #(reply interaction % true)))}

                     :info {:binding "info"
                            :description "Show information about the bot, including current queue & share links"
                            :handler (fn [{:keys [queue collaboration server-name]} _ interaction]
                                       (reply interaction (str server-name " radio, version " version ". Contact Dom if something doesn't work.\n\nPlay queue: " queue "\n\nInvitation link for collaborators: " collaboration) true))}

                     :album {:binding     "album"
                             :description "Add album to the queue"
                             :options     {:name        "query"
                                           :description "Search query string"
                                           :required    true}
                             :handler     (fn [config query interaction]
                                            (add-album config query #(reply interaction % false)))}

                     :playlist {:binding     "playlist"
                                :description "Add a playlist to the queue"
                                :options     {:name        "query"
                                              :description "Address of playlist to add"
                                              :required    true}
                                :handler     (fn [config query interaction]
                                               (go (<! (merge-into-playlist config query #(reply interaction % false) {:randomise? false}))))}

                     :playlist-shuffle {:binding     "playlist-shuffle"
                                        :description "Add a playlist to the queue, shuffling tracks first"
                                        :options     {:name        "query"
                                                      :description "Address of playlist to add"
                                                      :required    true}
                                        :handler     (fn [config query interaction]
                                                       (go (<! (merge-into-playlist config query #(reply interaction % false) {:randomise? true}))))}

                     :np {:binding     "np"
                          :description "Get the currently-playing track (if any)"
                          :handler     (fn [config _ interaction]
                                         (go (if-let [current-song (<! (get-current-song config))]
                                               (reply interaction (str "Now playing: " current-song) true)
                                               (reply interaction "No song playing." true))))}

                     :fix {:binding     "fix"
                           :description "Audio not working? Try this."
                           :handler     (fn [{:keys [state] :as config} _ interaction]
                                          (if-let [^js channel (find-voice-channel interaction)]
                                            (let [{:keys [stopped?]} @state]
                                              (if stopped?
                                                (reply interaction (q "Playback is marked as stopped.") true)
                                                (do (disconnect! config)
                                                    (connect! (assoc config :voice (.-id channel)))
                                                    (reply interaction (q "Rejoining to restart audio stream") true))))
                                            (reply interaction (q "Unable to join channel - are you even in one?") true)))}

                     :stop {:binding     "stop"
                            :description "Stop playback"
                            :handler     (fn [config _ interaction]
                                           (reply interaction "Stopping playback." false)
                                           (stop config))}

                     :join {:binding "join"
                            :description "Join bot to channel"
                            :handler (fn [config _ interaction]
                                       (if-let [^js channel (find-voice-channel interaction)]
                                         (do (connect! (assoc config :voice (.-id channel)))
                                             (reply interaction "Joined channel." true))
                                         (reply interaction "Couldn't find voice channel." true)))}

                     :leave {:binding "leave"
                             :description "Send the bot out of the channel."
                             :handler (fn [config _ interaction]
                                        (disconnect! config)
                                        (reply interaction "Sent bot out of the channel." true))}

                     :play {:binding     "play"
                            :description "Start playback"
                            :handler     (fn [{:keys [state] :as config} _ interaction]
                                           (if-let [^js channel (find-voice-channel interaction)]
                                             (go
                                               (let [{:keys [stopped?]} (swap! state assoc :voice (.-id channel))]
                                                 (if stopped?
                                                   (if (<! (play-next config))
                                                     (reply interaction "Starting playback." false)
                                                     (reply interaction "Empty queue." true))
                                                   (reply interaction "Playback already started." true))))
                                             (reply interaction "Unable to join channel - are you even in one?" true)))}

                     :q {:binding     "q"
                         :description "Add first search result"
                         :options     {:name        "query"
                                       :description "Search query string"
                                       :required    true}
                         :handler     (fn [config query interaction]
                                        (search-for-track-add-first config query #(reply interaction % true)))}

                     :lev-naive {:binding     "lev-naive"
                                 :description "Add search result with best levenshtein distance"
                                 :options     {:name        "query"
                                               :description "Search query string"
                                               :required    true}
                                 :handler     (fn [config query interaction]
                                                (search-for-track-add-first config query #(reply interaction % true)))}

                     :add {:binding     "add"
                           :description "Add by index"
                           :options     {:name        "query"
                                         :description "Selection to add from last search"
                                         :required    true}
                           :handler     (fn [{:keys [state] :as config} index-string interaction]
                                          (when-let [track (try (:id (nth (:search @state) (dec (js/parseInt index-string))))
                                                                (catch js/Error _ nil))]
                                            (enqueue config [(str "spotify:track:" track)])
                                            (reply interaction
                                                   (str "Added track: https://open.spotify.com/track/" track)
                                                   true)))}

                     :random {:binding     "random"
                              :description "Add ten random selections from the top 100 ever played by Capital Dance DJs"
                              :handler     (fn [config _ interaction]
                                             (do (get-random-top-10 config)
                                                 (reply interaction "Adding ten songs at random from Capital Dance DJs top 100." true)))}

                     :auto {:binding     "auto"
                            :description "Toggle automatic DJ on/off"
                            :handler     (fn [{:keys [state] :as config} _ interaction]
                                           (let [{:keys [capital]} @state
                                                 {:keys [auto]}    (swap! state update :auto not)]
                                             (reply interaction (str "Auto DJ: " (if auto "on!" "off!")) false)
                                             (when (and auto capital)
                                               (search-for-track-add-first
                                                config
                                                capital
                                                #(send-message config %)))))}

                     :skip {:binding     "skip"
                            :description "Skip song"
                            :handler     (fn [config _ interaction]
                                           (go
                                             (reply interaction "Skipping track." false)
                                             (<! (pause config))))}

                     :clear {:binding     "clear"
                             :description "Clear queue"
                             :handler     (fn [{:keys [spotify]} _ interaction]
                                            (let [{:keys [^js client playlist]} spotify]
                                              (reply interaction "Clearing queue." false)
                                              (go (<! (clear-playlist client playlist)))))}})

(defn create-command [{:keys [binding description options]}]
  (let [{:keys [name]} options
        command (-> (new SlashCommandBuilder)
                    (.setName binding)
                    (.setDescription description))]
    (when options
      (.addStringOption command
                        (-> (new SlashCommandStringOption)
                            (.setName name)
                            (.setDescription (:description options)))))
    (.toJSON command)))

(defn handle-slash-command [config ^js interaction]
  (let [command-name   (keyword (.-commandName interaction))
        query-option   (.get (.-options interaction) "query")
        command-data   (when query-option (js->clj (.-value query-option)))
        handler        (get-in command-config [command-name :handler])]
    (if handler
      (handler config command-data interaction)
      (reply interaction "Unknown command." true))))

(defn register!
  [{:keys [token guild client-id commands]}]
  (let [rest         (.setToken (new REST {:version "10"}) token)
        command-data (into [] (map #(create-command (get command-config %)) commands))
        commands-url (str "/applications/" client-id "/guilds/" guild "/commands")] ;; Construct the URL manually
    (-> (.put rest commands-url (clj->js {:body command-data}))
        (.then (fn [_] (info "Registered slash commands:" commands)))
        (.catch (fn [err] (error "Error registering slash command:" err))))))
