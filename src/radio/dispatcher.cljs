(ns radio.dispatcher
  (:require
   [radio.spotify :as spotify]
   [clojure.string :as str]
   [taoensso.timbre :refer [info]]))

(defn shutdown []
  (js/process.exit 0))

(defn parse-song [m]
  (-> m
      (str/replace "```" "")
      (str/replace #"seen.*" "")
      (str/replace "Capital Dance: " "")
      (str/replace #"\n\s*" "")))

(defn dispatcher [{:keys [state] :as config} ^js message]
  (when (get-in config [:spotify :client])
    (let [{:keys [auto]} @state
          content (.-content message)]
      (cond (re-find #"Capital Dance:" content)
            (let [found-song (parse-song content)]
              (info "Found song:" found-song)
              (swap! state assoc :capital found-song)
              (when auto
                (spotify/search-for-track-add-first
                 config
                 found-song
                 #(spotify/send-message config %))))
            (= content "%die") (shutdown)))))
