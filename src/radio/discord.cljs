(ns radio.discord
  (:require
   ["discord.js" :as discord :refer [GatewayIntentBits Client]]
   [taoensso.timbre :refer [info error]]
   [radio.dispatcher :refer [dispatcher]]
   [radio.slash :refer [handle-slash-command register!]]
   [radio.spotify :as spotify]
   [clojure.string :as str]
   [radio.util :refer [then on]]))

(defn client-config [{:keys [intents partials]}]
  (clj->js {:partials (mapv #(-> % name str/upper-case) partials)
            :intents  (mapv #(aget GatewayIntentBits (name %)) intents)}))

(defn client! [config]
  (->> config client-config (new Client) (assoc config :client)))

(defn create-clients! [config]
  (-> config
      spotify/state!
      spotify/db!
      client!
      spotify/client!
      spotify/server!))

(defn login [{:keys [token] :as init}]
  (let [{:keys [^js client] :as config} (create-clients! init)]
    (-> client
        (on :ready
            (fn [^js client]
              (info "Logged in as" (.-tag (.-user client)))
              (register! config)))
        (on :messageCreate
            (fn [m] (dispatcher config m)))
        (on :error
            (fn [e]
              (error "An error occurred:" e)
              (error "Error stack trace:" (.-stack e))))
        (on :interactionCreate
            (fn [interaction]
              (if (aget interaction "isCommand")
                (handle-slash-command config interaction)
                (error "Unhandled interaction type."))))
        (.login token))))
