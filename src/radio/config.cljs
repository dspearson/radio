(ns radio.config
  (:require
   [clojure.edn :as edn]))

(def version "2.0")

(def fs (js/require "fs"))

(defn get-env-var [var-name]
  (let [env-var (aget js/process.env var-name)]
    (when env-var
      (str env-var))))

(defn get-config []
  (-> (.readFileSync fs
                     (or (get-env-var "DISCORD_CONFIG")
                         ".bot-config.edn"))
      (.toString)
      (.trim)
      (edn/read-string)))
