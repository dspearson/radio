(ns radio.main
  (:require
   [radio.discord :refer [login]]
   [radio.config :refer [get-config]]
   [taoensso.timbre :refer [error]]))

(defn error-and-die [message]
  (error message)
  (js/process.exit 1))

(defn init []
  (let [;; env (if (not (= (js/process.env "HOSTNAME") "sbf")))
        {:keys [stream token guild voice text] :as config} (get-config)]
    (cond (nil? token)       (error-and-die "No token provided.")
          (nil? guild)       (error-and-die "No guild provided.")
          (nil? text)        (error-and-die "No text channel provided.")
          (when stream
            (not voice))    (error-and-die "Stream provided, but no voice channel.")
          :else (login config))))
