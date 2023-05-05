(ns radio.voice
  (:require
   [taoensso.timbre :refer [info]]
   ["@discordjs/voice" :as voice]))

(defn join-voice-channel [channel ^js guild]
  (voice/joinVoiceChannel
   (clj->js {:channelId      (.-id channel)
             :guildId        (.-id guild)
             :options        {:bitrate (* 96 1000)}
             :adapterCreator (aget guild "voiceAdapterCreator")})))

(defn disconnect! [{:keys [guild]}]
  (-> (voice/getVoiceConnection guild)
      .disconnect))

(defn connect! [{:keys [^js client guild voice stream]}]
  (-> (.fetch (aget client "guilds") guild)
      (.then (fn [guild]
               (-> (.fetch (.-channels client) voice)
                   (.then (fn [channel]
                            (let [connection   (join-voice-channel channel guild)
                                  resource     (voice/createAudioResource stream {:inlineVolume false})
                                  audio-player (voice/createAudioPlayer)]
                              (.subscribe connection audio-player)
                              (.play audio-player resource)
                              (info "Playing:" stream)))))))))
