(ns examples.ircbot
  (:require [irclj.core :refer :all]
            [irclj.parser :refer :all]
            [narjure.global-atoms :refer :all]
            [narjure.core :as nar]
            [narjure.sensorimotor :refer :all]))

; - Constants -
(def channel "#narstest")
(def server "irc.freenode.org")
(def port 6667)
(def bot-nick "grazkripo")
;(def bot-nick-password "password")

; - State, with example structure -
(def state (atom {}))

(defn setup-nars [irc]
  (nars-register-operation 'op_talk (fn [args operationgoal]
                                    (do
                                      (println (str "NARS says " args))
                                      (message irc channel (str "NARS says " args))
                                      true))))

(def help [
   "Commands:"
   "!n {string} - input narsese."
   "!h - see this message."
])

(defn parse-narsese [string]
  (nars-input-narsese string)
  (println (str "NARS hears " string))
  (str "NARS hears " string))

(defn reset-nars []
  (nar/shutdown)
  (nar/run)
  "NARS reset")

(defn user-said-in-channel [state nick said]
  (let [[command string] (clojure.string/split said #" " 2)]
    (case command
      ("!n" "!nars" "!narsese")
        [(parse-narsese string) state]
      ("!r" "!reset")
        [(reset-nars) state]
      ("!h" "!help")
        [help]
      [nil]
     )))

(defn handle-command [state command text nick]
  (case command
    "PRIVMSG" (user-said-in-channel state nick text)
    [nil state]))

; - irclj interfacing procedures -
(defn working-reply [irc m string]
  (let [m2 (if (.startsWith (:target m) "#") m (assoc m :target (:nick m)))]
    (reply irc m2 string)))

(defn say! [irc m string]
  (if (nil? string)
    nil
    (working-reply irc m string)))

(defn callback [irc args]
  (let [{text :text
         target :target
         nick :nick
         command :command} args]
    (let [[response, new_state] (handle-command @state command text nick)]
      (if (sequential? response)
        (doseq [line response]
          (say! irc args line))
        (say! irc args response))
      (reset! state new_state))))

(defn debug-callback [irc & args]
  (prn args)
  (println))

(defn -main []
  (println "Connecting...")
  (def irc (connect server port bot-nick :callbacks {:privmsg callback}))
  (setup-nars irc)

  ;(identify irc bot-nick-password)

  (println "Joining")
  (join irc channel))