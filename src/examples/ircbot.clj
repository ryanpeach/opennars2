(ns examples.ircbot
  (:require [irclj.core :refer :all]
            [irclj.parser :refer :all]
            [narjure.global-atoms :refer :all]
            [narjure.core :as nar]
            [narjure.narsese :refer [parse2]]
            [narjure.sensorimotor :refer :all]
            [narjure.debug-util :refer :all]))

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
                                      (let [msg (str "NARS says " args)]
                                        (println msg)
                                        (message irc channel msg)
                                        true)))
  (nars-register-answer-handler (fn [task solution]
                                  (let [msg (str "NARS answer on " (narsese-print (:statement task)) "? is " (task-to-narsese solution))]
                                    (println msg)
                                    (message irc channel msg)))))

(def help [
   "Commands:"
   "!n {string} - input narsese."
   "!s {string} - input sentence."
   "!c {string} - show concept."
   "!cs - show concepts."
   "!r - reset NARS."
   "!h - see this message."
])

(defn concept [concept-str]
  (try
    (let [statement (parse2 (str concept-str "."))]
      (dissoc
        (first (narjure.bag/get-by-id @c-bag (:statement statement)))
        :ref))
    (catch Exception e (str "Invalid narsese " concept-str))))

(defn concepts []
  (:priority-index @c-bag))

(defn parse-narsese [string]
  (try
    (let [statement (parse2 string)]
      (nars-input-narsese string)
      (println (str "NARS hears " string))
      (str "NARS hears " string))
    (catch Exception e (str "Invalid narsese " string))))

(defn parse-sentence [string]
  (let [words (clojure.string/split string #" ")
        sentence (str "<(*," (clojure.string/join "," words) ") --> SENTENCE>.")]
    (parse-narsese sentence)))

(defn reset-nars []
  (nar/shutdown)
  (nar/run)
  "NARS reset")

(defn user-said-in-channel [state nick said]
  (let [[command string] (clojure.string/split said #" " 2)]
    (case command
      ("!n" "!nars" "!narsese")
        [(parse-narsese string) state]
      ("!s" "!sentence")
        [(parse-sentence string) state]
      ("!c" "!concept")
        [(concept string) state]
      ("!cs" "!concepts")
        [(concepts) state]
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

(defn -main [& args]
  (println "Connecting..." server)
  (def irc (connect server port bot-nick :callbacks {:privmsg callback}))
  (setup-nars irc)

  ;(identify irc bot-nick-password)

  (println "Joining" channel)
  (join irc channel))