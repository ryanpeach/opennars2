(ns examples.interface
  (:require [aleph.tcp :as tcp]
            [narjure.global-atoms :refer :all]
            [narjure.core :as nar]
            [narjure.sensorimotor :refer :all]
            [clojure.string :as :cstr]
            [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout]])
  (:gen-class))

; The help dialogue for the server
(def HELP "Client Querys:
!new_op {string} - name a new operation
!n {string} - input narsese.
!c {string} - show concept.
!cs - show concepts.
!r - reset NARS.
        
Server Response:
*answ {question} {answer}
*invalid
*confirmed
*online
*offline
          
Server Commands:
?say &args - execute operation
          
Client Response:
$op_name $t/f$ & ret
$invalid")

(defn echo-handler [s info]
  (s/connect s s))

; Support functions
(defn exists?
  "Tests: v equals some item in l?"
  [v l]
  (some #(= v %) l))

(defn all?
  "[l]:   Tests: All items in l are true.
   [f l]: Tests: All items in l are evaluated true by f?"
  ([l]
  (all? (fn [x] x) l))
  ([f l]
  (not (some (complement f) l))))

; Communication 
(defn sendTCP
  "Sends string over TCP, returns boolean success."
  [string]
  (do 
    (println (str "Sending: " string))
    (println string)
    true))
  
(defn confirm
  "Quick function to send confirmation or error as a response given boolean input."
  [success]
  (if success
    (sendTCP "*confirmed")
    (sendTCP "*invalid")))

(defn parse-in
  "Prints the string as received and splits it in two."
  [string] (do 
    (println (str "Received: " string))
    (cstr/split string #" " 2)))

(def error (partial confirm false))

; Some string functions
(defn rm-white [s] (apply str (filter #(not (= \space %)) (into [] s))))
(defn new-uuid [] (str (java.util.UUID/randomUUID)))

; Asyncronous listening function
(def waiting (atom {}))
(defn new_op_template
  "Used as the template to define new operations in narsee over the server."
  [op_name args operationgoal]
  ; First, create the key we will use for this particular call
  (let [id (new-uuid)
        k (rm-white (str id ":?:" op_name "@-|" (cstr/join "," args) "|-@" operationgoal))]
  ; First, add a channel for yourself
  (swap! waiting conj [k (chan)])
  ; Then, send the message requesting an answer
  (sendTCP k)
  ; Then wait for a reply
  (let [[tf & extra] (<!! (get waiting id))]
  ; When done, delete yourself from waiting
  (swap! waiting dissoc k)
  ; Then, process extra as narsee
  (if (all? (map receive-narsee extra))
      (sendTCP "*confirmed")
      (sendTCP "*invalid_narsee"))
  ; And return true or false
  tf)))

(defn new_op
  "Register a new operation."
  [op_name]
  (do 
  (nars-register-operation (partial new_op_template op_name))
  (confirm true))

(defn classify
  [input]
  (let [sp (cstr/split " " input 2)
        ans (cstr/split ":>:" input)]
  (cond
    (> (count ans) 1) (apply answ-question ans)
    (= (count sp) 2)  (apply handle-double sp)
    (= (count sp) 1)  (apply handle-single sp)
    :else             nil)))
  
(defn handle-single
  "Handles client statements."
  [input]
    (case input
      "!Quit" (println "Quitting...")
      (error)))
    
(defn handle-double
  "Handles complex client statements."
  [command args]
  (case command
    "!echo" (println args)
    (error)))
  
(defn answer-question
  "Specifically handles answers."
  [id tf & extra]
  (>!! (get waiting id) (into [tf] extra))) ; put [tf & extra] onto channel in waiting at key id

; Setup Narjure
(defn parse-narsese
  "Input the received Narsese into the system."
  [string]
  (try
    (let [statement (parse2 string)]
      (nars-input-narsese string)
      (println (str "NARS hears " string))
      true)
    (catch Exception e false)))

(defn list-to-narsee [l]
  (let [success (map parse-narsee l)] (all? success))

(defn setup-nars
  "Registers the operation and answer handler"
  []
  (nars-register-operation 'op_say (fn [args operationgoal]
                                      (let [msg (str "?say " args)]
                                        (sendTCP msg)
                                        true)))
  (nars-register-answer-handler (fn [task solution]
                                  (let [msg (str "?answ " (narsese-print (:statement task)) "? " (task-to-narsese solution))]
                                    (sendTCP msg)))))
    
(defn -main [& args]
  (println "Connecting...")
  (tcp/start-server echo-handler {:port 10001})
  (setup-nars)
  (repl)
  
  ;(if (not (exists? "--nogui" args))
  ;  (lense/-main)
  ;  (set-fast-speed)