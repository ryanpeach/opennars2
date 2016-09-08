(ns examples.tcp.interface
  (:require [narjure.global-atoms :refer :all]
            [narjure.core :as nar]
            [narjure.sensorimotor :refer :all]
            [narjure.narsese :refer [parse2]]
            [narjure.debug-util :refer :all]
            [manifold.stream :as s]
            [clojure.string :as cstr]
            [clojure.core.async :refer [>! <! >!! <!! chan go]]
            [examples.tcp.server :refer :all])
  (:gen-class))

; The help dialogue for the server
(def HELP
"
        Client Querys                       |         Expected Reply
       op    |       args                   |     op     |          args
1.  new-op   | opname1 opname2...           | new-op     | success1? success2?...
2.  input    | narsese1 narsese2...         | input      | success1? success2?...
3.  valid    | narsese1 narsese2...         | valid      | tf1 tf2...
4.  concept  | narsese1 narsese2...         | concept    | conceptstr1/invalid conceptstr2/invalid...
5.  concepts |                              | concept    | conceptstr1 conceptstr2...
6.  help     |                              | help       | helpstring
7.  reset    |                              | confirmed? |
8.  quit     |                              | confirmed? |
9.  answer   | success? return1 return2...  | confirmed? | validnarsese1? validnarsese2?

        Server Querys                       |         Expected Reply
       op    |       args                   |     op     |          args
9.  op-name  | arg1 arg2...                 | answer     | success? return1 return2...
10. say      | narsese                      |            |
11. answer   | task solution                |            |
")

; ID & Dividers
(defn newid [] (str (java.util.UUID/randomUUID)))
(def IN ":<:")
(def OUT ":>:")
(def CONFIRMED "confirmed")
(def INVALID "invalid")
(def RUNNING true)
(def WAITING (atom {}))
(def OPS (atom {}))
(def predefined-ops ["new-op" "input" "valid" "concept" "concepts"
                     "help" "reset" "quit" "answer" "say" CONFIRMED INVALID IN OUT])

; Server
(def STREAMS (atom {}))
(def PORT (if (empty? *command-line-args*)
              8080
              (first *command-line-args*)))

; Support functions
(defn replace-with
  [l f y]
  (let [r (fn [x] (if (f x) y x))] ; if f(x) is true, then replace with y, else return x
    (map r l)))                    ; return the mapping of r onto l

; Communication
(defn send-stream
  "Sends string over TCP to given stream."
  ([ch op]
  (send-stream ch (newid) op))
  ([ch id op]
  (let [msg (str id OUT op "\n")]
    (println (str "Sending: " msg))
    (put! ch msg)))
  ([ch id op & args]
  (let [msg (str id OUT op OUT (cstr/join OUT args) "\n")]
    (println (str "Sending: " msg))
    (put! ch msg))))
  
(defn send-all
  "Sends string over TCP to all clients."
  [arg0 & args]
  (map #(apply send-stream (into [% arg0] args)) STREAMS))

(defn confirm
  "Quick function to send confirmation or error as a response given boolean input."
  [ch id success & args]
  (do
    (if success
      (apply send-stream (into [ch id CONFIRMED] args))
      (apply send-stream (into [ch id INVALID] args)))
  success))

; Automatic true/false
(def error #(confirm % % false))
(def conf  #(confirm % % true))

; Narsese Input
(defn input-narsese
  "Input the received Narsese into the system."
  [string]
  (try
    (let [statement (parse2 string)]
      (nars-input-narsese string)
      (println (str "NARS hears " string))
      true)
    (catch Exception e false)))

(defn valid-narsese
  "Checks if input narsese is valid."
  [string]
  (try
    (do (parse2 string) true)
    (catch Exception e false)))

; Asyncronous listening function
(defn new_op_template
  "Used as the template to define new operations in narsee over the server."
  [op_name args operationgoal]
  ; First, create the key we will use for this particular call
  (try 
    (let [id (newid)
          comb (into [operationgoal] args)
          operation (str "(^" op_name (cstr/join ", " args) ")")
          swrite (get OPS opname)
          sread (promise)]
      ; Create a waiting reference at this id
      (swap! WAITING conj [id sread]) 
      ; Send the message to the appropriate stream requesting an answer
      (apply send-stream (into [swrite id op_name] comb))
      ; Then wait for a reply
      (let [[tf concequence] @sread]
        ; Destroy the waiting reference
        (swap! WAITING dissoc id)
        ; Then, process concequence as narsee, and return true or false, Confirm receipt
        (confirm swrite id
          (input-narsese (str "<" operation "/=>" concequence ">. :|:")))
        ; Return the given true/false
        tf))
    ; On an exception, remove the opname from OPS so it can be reinitialized, and return false
    (catch Exception e
      (swap! OPS dissoc opname)
      (swap! WAITING dissoc id)
      (println e)
      (false))))

(defn new-op
  "Register a new operation."
  [ch op_name]
  (if (not (or (some #{op_name} predefined-ops)
               (some #{op_name} (keys OPS))))
    (do 
        (swap! OPS conj [op_name ch])
        (nars-register-operation (partial new_op_template op_name)) true)
    (false)))

(defn string-to-bool
  "Converts to true if it matches certain templates, false otherwise."
  [tfstr]
  (some #{tfstr} ["True" "true" "t" "T" "1" "1." "1.0"]))

(defn answer-question
  "Specifically handles answers."
  [ch id tfstr concequence]
  (try
    (let [tf (string-to-bool tfstr)
          w (get WAITING id)]
      (deliver w (conj [tf] concequence)))
    (catch Exception e (println e) (error ch id))))

; Copied from ircbot
(defn concept
  "Show a concept"
  [concept-str]
  (try
    (let [statement (parse2 (str concept-str "."))]
      (dissoc
        (first (narjure.bag/get-by-id @c-bag (:statement statement)))
        :ref))
    (catch Exception e nil)))

(defn concepts
  "Show all the concepts"
  ([]
  (:priority-index @c-bag))              ; get all concepts. FIXME: Is this a list?
  ([args]
  (let [returns (map concept args)]      ; get a list containing each concept or an error
    (replace-with returns not INVALID))) ; replace errors with INVALID and return
  ([arg0 & args] (apply concepts (into [arg0] args))))            ; can take them either as a list or as a bunch of items

; Read Loop
(defn quit
  "Closes a client connection."
  [ch id]
  (confirm ch id)
  (s/close! ch)
  true)

(defn reset-nars
  "Resets the system"
  []
  (nar/shutdown)
  (nar/run)
  true)

(defn parse-in
  "Prints the string as received and splits it in two."
  [string]
    (println (str "Parsing: " string))
    (map cstr/trim (cstr/split string (re-pattern IN))))

(defn process-in
  [ch id op & args]
  (case op
    "new-op"     (apply send-stream (into [ch id "new-op"] (map #(new-op ch %) args)))
    "input"      (apply send-stream (into [ch id "input"] (map input-narsese args)))
    "valid"      (apply send-stream (into [ch id "valid"] (map valid-narsese args)))
    "concept"    (apply send-stream (into [ch id "concept"] (concepts args)))
    "concepts"   (apply send-stream (into [ch id "concept"] (concepts)))
    "help"       (send-stream ch id "help" (str HELP))
    "reset"      (confirm ch id (reset-nars))
    "quit"       (quit ch)
    "answer"     (try (apply answer-question (into [ch id] args)) (catch Exception e (println e) false))
    (error id)))

; Main Read Loop
(def SERVER (start-server
  (fn 
    "REF: https://github.com/ztellman/aleph/blob/master/examples/src/aleph/examples/tcp.clj"
    [ch info]
      (println (str "New Connection: " info))
      (d/loop []

        ;; take a message, and define a default value that tells us if the connection is closed
        (-> (s/take! s ::none)

          (d/chain

            ;; process message
            (fn [msg]
              (when-not (= ::none msg)
                (println (str "Received: " input))
                (let [parse (into [ch] (parse-in input))]
                  (when (>= (count parse) 3)
                    (apply process-in parse)
                    true))))

            ;; if we were successful in our response, recur and repeat
            (fn [result]
              (when result
                (d/recur))))

          ;; if there were any issues on the far end, send a stringified exception back
          ;; and close the connection
          (d/catch
            (fn [ex]
              (s/put! ch (str -1 OUT "quit" OUT ex))
              (s/close! ch)))))
  PORT))

;(defn shutdown
;  "Quits everything"
;  []
;  (map quit STREAMS)
;  (nar/shutdown)
;  (swap! RUNNING not)
;  (.close SERVER)
;  true)

; Startup
(defn setup-nars
  "Registers the operation and answer handler"
  []
  (nars-register-operation 'op_say (fn [args operationgoal]
                                      (let [allargs (into [operationgoal] args)
                                            total   (into [(newid) "say"] allargs)]
                                      (apply sendCMD total))))
  (nars-register-answer-handler (fn [task solution]
                                  (let [taskn (str (narsese-print (:statement task)))
                                        soln  (str (task-to-narsese solution))]
                                    (sendCMD (newid) "answer" taskn soln)))))

;(defn -test
;  []
;  (>!! reader (str 1 IN "input" IN "<a --> b>."))
;  (>!! reader (str 2 IN "input" IN "<b --> c>."))
;  (>!! reader (str 3 IN "input" IN "<a --> c>?"))
;  (loop [] (println (<!! writer)) (recur)))

(defn -main [& args]
  ; Setup nars answer-handler and new-op function
  (println "Running!")
  (setup-nars))
