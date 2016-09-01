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
9.  answer   | success? return1 return2...  | confirmed? |

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
(def RUNNING (atom true))
(def WAITING (atom {}))
(def predefined-ops ["new-op" "input" "valid" "concept" "concepts"
                     "help" "reset" "quit" "answer" "say" CONFIRMED INVALID IN OUT])

; Server
(def WRITER (chan))
(def READER (chan))
(def PORT (if (empty? *command-line-args*)
              8080
              (first *command-line-args*)))

(def SERVER (start-server
  (fn [s info]
    (println (str "New Connection: " info))
    (s/connect s READER)
    (s/connect WRITER s))
  PORT))

; Support functions
(defn replace-with
  [l f y]
  (let [r (fn [x] (if (f x) y x))] ; if f(x) is true, then replace with y, else return x
    (map r l)))                    ; return the mapping of r onto l

; Communication
(defn sendCMD
  "Sends string over TCP, returns boolean success."
  ([op]
  (sendCMD (newid) op))
  ([id op]
  (let [msg (str id OUT op "\n")]
    (println (str "Sending: " msg))
    (go (>! WRITER msg))))
  ([id op & args]
  (let [msg (str id OUT op OUT (cstr/join OUT args) "\n")]
    (println (str "Sending: " msg))
    (go (>! WRITER msg)))))

(defn confirm
  "Quick function to send confirmation or error as a response given boolean input."
  [id success]
  (do
    (if success
      (sendCMD id CONFIRMED)
      (sendCMD id INVALID))
  success))

; Automatic true/false
(def error #(confirm % false))
(def conf  #(confirm % true))

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
  (let [id (newid)
        comb (conj [operationgoal] args)]
  ; First, add a channel for yourself
  (swap! WAITING conj [id (chan)])
  ; Then, send the message requesting an answer
  (apply sendCMD (into [id op_name] comb))
  ; Then wait for a reply
  (let [[tf & extra] (<!! (get WAITING id))]
  ; When done, delete yourself from waiting
  (swap! WAITING dissoc id)
  ; Then, process extra as narsee, and return true or false
  (and (map input-narsese args)))))

(defn new-op
  "Register a new operation."
  [op_name]
  (if (not (some #{op_name} predefined-ops))
    (do
      (nars-register-operation (partial new_op_template op_name))
      true)
    (false)))

(defn string-to-bool
  "Converts to true if it matches certain templates, false otherwise."
  [tfstr]
  (some #{tfstr} ["True" "true" "t" "T" "1" "1." "1.0"]))

(defn answer-question
  "Specifically handles answers."
  [id tfstr & args]
  (let [tf (string-to-bool tfstr)]
  (if (not (every? identity (map valid-narsese args)))
    (error id)
    (do (>!! (get WAITING id) (into [tf] args)) true))))

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
  "Quits everything"
  []
  (nar/shutdown)
  (swap! RUNNING not)
  (.close SERVER)
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
  [id op & args]
  (case op
    "new-op"     (apply sendCMD (into [id "new-op"] (map new-op args)))
    "input"      (apply sendCMD (into [id "input"] (map input-narsese args)))
    "valid"      (apply sendCMD (into [id "valid"] (map valid-narsese args)))
    "concept"    (apply sendCMD (into [id "concept"] (concepts args)))
    "concepts"   (apply sendCMD (into [id "concept"] (concepts)))
    "help"       (sendCMD id "help" (str HELP))
    "reset"      (confirm id (reset-nars))
    "quit"       (confirm id (quit))
    "answer"     (confirm id (apply answer-question (into [id] args)))
    (error id)))

; Main Read Loop
(defn readCMD
  "The main reading loop."
  []
  (println "Waiting...")
  (loop [input (<!! READER)]
  (println (str "Received: " input))
  (let [parse (parse-in input)]
    (when (> (count parse) 1) (apply process-in parse)))
  (if @RUNNING (recur (<!! READER)) (println "Quitting"))))

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
  (setup-nars)
  (readCMD))
