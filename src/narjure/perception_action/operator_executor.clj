(ns narjure.perception-action.operator-executor
  (:require
    [co.paralleluniverse.pulsar.actors :refer [self ! whereis cast! Server gen-server register! shutdown! unregister! set-state!]]
    [co.paralleluniverse.pulsar [core :refer [defsfn]]]
    [narjure.actor.utils :refer [defactor]]
    [taoensso.timbre :refer [debug info]]
    [narjure.debug-util :refer :all]
    [narjure.global-atoms :refer :all])
  (:refer-clojure :exclude [promise await]))

(def aname :operator-executor)                              ; actor name
(def display (atom '()))                                    ; for lense output
(def search (atom ""))                                      ; for lense output filtering
(def registered-operator-functions (atom {}))

(defn operator-execution-handler
  "Processes an :operator-execution-msg:
    executes operation with optionally supplied parameters
    if feedback msg required posts :sentence-msg to task creator"
  [from [msg operationgoal]]
  (let [feedback (assoc operationgoal :task-type :belief
                                      :occurrence @nars-time
                                      :budget [0.9 0.8 0.5])
        operation (:statement operationgoal)
        arguments (second operation)
        operator (nth operation 2)]
    (try (let [func (@registered-operator-functions operator)]
           (when (not= nil func)
             (func arguments)))
      (catch Exception e (debuglogger search display (str "operator execution error " (.toString e)))))
    (output-task :execution operationgoal)
    (cast! (whereis :task-creator) [:derived-sentence-msg [feedback (:budget feedback) (:evidence feedback)]]))) ;derived-sentence so we keep evidence trail

(defn msg-handler
  "Identifies message type and selects the correct message handler.
   if there is no match it generates a log message for the unhandled message "
  [from [type :as message]]
  (debuglogger search display message)
  (case type
    :operator-execution-msg (operator-execution-handler from message)
    (debug aname (str "unhandled msg: " type))))

(defn initialise
  "Initialises actor:
      registers actor and sets actor state"
  [aname actor-ref]
  (reset! display '())
  (register! aname actor-ref)
  ; cache task-creator reference for performance
  (set-state! {:task-creator (whereis :task-creator)}))

(defn operator-executor []
  "creates gen-server for operator-executor. This is used by the system supervisor"
  (gen-server
    (reify Server
      (init [_] (initialise aname @self))
      (terminate [_ _])
      (handle-cast [_ from _ message] (msg-handler from message)))))
