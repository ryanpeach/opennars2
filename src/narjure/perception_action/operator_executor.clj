(ns narjure.perception-action.operator-executor
  (:require
    [co.paralleluniverse.pulsar
     [core :refer :all]
     [actors :refer :all]]
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
                                      :occurrence 0)
        operation (:statement operationgoal)
        arguments (rest (second operation))
        operator (nth operation 2)]
    (try (let [func (@registered-operator-functions operator)]
           (if (not= nil func)
             (let [success (func arguments operationgoal)]
               (when success          ;when there is an operator function only give feedback if it
                (output-task :execution operationgoal)
                (cast! (whereis :task-creator) [:sentence-msg feedback])
                (when (coll? success)
                  (doseq [custom-feedback success]
                    (cast! (whereis :task-creator) [:sentence-msg custom-feedback])))))
             (do
               (output-task :execution operationgoal)
               (cast! (whereis :task-creator) [:sentence-msg feedback]))))
      (catch Exception e (debuglogger search display (str "operator execution error " (.toString e)))))
    (output-task :execution operationgoal)
    (cast! (whereis :task-creator) [:sentence-msg feedback]))) ;derived-sentence so we keep evidence trail

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
  ; (set-state! {:task-creator (whereis :task-creator)})
  )

(defn operator-executor []
  "creates gen-server for operator-executor. This is used by the system supervisor"
  (gen-server
    (reify Server
      (init [_] (initialise aname @self))
      (terminate [_ _])
      (handle-cast [_ from _ message] (msg-handler from message)))))
