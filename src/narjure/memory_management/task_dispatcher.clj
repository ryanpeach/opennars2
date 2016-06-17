(ns narjure.memory-management.task-dispatcher
  (:require
    [co.paralleluniverse.pulsar
     [core :refer :all]
     [actors :refer :all]]
    [narjure.global-atoms :refer [c-bag]]
    [narjure.memory-management.local-inference.local-inference-utils :refer [get-task-id]]
    [narjure.bag :as b]
    [taoensso.timbre :refer [debug info]]
    [narjure.debug-util :refer :all])
  (:refer-clojure :exclude [promise await]))

(def aname :task-dispatcher)                                ; actor name
(def display (atom '()))                                    ; for lense output
(def search (atom ""))                                      ; for lense output filtering

(defn event?
  "return true if task is event otherwise false"
  [{:keys [occurrence]}]
  (not= occurrence :eternal))

(defn term-exists?
  "Returns true if concept bag contains term"
  [term]
  (b/exists? @c-bag term))

(defn task-handler
  "If concept, or any sub concepts, do not exist post task to concept-creator,
   otherwise, dispatch task to respective concepts. Also, if task is an event
   dispatch task to event buffer actor."
  [from [_ [task-concept-id belief-concept-id task]]]
  (let [terms (:terms task)]
    (if (every? term-exists? terms)
      (do
        (doseq [term terms]
          (when-let [{c-ref :ref} ((:elements-map @c-bag) term)]
            (cast! c-ref [:task-msg [task-concept-id belief-concept-id task]])
            )))
      (cast! (whereis :concept-manager) [:create-concept-msg [task-concept-id belief-concept-id task]]))))

(defn msg-handler
  "Identifies message type and selects the correct message handler.
   if there is no match it generates a log message for the unhandled message"
  [from [type :as message]]
  (debuglogger search display message)
  (case type
    :task-msg (task-handler from message)
    (debug aname (str "unhandled msg: " type))))

(defn initialise
  "Initialises actor:
    registers actor and sets actor state"
  [aname actor-ref]
  (reset! display '())
  (register! aname actor-ref)
  ; cache actor references for performance
  ;(set-state! {:concept-manager (whereis :concept-manager)})
  )

(defn task-dispatcher
  "creates gen-server for task-dispatcher. This is used by the system supervisor"
  []
  (gen-server
    (reify Server
      (init [_] (initialise aname @self))
      (terminate [_ _])
      (handle-cast [_ from _ message] (msg-handler from message)))))
:event-buffer