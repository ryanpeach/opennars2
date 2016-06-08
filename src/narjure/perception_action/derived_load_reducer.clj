(ns narjure.perception-action.derived-load-reducer
  (:require
    [co.paralleluniverse.pulsar.actors :refer [! spawn gen-server register! cast! Server self whereis shutdown! unregister! set-state! state]]
    [narjure.narsese :refer [parse2]]
    ;[narjure.actor.utils :refer [defactor]]
    [taoensso.timbre :refer [debug info]]
    [narjure.bag :as b]
    [narjure.control-utils :refer :all]
    [narjure.debug-util :refer :all])
  (:refer-clojure :exclude [promise await]))

(def aname :derived-load-reducer)                           ; actor name
(def display (atom '()))                                    ; for lense output
(def search (atom ""))                                      ; for lense output filtering

(def max-derived-sentences 50)                              ; task bag capacity
(def max-selections 10)                                     ; max selections per cycle
(def bag (atom (b/default-bag max-derived-sentences)))      ; task bag

(defn system-time-tick-handler
  "select n sentences from input bag and post to :task-creator"
  []
  (doseq [n (range (min max-selections (b/count-elements @bag)))]
    (let [[element bag'] (b/get-by-index @bag (selection-fn @bag))
          msg [:derived-sentence-msg [(:id element)]]]
      (reset! bag bag')
      (cast! (:task-creator @state) msg)
      (debuglogger search display [:forward msg]))))

(defn derived-sentence-handler
  "adds sentence to input-bag and selects n senetences on system-time-tick"
  [from [msg sentence]]
  (let [elem {:id sentence :priority (first (:budget sentence))}]
    (debuglogger search display [:add elem])
    (swap! bag b/add-element elem)))

(defn register-task-creator-handler
  "add task-creator reference to state"
  [from]
  (set-state! {:task-creator from}))

(defn initialise
  "Initialises actor:
      registers actor and sets actor state"
  [aname actor-ref]
  (reset! display '())
  (register! aname actor-ref))

(def display (atom '()))
(defn msg-handler
  "Identifies message type and selects the correct message handler.
   if there is no match it generates a log message for the unhandled message "
  [from [type :as message]]
  ;(debuglogger search display message)
  (case type
    :derived-sentence-msg (derived-sentence-handler from message)
    :system-time-tick-msg (system-time-tick-handler)
    :register-task-creator-msg (register-task-creator-handler from)
    (debug aname (str "unhandled msg: " type))))

(defn initialise
  "Initialises actor:
      registers actor and sets actor state"
  [aname actor-ref]
  (reset! display '())
  (register! aname actor-ref))

(defn derived-load-reducer
  "creates gen-server for derived-load-reducer. This is used by the system supervisor"
  []
  (gen-server
    (reify Server
      (init [_] (initialise aname @self))
      (terminate [_ _])
      (handle-cast [_ from _ message] (msg-handler from message)))))