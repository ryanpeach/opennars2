(ns narjure.perception-action.derived-load-reducer
  (:require
    [co.paralleluniverse.pulsar
     [core :refer :all]
     [actors :refer :all]]
    [taoensso.timbre :refer [debug info]]
    [narjure.bag :as b]
    [narjure.control-utils :refer :all]
    [narjure.memory-management.local-inference.local-inference-utils :refer [get-task-id]]
    [narjure.debug-util :refer :all])
  (:refer-clojure :exclude [promise await]))

(def aname :derived-load-reducer)                           ; actor name
(def display (atom '()))                                    ; for lense output
(def search (atom ""))                                      ; for lense output filtering

(def max-derived-sentences 50)                              ; task bag capacity
(def max-selections 10)                                     ; max selections per cycle
(def derivation-bag (atom (b/default-bag max-derived-sentences)))      ; task bag

(defn system-time-tick-handler
  "select n sentences from input bag and post to :task-creator"
  []
  (doseq [n (range (min max-selections (b/count-elements @derivation-bag)))]
    (let [[element derivation-bag'] (b/get-by-index @derivation-bag (selection-fn @derivation-bag))
          msg [:derived-sentence-msg [(:task element)]]]
      (reset! derivation-bag derivation-bag')
      (cast! (whereis :task-creator) msg)
      ;(cast! (:task-creator @state) msg) ; see register-task-creator-handler for comments
      (debuglogger search display [:forward msg]))))

(defn derived-sentence-handler
  "adds sentence to input-bag and selects n senetences on system-time-tick"
  [from [msg sentence]]
  (let [elem {:id (get-task-id sentence) :priority (first (:budget sentence)) :task sentence}]
    (debuglogger search display [:add elem])
    (swap! derivation-bag b/add-element elem)))

; causes sequencing issues with actor instantiation currenlty.
; reverting to (whereis) in system-time-tick-handler
(defn register-task-creator-handler
  "add task-creator reference to state"
  [from]
  (set-state! {:task-creator from}))

(defn initialise
  "Initialises actor"
  [aname actor-ref]
  (reset! display '())
  (register! aname actor-ref)
  (reset! derivation-bag (b/default-bag max-derived-sentences)))

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