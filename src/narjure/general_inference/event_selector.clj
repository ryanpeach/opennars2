(ns narjure.general-inference.event-selector
  (:require
    [co.paralleluniverse.pulsar
     [core :refer :all]
     [actors :refer :all]]
    [narjure.global-atoms :refer :all]
    [narjure.bag :as b]
    [clojure.math.numeric-tower :as math]
    [taoensso.timbre :refer [debug info]]
    [narjure.debug-util :refer :all]
    [narjure.control-utils :refer :all])
  (:refer-clojure :exclude [promise await]))

(def aname :event-selector)                                 ; actor name
(def max-event-selections 10)                               ; number of events to select per cycle
(def display (atom '()))                                    ; for lense output
(def search (atom ""))                                      ; for lense output filtering

(defn get-events
  "recursively selects n events (or bag capaity if less)
   and returns event collection and updated bag. An even
   number of events is returned to allow pairs to be sent
   to general inferencer"
  [n events bag]
  (if (or (= n 0) (= 0 (b/count-elements bag)))
    (if (odd? (count events))
      [(pop events) bag]
      [events bag])
    (let [[event bag'] (b/get-by-index bag (selection-fn bag))
          events' (conj events event)]
      (get-events (dec n) events' bag'))))

(defn forget-events
  "recursively 'forgets' the events after use and returns
   them to the event bag. The updated bag is returned"
  [events bag]
  (if (empty? events)
    bag
    (let [event (peek events)
          events' (pop events)
          bag' (b/add-element bag (forget-element event))]
      (forget-events events' bag'))))

(defn cast-display-events
  "takes a vector of two events and outputs them to lense
  then posts them to :general-inferencer"
  [[a b]]
  (debuglogger search display ["selected events:" a "§" b "§§"])
  (cast! (:general-inferencer @state) [:do-inference-msg [(:task a) (:task b)]]))

(defn inference-tick-handler
  "Select n pairs of events events from event buffer for inference
   and post do-inference-msg to general inferencer"
  [_ [_]]
  (try
    (when (> (b/count-elements @e-bag) 1)
      (let [[events bag] (get-events max-event-selections [] @e-bag)]
        (reset! e-bag (forget-events events bag))
        (doseq [event-pair (partition 2 events)]
          (cast-display-events (vec event-pair)))))
    (catch Exception e (debuglogger search display (str "event select error " (.toString e))))))

(defn initialise
  "Initialises actor:
      registers actor and sets actor state"
  [aname actor-ref]
  (reset! display '())
  (register! aname actor-ref)
  (set-state! {:general-inferencer (whereis :general-inferencer)}))

(defn msg-handler
  "Identifies message type and selects the correct message handler.
   if there is no match it generates a log message for the unhandled message "
  [from [type :as message]]
  ;(debuglogger display message) same as in concept_selector
  (case type
    :inference-tick-msg (inference-tick-handler from message)
    (debug aname (str "unhandled msg: " type))))

(defn event-selector []
  (gen-server
    (reify Server
      (init [_] (initialise aname @self))
      (terminate [_ cause] #_(info (str aname " terminated.")))
      (handle-cast [_ from id message] (msg-handler from message)))))