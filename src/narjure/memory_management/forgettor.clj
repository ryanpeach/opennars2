(ns narjure.memory-management.forgettor
  (:require
    [co.paralleluniverse.pulsar
     [core :refer :all]
     [actors :refer :all]]
    [narjure.global-atoms :refer [c-bag]]
    [narjure.memory-management.concept :as c]
    [narjure.bag :as b]
    [narjure.defaults :refer [c-priority]]
    [taoensso.timbre :refer [debug info]]
    [narjure.debug-util :refer :all])
  (:refer-clojure :exclude [promise await]))

(def aname :forgettor)                                      ; actor name
(def display (atom '()))                                    ; for lense output
(def search (atom ""))                                      ; for lense output filtering

(defn system-time-tick-handler
  "send message to each concept to forget"
  []
  (doseq [[id el] (:elements-map @c-bag)]
    (cast! (:ref el) [:concept-forget-msg []])))

(defn msg-handler
  "Identifies message type and selects the correct message handler.
   if there is no match it generates a log message for the unhandled message "
  [from [type :as message]]
  (debuglogger search display message)
  (case type
    :system-time-tick-msg (system-time-tick-handler)
    (debug aname (str "unhandled msg: " type))))

(defn initialise
  "Initialises actor: registers actor and sets actor state"
  [aname actor-ref]
  (reset! c/display '())                                    ;we also reset concept display here
  (reset! display '())                                      ;since concept actor startup is not
  (register! aname actor-ref)                               ;a place where it can be done
  (set-state! {}))

(defn forgettor
  "creates gen-server for forgettor. This is used by the system supervisor"
  []
  (gen-server
    (reify Server
      (init [_] (initialise aname @self))
      (terminate [_ _])
      (handle-cast [_ from _ message] (msg-handler from message)))))

