(ns narjure.memory-management.concept-manager
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

(def aname :concept-manager)                                   ; actor name
(def display (atom '()))                                    ; for lense output
(def search (atom ""))                                      ; for lense output filtering

(defn make-general-concept
  "Create a concept, for the supplied term, and add to
   the concept bag"
  [term]
  (let [concept-ref (spawn (c/concept term))]
    (swap! c-bag b/add-element {:id term :priority c-priority :quality 0.0 :ref concept-ref})
    ;(info (str "concept count: " (b/count-elements @c-bag)))
    ))

(defn create-concept-handler
  "Create a concept for each term in statement, if they dont
   exist. Then post the task back to task-dispatcher."
  [from [_ {:keys [statement]  :as task}]]
  (doseq [term (:terms task)]
    (when-not (b/exists? @c-bag term)
      (make-general-concept term)))
  (cast! from [:task-msg task]))

(defn persist-state-handler
  ""
  [from message]
  ;todo
  (info (str "in persist-state-handler"))
  )

(defn load-state-handler
  ""
  [from message]
  ;todo
  (info (str "in load-state-handler"))
  )

(defn initialise
  "Initialises actor: registers actor and sets actor state"
  [aname actor-ref]
  (reset! c/display '())                                    ;we also reset concept display here
  (reset! display '())                                      ;since concept actor startup is not
  (register! aname actor-ref)                               ;a place where it can be done
  (set-state! {}))

(defn clean-up
  "Shutdown all concept actors"
  []
  (doseq [[_ {actor-ref :ref}] (:elements-map @c-bag)]
    (shutdown! actor-ref)))

(defn msg-handler
  "Identifies message type and selects the correct message handler.
   if there is no match it generates a log message for the unhandled message "
  [from [type :as message]]
  (debuglogger search display message)
  (case type
    :create-concept-msg (create-concept-handler from message)
    :persist-state-msg (persist-state-handler from message)
    :load-state-msg (load-state-handler from message)
    (debug aname (str "unhandled msg: " type))))

(defn initialise
  "Initialises actor: registers actor and sets actor state"
  [aname actor-ref]
  (reset! c/display '())                                    ;we also reset concept display here
  (reset! display '())                                      ;since concept actor startup is not
  (register! aname actor-ref)                               ;a place where it can be done
  (set-state! {}))

(defn concept-manager
  "creates gen-server for concept-manager. This is used by the system supervisor"
  []
  (gen-server
    (reify Server
      (init [_] (initialise aname @self))
      (terminate [_ _] (clean-up))
      (handle-cast [_ from _ message] (msg-handler from message)))))

