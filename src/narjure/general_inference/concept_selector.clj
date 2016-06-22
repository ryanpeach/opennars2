(ns narjure.general-inference.concept-selector
  (:require
    [co.paralleluniverse.pulsar
     [core :refer :all]
     [actors :refer :all]]    [narjure.global-atoms :refer [c-bag lense-taskbags lense-termlinks]]
    [narjure.bag :as b]
    [narjure.defaults :refer [max-concept-selections]]
    [clojure.math.numeric-tower :as math]
    [taoensso.timbre :refer [debug info]]
    [narjure.debug-util :refer :all]
    [narjure.control-utils :refer :all])
  (:refer-clojure :exclude [promise await]))

(def aname :concept-selector)
(def display (atom '()))
(def search (atom ""))

(defn create-temporal-link
  " creates a term-link between last-selected concept and the currently selected concept"
  [state selected]
  (when-let [last-selected (:last-selected state)]
    (when-not (= (:ref last-selected) (:ref selected))
      (cast! (:ref selected) [:termlink-create-msg [(:id last-selected)]])
      (cast! (:ref last-selected) [:termlink-create-msg [(:id selected)]])))
  (set-state! (assoc state :last-selected selected)))

(defn inference-tick-handler
  "Select n concepts for inference and post
   inference-request-message to each selected
   concept"
  [from [msg]]
  (doseq [[k v] @lense-taskbags]                            ;is empty if not in debug so can stay here for now since we
    (when-not (b/exists? @c-bag k)                          ;don't want mem to get full just because lense isn't running
      (swap! lense-taskbags (fn [old] (dissoc old k))) ;element doesnt exist anymore
      (swap! lense-termlinks (fn [old] (dissoc old k)))))
  ; (dotimes [n (min (b/count-elements @c-bag) 1)]
  ;one concept for inference is enough for now ^^

  (doseq [selected (select-concepts max-concept-selections @c-bag)]
    #_(create-temporal-link @state selected)
    (when (sufficient-priority? selected)
      (cast! (:ref selected) [:inference-request-msg (:id selected)])
      (debuglogger search display (str "Concept selected: " [:task selected :priority (:priority selected)])))))

(defn initialise
  "Initialises actor:
      registers actor and sets actor state"
  [aname actor-ref]
  (reset! display '())
  (register! aname actor-ref)
  (set-state! {}))

(defn msg-handler
  "Identifies message type and selects the correct message handler.
   if there is no match it generates a log message for the unhandled message "
  [from [type :as message]]
  ;(debuglogger display message) since tick is uninteresting we use what is selected
  (case type
    :inference-tick-msg (inference-tick-handler from message)
    (debug aname (str "unhandled msg: " type))))

(defn concept-selector []
  (gen-server
    (reify Server
      (init [_] (initialise aname @self))
      (terminate [_ cause] #_(info (str aname " terminated.")))
      (handle-cast [_ from id message] (msg-handler from message)))))
