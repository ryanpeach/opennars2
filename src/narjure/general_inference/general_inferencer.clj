(ns narjure.general-inference.general-inferencer
  (:require
    [co.paralleluniverse.pulsar
     [core :refer :all]
     [actors :refer :all]]
    [nal.deriver :refer [inference]]
    [taoensso.timbre :refer [debug info]]
    [narjure.debug-util :refer :all]
    [narjure.budget-functions :refer [derived-budget]]
    [narjure.defaults :refer [priority-threshold]]
    [narjure.control-utils :refer [make-evidence non-overlapping-evidence? round2]])
  (:refer-clojure :exclude [promise await]))

(def display (atom '()))
(def search (atom ""))

(defn do-inference-handler
  "Processes :do-inference-msg:
    generates derived results, budget and occurrence time for derived tasks.
    Posts derived sentences to task creator"
  [from [msg [task-concept-id belief-concept-id bLink task belief]]]
  (try
    (when (non-overlapping-evidence? (:evidence task) (:evidence belief))
      (let [pre-filtered-derivations (inference task belief)
            filtered-derivations (filter #(not= (:statement %) (:parent-statement task)) pre-filtered-derivations)
            evidence (make-evidence (:evidence task) (:evidence belief))
            derivation-depth (if (not (:depth task)) 1 (:depth task))
            derived-load-reducer (whereis :derived-load-reducer)]
        (when-not (empty? evidence)
          (doseq [derived filtered-derivations]
            (let [budget (derived-budget task derived bLink)
                  derived-task (assoc derived :budget budget
                                              :parent-statement (:statement task) :depth (inc derivation-depth)
                                              :evidence evidence)]
              (when (and budget (> (first budget) priority-threshold))
                (cast! derived-load-reducer [:derived-sentence-msg [task-concept-id
                                                                    belief-concept-id
                                                                    derived-task]])))))))
    (catch Exception e (debuglogger search display (str "inference error " (.toString e))))))

(defn initialise
  "Initialises actor:
      registers actor and sets actor state"
  [aname actor-ref]
  (reset! display '())
  (register! aname actor-ref))

(defn msg-handler
  "Identifies message type and selects the correct message handler.
   if there is no match it generates a log message for the unhandled message "
  [from [type :as message]]
  (debuglogger search display message)
  (case type
    :do-inference-msg (do-inference-handler from message)
    (debug :ge (str "unhandled msg: " type))))

(defn general-inferencer [aname]
  (gen-server
    (reify Server
      (init [_] (initialise aname @self))
      (terminate [_ cause] #_(info (str aname " terminated.")))
      (handle-cast [_ from id message] (msg-handler from message)))))
