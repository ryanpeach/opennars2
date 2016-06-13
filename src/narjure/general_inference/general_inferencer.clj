(ns narjure.general-inference.general-inferencer
  (:require
    [co.paralleluniverse.pulsar
     [core :refer :all]
     [actors :refer :all]]
    [nal.deriver :refer [inference]]
    [taoensso.timbre :refer [debug info]]
    [narjure.global-atoms :refer :all]
    [narjure.debug-util :refer :all]
    [narjure.defaults :refer [priority-threshold]]
    [narjure.control-utils :refer [make-evidence round2]]
    [nal.term_utils :refer [syntactic-complexity]]
    [nal.deriver.truth :refer [expectation]])
  (:refer-clojure :exclude [promise await]))

(def aname :general-inferencer)
(def display (atom '()))
(def search (atom ""))

(defn non-overlapping-evidence? [e1 e2]
  (empty? (clojure.set/intersection (set e1) (set e2))))

(def max-evidence 10)

(defn occurrence-penalty-tr [occ]
  (let [k 0.01]
    (if (= occ :eternal)
      1.0
      (/ 1.0 (+ 1.0 (* k (Math/abs (- @nars-time occ))))))))

(defn do-inference-handler
  "Processes :do-inference-msg:
    generated derived results, budget and occurrence time for derived tasks.
    Posts derived sentences to task creator"
  [from [msg [task belief]]]
  (try
    (when (non-overlapping-evidence? (:evidence task) (:evidence belief))
      (let [derivations (filter #(not= (:statement %) (:parent-statement task)) (inference task belief))
            evidence (make-evidence (:evidence task) (:evidence belief))
            derived-load-reducer (whereis :derived-load-reducer)
            budget [(* (first (:budget task))               ;im not sure anymore whether task parent priority is good here
                       (if (= nil (:truth task)) ;needs discussing.
                         0.5
                         (expectation (:truth task)))
                       (occurrence-penalty-tr (:occurrence task)))
                    (/ 1.0 (+ 1.0 (syntactic-complexity (:statement task)))) 0.0]]
        ; dont post if evidence is nil, saves multiple checks further down the pipe
        (when (and (not= evidence '()) (> (first budget) priority-threshold))
          (doseq [derived derivations]
            (cast! derived-load-reducer [:derived-sentence-msg (assoc derived :budget [(round2 4 (first budget)) (round2 4 (second budget)) 0.0]
                                                                              :parent-statement (:statement task)
                                                                              :evidence evidence)])))))
    (catch Exception e (debuglogger search display (str "inference error " (.toString e))))))

(defn initialise
  "Initialises actor:
      registers actor and sets actor state"
  [aname actor-ref]
  (reset! display '())
  (register! aname actor-ref)
  (set-state! {:state 0}))

(defn msg-handler
  "Identifies message type and selects the correct message handler.
   if there is no match it generates a log message for the unhandled message "
  [from [type :as message]]
  (debuglogger search display message)
  (case type
    :do-inference-msg (do-inference-handler from message)
    (debug aname (str "unhandled msg: " type))))

(defn general-inferencer []
  (gen-server
    (reify Server
      (init [_] (initialise aname @self))
      (terminate [_ cause] #_(info (str aname " terminated.")))
      (handle-cast [_ from id message] (msg-handler from message)))))
