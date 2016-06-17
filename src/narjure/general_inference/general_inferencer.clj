(ns narjure.general-inference.general-inferencer
  (:require
    [co.paralleluniverse.pulsar
     [core :refer :all]
     [actors :refer :all]]
    [nal.deriver :refer [inference]]
    [taoensso.timbre :refer [debug info]]
    [nal.deriver.truth :refer [t-or t-and w2c]]
    [narjure.global-atoms :refer :all]
    [narjure.debug-util :refer :all]
    [narjure.defaults :refer [priority-threshold]]
    [narjure.control-utils :refer [make-evidence non-overlapping-evidence? round2]]
    [nal.term_utils :refer [syntactic-complexity]]
    [nal.deriver.truth :refer [expectation]])
  (:refer-clojure :exclude [promise await]))

;(def aname :general-inferencer)
(def display (atom '()))
(def search (atom ""))

(defn occurrence-penalty-tr [occ]
  (let [k 0.0001]
    (if (= occ :eternal)
      1.0
      (/ 1.0 (+ 1.0 (* k (Math/abs (- @nars-time occ))))))))

(defn do-inference-handler
  "Processes :do-inference-msg:
    generated derived results, budget and occurrence time for derived tasks.
    Posts derived sentences to task creator"
  [from [msg [task-concept-id belief-concept-id bLink task belief debug]]]
  (set-state! (update @state :all-inference-requests inc))  ;for stats tracking
  (try
    (when debug
      (println (str "inference between: " task belief)
               (inference task belief)))
    (when (non-overlapping-evidence? (:evidence task) (:evidence belief))
      (println "no overlap")
      (set-state! (update @state :non-overlapping-inference-requests inc))      ;for stats tracking
      (let [pre-filtered-derivations (inference task belief)]
        (let [filtered-derivations (filter #(not= (:statement %) (:parent-statement task)) pre-filtered-derivations)
             evidence (make-evidence (:evidence task) (:evidence belief))
             derivation-depth (if (not (:depth task)) 1 (:depth task))
             derived-load-reducer (whereis :derived-load-reducer)]
          (set-state! (update @state :pre-filtered-derivations + (count pre-filtered-derivations)))      ;for stats tracking
         ; dont post if evidence is nil, saves multiple checks further down the pipe
         (when (not= evidence '())
           (doseq [derived filtered-derivations]
             (let
               ;TRADITIONAL BUDGET INFERENCE (DERIVED TASK PART)
               [priority (first (:budget task))
                durability (* (second (:budget task))
                                 (/ 1.0 (+ 1.0 (syntactic-complexity (:statement derived)))))

                priority' (if bLink (t-or priority (first bLink)) priority)
                durability' (if bLink (t-and durability (second bLink)) durability)

                complexity (syntactic-complexity (:statement derived))

                budget [(* priority' (occurrence-penalty-tr (:occurrence derived)))

                        durability'

                        (if (:truth derived)
                          (/ (expectation (:truth derived))
                             complexity)
                          (w2c 1.0))
                        ]
                ]
               (when (and (> (first budget) priority-threshold)
                          (or (not (:truth derived))
                              true #_(> (expectation (:truth derived)) 0.5))) ;buffer filter of older versions
                 (set-state! (update @state :filtered-derivations inc))              ;for stats tracking
                 (cast! derived-load-reducer [:derived-sentence-msg [task-concept-id
                                                                     belief-concept-id
                                                                     (assoc derived :budget [(round2 4 (first budget)) (round2 4 (second budget)) (round2 4 (nth budget 2))]
                                                                                    :parent-statement (:statement task) :depth (inc derivation-depth)
                                                                                    :evidence evidence)]]))))))))
    (catch Exception e (debuglogger search display (str "inference error " (.toString e))))))

(defn begin-count-handler [_ _]
  (when (pos? (:all-inference-requests @state))
    (println (str
               "[GI(" (:name @state) ")] all-inference-requests: " (:all-inference-requests @state)
               ", non-overlapping-inference-requests: " (:non-overlapping-inference-requests @state)
               ", pre-filtered-derivations: " (:pre-filtered-derivations @state)
               ", filtered-derivations: " (:filtered-derivations @state))))
  (set-state! (assoc @state
                :all-inference-requests 0
                :non-overlapping-inference-requests 0
                :pre-filtered-derivations 0
                :filtered-derivations 0)))

(defn initialise
  "Initialises actor:
      registers actor and sets actor state"
  [aname actor-ref]
  (reset! display '())
  (register! aname actor-ref)
  (set-state! {:name aname :all-inference-requests 0 :non-overlapping-inference-requests 0 :pre-filtered-derivations 0 :filtered-derivations 0}))

(defn msg-handler
  "Identifies message type and selects the correct message handler.
   if there is no match it generates a log message for the unhandled message "
  [from [type :as message]]
  (when-not (= type :begin-count-msg) (debuglogger search display message))
  (case type
    :do-inference-msg (do-inference-handler from message)
    :begin-count-msg (begin-count-handler from message)
    (debug :ge (str "unhandled msg: " type))))

(defn general-inferencer [aname]
  (gen-server
    (reify Server
      (init [_] (initialise aname @self))
      (terminate [_ cause] #_(info (str aname " terminated.")))
      (handle-cast [_ from id message] (msg-handler from message)))))
