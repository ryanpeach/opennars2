
(ns narjure.budget-functions
  (:require
    [nal.deriver.truth :refer [t-or t-and w2c]]
    [narjure.global-atoms :refer :all]
    [narjure.control-utils :refer [round2]]
    [narjure.debug-util :refer :all]
    [nal.term_utils :refer [syntactic-complexity precondition-operation-consequent-statement]]
    [nal.deriver.truth :refer [expectation]]))

(defn occurrence-penalty-tr [occ]
  (let [k 0.0001]
    (if (= occ :eternal)
      1.0
      (/ 1.0 (+ 1.0 (* k (Math/abs (- @nars-time occ))))))))


(defn structural-reward-budget [budget derived-task]
  "returns a budget"
  (let [match (precondition-operation-consequent-statement derived-task)
        quality (max (nth budget 2) 0.9)]
    (if match
      (do
        #_(println (narsese-print (:statement derived-task)) " " (:truth derived-task) " " (:occurrence derived-task))
        [(max (first budget) quality)
        (second budget)
        quality])
      [(* (first budget) 0.75) (second budget) (nth budget 2)])) ;tODO too radical

  )

(defn derived-budget
  "
  "
  ;TRADITIONAL BUDGET INFERENCE (DERIVED TASK PART)
  [task derived-task bLink]

  (let    [priority (first (:budget task))
           durability (* (second (:budget task))
                         (/ 1.0 (+ 1.0 (syntactic-complexity (:statement derived-task)))))
           priority' (if bLink (t-and priority (first bLink)) priority) ;t-or traditionally
           durability' (if bLink (t-and durability (second bLink)) durability)
           complexity (syntactic-complexity (:statement derived-task))
           budget [(round2 4 (* priority' (occurrence-penalty-tr (:occurrence derived-task))))
                   (round2 4 durability')
                   (round2 4(if (:truth derived-task)
                              (/ (expectation (:truth derived-task))
                                 complexity)
                              (w2c 1.0)))
                   ]
           ]
    (structural-reward-budget budget derived-task)))