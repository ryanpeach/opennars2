(ns narjure.budget-functions
  (:require
    [nal
     [term_utils :refer [precondition-operation-consequent-statement]]]
    [nal.deriver
     [truth :refer [expectation t-or t-and w2c]]]
    [narjure
     [defaults :refer :all]
     [global-atoms :refer :all]
     [control-utils :refer [round2]]
     [debug-util :refer :all]]
    [narjure.memory-management.concept-utils :refer :all]))

(defn truth-to-quality [t]
  (let [exp (expectation t)
        positive-truth-bias 0.75]
    (max exp (* (- 1.0 exp) positive-truth-bias))))

(defn occurrence-penalty-tr [occ]
  (let [k 0.0001]
    (if (= occ :eternal)
      1.0
      (/ 1.0 (+ 1.0 (* k (Math/abs (- @nars-time occ))))))))

(defn highest-desire-in-respect-to-now [concept-term]
  (:truth (:strongest-desire-about-now ((:elements-map @c-bag) concept-term)))) ;also projected to now!!

(defn structural-reward-budget [budget derived-task]
  "returns a budget"
  (let [not-matched-or-not-desired-budget [(* (first budget) 0.5) (second budget) (nth budget 2)]
        match (second (precondition-operation-consequent-statement derived-task))]
    (if (and (:truth derived-task)
             match)
      (do
        ;(println "1")
        ;(println (str "1.1" match))
        ;(println (str "1.2" (match '?goal)))
        ;(println (str "1.3" (highest-desire-in-respect-to-now (match '?goal))))
        (let [goal (match '?goal)
              precondition (match '?precondition)
              goal-desire (highest-desire-in-respect-to-now goal)]
          ;(println (str "2: " goal))
          (if (= precondition goal)                         ;not a valid statement at all, I wonder why I didn't see this earlier.
            nil                                             ;TODO add invalid NAL statement filter for derivations anyway
            (if goal-desire
              (let [quality (max (nth budget 2)
                                 (t-or (expectation (:truth derived-task)) (t-or (second goal-desire) 0.65)))] ;TODO see goal-processor (unify)
                (do
                  (println "3")
                  (println (narsese-print (:statement derived-task)) " " (:truth derived-task) " " (:occurrence derived-task))
                  [(max (first budget) quality)
                   (second budget)
                   quality]))
              not-matched-or-not-desired-budget))))
      not-matched-or-not-desired-budget))                   ;tODO too radical
  )


(defn derived-budget
  [task derived-task]
  (when (< (:sc derived-task) max-term-complexity)
    (let [priority (* 0.8 (first (:budget task)))
         durability 0.5
         truth-quality (if (:truth derived-task) (truth-to-quality (:truth derived-task))
                                                 (w2c 1.0))
         complexity (:sc derived-task)
         quality (* truth-quality
                    (/ 1.0 (Math/sqrt complexity)))]
     (structural-reward-budget [priority durability quality] derived-task)
     ;[priority durability quality]
     )))

#_(defn derived-budget
    "
    "
    ;TRADITIONAL BUDGET INFERENCE (DERIVED TASK PART)
    [task derived-task bLink]

    (let [priority (first (:budget task))
          durability (* (second (:budget task))
                        (/ 1.0 (+ 1.0 (syntactic-complexity (:statement derived-task)))))
          priority' (if bLink (t-and priority (first bLink)) priority) ;t-or traditionally
          durability' (if bLink (t-and durability (second bLink)) durability)
          complexity (syntactic-complexity (:statement derived-task))
          budget [(round2 4 (* priority' (occurrence-penalty-tr (:occurrence derived-task))))
                  (round2 4 durability')
                  (round2 4 (if (:truth derived-task)
                              (/ (expectation (:truth derived-task))
                                 complexity)
                              (w2c 1.0)))
                  ]
          ]
      #_(let [result1 (structural-reward-budget budget derived-task)]
          (when result1
            (budget-consider-sequence-event task (budget-consider-temporality derived-task result1))))))