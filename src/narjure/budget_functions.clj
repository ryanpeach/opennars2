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

(defn truth-to-quality
  "The task quality judged by its truth."
  [t]
  (let [exp (expectation t)
        positive-truth-bias 0.75]
    (max exp (* (- 1.0 exp) positive-truth-bias))))

(defn occurrence-penalty-tr
  "Occurrence budget penalty. Currently not used as forgetting seems to suffice."
  [occ]
  (let [k 0.0001]
    (if (= occ :eternal)
      1.0
      (/ 1.0 (+ 1.0 (* k (Math/abs (- @nars-time occ))))))))

(defn highest-desire-in-respect-to-now
  "The highest desire value in respect to current nars-time."
  [concept-term]
  (:truth (:strongest-desire-about-now ((:elements-map @c-bag) concept-term)))) ;also projected to now!!

(defn structural-reward-budget
  "returns a increased budget for statements of structural interest, mainly for experiments."
  [budget derived-task]
  (let [not-matched-or-not-desired-budget [(* (first budget) 0.8) (second budget) (nth budget 2)]
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
          (if (= precondition goal)
            nil
            (if goal-desire
              (let [quality (max (nth budget 2)
                                 (t-or (expectation (:truth derived-task)) (t-or (second goal-desire) 0.8)))] ;TODO see goal-processor (unify)
                (do
                  (println "INCREASED DERIVED BUDGET")
                  (println (narsese-print (:statement derived-task)) " " (:truth derived-task) " " (:occurrence derived-task))
                  [(max (first budget) quality)
                   (max (second budget) 0.9)
                   (nth budget 2)]))
              not-matched-or-not-desired-budget))))
      not-matched-or-not-desired-budget))                   ;tODO too radical
  )


(defn derived-budget
  "The budget of a by general inference derived task."
  [task derived-task]
  (when (< (:sc derived-task) max-term-complexity)
    (let [activation-gain 0.95
          priority (max 1.0 (t-or activation-gain (first (:budget task))))
         durability (/ (second (:budget task)) (Math/sqrt (:sc derived-task)))
         truth-quality (if (:truth derived-task) (truth-to-quality (:truth derived-task))
                                                 0.0 #_(w2c 1.0))
         complexity (:sc derived-task)
         rescale-factor 0.4 ;should probably not above input belief quality!
         quality (* truth-quality
                    rescale-factor
                    #_(/ 1.0 (Math/sqrt complexity)))]
     (structural-reward-budget [priority durability quality] derived-task))))