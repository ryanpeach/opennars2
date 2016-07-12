(ns narjure.memory-management.termlink-utils
  (:require
    [co.paralleluniverse.pulsar
     [core :refer :all]
     [actors :refer :all]]
    [taoensso.timbre :refer [debug info]]
    [narjure
     [global-atoms :refer :all]
     [bag :as b]
     [debug-util :refer :all]
     [control-utils :refer :all]
     [defaults :refer :all]
     [budget-functions :refer :all]]
    [narjure.memory-management
     [concept-utils :refer :all]]
    [clojure.core.unify :refer [unifier]]
    [nal.term_utils :refer [syntactic-complexity termlink-subterms]]
    [narjure.memory-management.local-inference
     [local-inference-utils :refer [get-task-id get-tasks]]
     [belief-processor :refer [process-belief]]
     [goal-processor :refer [process-goal]]
     [quest-processor :refer [process-quest]]
     [question-processor :refer [process-question]]]
    [nal.deriver
     [truth :refer [w2c t-or t-and confidence frequency expectation revision]]
     [projection-eternalization :refer [project-eternalize-to]]]
    [clojure.set :as set])
  (:refer-clojure :exclude [promise await]))

(defn get-linkable-terms
  "disallow linking to itself"
  [task]
  (filter #(not= % (:id @state)) (termlink-subterms (:statement task))))

(defn get-existing-terms-from-links
  "only use links whose target concept still exists"
  [links]
  (filter #(b/exists? @c-bag %) (keys links)))

(defn forget-termlinks
  "This one is for absolute forgetting of links: Remove termlinks whose concepts were forgot,
  remove worst termlinks if above max termlink count."
  []
  ;TODO test
  (doseq [[tl _] (:termlinks @state)]
    (when (not (b/exists? @c-bag tl))
      (set-state! (assoc @state :termlinks (dissoc (:termlinks @state) tl)))))
  (while (> (count (:termlinks @state)) concept-max-termlinks)
    (let [worst (apply min-key (comp expectation second) (:termlinks @state))]
      (set-state! (assoc @state :termlinks (dissoc (:termlinks @state) (first worst)))))))

(defn add-termlink
  "Adds a termlink with term tl and strength strength."
  [tl strength]
  (set-state! (assoc @state :termlinks (assoc (:termlinks @state)
                                         tl strength)))
  ;(forget-termlinks)
  )


(defn update-termlink [tl]                                  ;term
  (let [prio-me (:priority ((:elements-map @c-bag) (:id @state)))
        old-truth ((:termlinks @state) tl)
        prio-other (:priority ((:elements-map @c-bag) tl))
        association (t-and prio-me prio-other)
        disassocation (t-and prio-me (- 1.0 prio-other))
        frequency 1.0 #_(+ 0.5 (/ (- association 0.0 #_disassocation) 2.0))
        newstrength (revision [0.0 disassocation] (revision old-truth [frequency association]))] ;termlink-single-sample-evidence-amount
    (add-termlink tl newstrength)))

(defn use-stronger [t1 t2]
  (let [all-keys (set/union (map first t1) (map first t2))]
    (apply merge (for [k all-keys]
                 (let [str1 (when t1 (t1 k))
                       str2 (when t2 (t2 k))
                       st1 (if str1 str1 [0.0 0.0])
                       st2 (if str2 str2 [0.0 0.0])]
                   (if (> (first st1) (first st2))
                     {k st1}
                     {k st2}))))))

(defn refresh-termlinks [task]
  "Create new potential termlinks to other terms modulated by the concept priority they link to."
  ; :termlinks {term [budget]}
  (let [concept-prio (fn [z] (let [prio (concept-priority z)]
                               (if prio
                                 prio
                                 0.0)))
        newtermlinks (use-stronger (apply merge
                                   (for [tl (get-linkable-terms task)] ;prefer existing termlinks strengths
                                     {tl [1.0 0.01] #_[(* (first termlink-default-budget)
                                             (concept-prio tl)) (second termlink-default-budget)]}))
                            (:termlinks @state))
        valid-links (select-keys newtermlinks (get-existing-terms-from-links newtermlinks))];only these keys which exist in concept bag
    (set-state! (merge @state {:termlinks valid-links}))))

(defn link-feedback-handler
  "Link growth by contextual relevance of the inference and the result, as well as usefulness of the result."
  [from [_ [derived-task belief-concept-id]]]                       ;this one uses the usual priority durability semantics
  #_(try
    ;TRADITIONAL BUDGET INFERENCE (BLINK PART)
    (let [complexity (if (:truth derived-task) (syntactic-complexity belief-concept-id) 1.0)
          truth-quality (if (:truth derived-task)
                 (truth-to-quality (:truth derived-task))
                 (w2c 1.0))
          quality (/ truth-quality complexity)
          [result-concept _]  (b/get-by-id @c-bag (:statement derived-task))
          activation (:priority result-concept)
          [f c] ((:termlinks @state) belief-concept-id)
          strength (expectation [f c])]
      (when (and f c (:truth derived-task)) ;just revise by using the truth value directly
        ;as evidence for the usefulness
        (println (str [f c] (:truth derived-task)))
        (add-termlink belief-concept-id [(revision [f c] (:truth derived-task))]))
      )
    (catch Exception e () #_(println "fail"))))

(defn strengthen-termlink
  "Further strengthen a termlink"
  [link-strength]
  [(t-or (first link-strength) (first concept-selection-introduced-termlink-default-budget))
   (max (second link-strength) (second concept-selection-introduced-termlink-default-budget))])

(defn get-strengthened-termlink
  "Strenghten a termlink as used by the temporal linkage principle."
  [link-strength]
  (if link-strength
    (strengthen-termlink link-strength)
    concept-selection-introduced-termlink-default-budget))


(defn forget-termlink
  "Relative forgetting of a termlink"
  [term]
  (let [[p d] ((:termlinks @state) term)
        new-budget [(* p d) d]]
    (set-state! (assoc-in @state [:termlinks term] new-budget))))

(defn get-termlink-endpoints
  "Get the link endpoints, namely the concepts which the concept links to: their id as well as priority."
  []
  (let [initbag (b/default-bag concept-max-termlinks)]
    (try
      (reduce (fn [a b] (b/add-element a b)) initbag (for [[k v] (:termlinks @state)]
                                                      {:priority (+ (expectation v)
                                                                       (:priority (first (b/get-by-id @c-bag k))))
                                                       :id       k}))
      (catch Exception e (print "")
                         ;(println (str "error in get-termlink-endpoints: " e)
                         ))))

(defn select-termlink-ref
  "Select the termlink probabilistically, taking link strength and target priorities into account."
  []
  ;now search through termlinks, get the endpoint concepts, and form a bag of them
  (let [initbag (b/default-bag concept-max-termlinks)
        resbag (get-termlink-endpoints)]
    ;now select an element from this bag
    (if (and resbag (pos? (b/count-elements resbag)))
      (let [[beliefconcept _] (b/get-by-index resbag (selection-fn (b/count-elements resbag)))]
        (forget-termlink (:id beliefconcept))               ;apply forgetting for termlinks only on selection
        (get-ref-from-term (:id beliefconcept)))
      nil)))