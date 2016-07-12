(ns nal.deriver.preconditions
  (:require [nal.deriver.set-functions
             :refer [f-map not-empty-diff? not-empty-inter?]]
            [nal.deriver.utils :refer [walk postwalk-depth1]]
            [nal.deriver.substitution :refer [substitute munification-map]]
            [nal.deriver.terms-permutation :refer [implications equivalences]]
            [clojure.set :refer [union intersection]]
            [narjure.defaults :refer [temporal-window-duration]]
            [clojure.core.match :as m]
            [nal.deriver.normalization :refer [reduce-seq-conj]]))

(defn abs [^long n] (Math/abs n))

;TODO preconditions
;:shift-occurrence-forward :shift-occurrence-backward
(defmulti compound-precondition
  "Expands compound precondition to clojure sequence
  that will be evaluted later"
  first)

(defmethod compound-precondition :default [_] [])

(defmethod compound-precondition :!=
  [[_ & args]]
  [`(not= ~@args)])

(defn check-set
  "Whether it is a extensional or intensional set"
  [set-type arg]
  `(and (coll? ~arg) (= ~set-type (first ~arg))))

(defmethod compound-precondition :set-ext? [[_ arg]]
  [(check-set 'ext-set arg)])

(defmethod compound-precondition :set-int? [[_ arg]]
  [(check-set 'int-set arg)])

(def sets '#{ext-set int-set})

(defn set-conditions
  "Set condition handling"
  [set1 set2]
  [`(coll? ~set1)
   `(coll? ~set2)
   `(let [k# 1 afop# (first ~set1)]
      (and (sets afop#) (= afop# (first ~set2))))])

(defmethod compound-precondition :difference [[_ arg1 arg2]]
  "Set difference precondition predicate, it is only true if the last arg is the difference of the input arguments"
  (concat (set-conditions arg1 arg2)
          [`(not-empty-diff? ~arg1 ~arg2)]))

(defmethod compound-precondition :union [[_ arg1 arg2]]
  "Set union precondition predicate, it is only true if the last arg is the union of the input arguments"
  (set-conditions arg1 arg2))

(defmethod compound-precondition :intersection [[_ arg1 arg2]]
  "Set intersection precondition predicate, it is only true if the last arg is the intersection of the input arguments"
  (concat (set-conditions arg1 arg2)
          [`(not-empty-inter? ~arg1 ~arg2)]))

(defmethod compound-precondition :substitute-if-unifies
  [[_ arg1 arg2 arg3]]
  "This predicate substitutes the for the unification of both arguments
   needed variable unification also in the conclusion."
  [`(munification-map ~arg1 ~arg2 ~arg3)])

(defmethod compound-precondition :contains?
  [[_ arg1 arg2]]
  "Whether the list arg1 contains the term arg2"
  [`(some (set [~arg2]) ~arg1)])

(def implications-and-equivalences
  (union implications equivalences))

(defmethod compound-precondition :not-implication-or-equivalence
  [[_ arg]]
  "This precondition predicate only returns true if the arg term is neither a type of implication
  nor a type of equivalence"
  [`(if (coll? ~arg)
      (nil? (~`implications-and-equivalences (first ~arg)))
      true)])

(defn get-terms
  [st]
  "Get all the statement sub-terms"
  (if (coll? st)
    (mapcat get-terms (rest st))
    [st]))

(defmethod compound-precondition :no-common-subterm
  [[_ arg1 arg2]]
  "Only true if no common subterm between both terms is shared"
  [`(empty? (intersection (set (get-terms ~arg1))
                          (set (get-terms ~arg2))))])

(defmethod compound-precondition :not-set?
  [[_ arg]]
  "Only true if the term in question is not a set term."
  [`(or (not (coll? ~arg)) (not (sets (first ~arg))))])

(defmethod compound-precondition :measure-time
  [_]
  "Measures the time between both premises, where task has to have the newer occurrence time and both arguments are non-eternal"
  [`(not= :eternal :t-occurrence)
   `(not= :eternal :b-occurrence)
   `(< :b-occurrence :t-occurrence)
   #_`(<= ~temporal-window-duration (abs (- :t-occurrence :b-occurrence)))])

(defmethod compound-precondition :measure-time-backward
  [_]
  "Measures the time between both premises, but this time belief is the newer task in terms of occurrence time."
  [`(not= :eternal :t-occurrence)
   `(not= :eternal :b-occurrence)
   `(> :b-occurrence :t-occurrence)
   #_`(<= ~temporal-window-duration (abs (- :t-occurrence :b-occurrence)))])

(defmethod compound-precondition :concurrent
  [_]
  "Only true when both premises happen below temporal-window-duration away from each other."
  [`(not= :eternal :t-occurrence)
   `(not= :eternal :b-occurrence)
   `(> ~temporal-window-duration (abs (- :t-occurrence :b-occurrence)))])

;-------------------------------------------------------------------------------
(defmulti precondition-transformation (fn [arg1 _] (first arg1)))

(defmethod precondition-transformation :default [_ conclusion] conclusion)

(defn sets-transformation
  [[cond-name el1 el2 el3] conclusion]
  (walk conclusion (= :el el3)
    `(~(f-map cond-name) ~el1 ~el2)))

(doall (map
         #(defmethod precondition-transformation %
           [cond concl] (sets-transformation cond concl))
         [:difference :union :intersection]))

(defmethod precondition-transformation :substitute
  [[_ el1 el2] conclusion]
  "This precondition predicate is only true if the conclusion has all occurrences of el1 replaced with el2"
  `(walk ~conclusion
     (= :el ~el1) ~el2))

(defmethod precondition-transformation :substitute-from-list
  [[_ el1 el2] conclusion]
  "Allows to substitute a certain element in the term list with another value, needed for elegant NAL4 rule specification"
  `(mapv (fn [k#]
           (if (= k# ~el1)
             k#
             (walk k# (= :el ~el1) ~el2 :single-level-hack)))
         ~conclusion))

(defmethod precondition-transformation :substitute-if-unifies
  [[_ p1 p2 p3] conclusion]
  `(substitute ~p1 ~p2 ~p3 ~conclusion))

(defmethod precondition-transformation :measure-time
  [[_ arg] conclusion]
  (let [mt (gensym)]
    (walk `(let [~arg (abs (- :t-occurrence :b-occurrence))]
             ~(walk conclusion
                (= :el arg) [:interval arg]))
      (= :el arg) mt)))

(defmethod precondition-transformation :measure-time-backward
  [[_ arg] conclusion]
  (let [mt (gensym)]
    (walk `(let [~arg (abs (- :t-occurrence :b-occurrence))]
             ~(walk conclusion
                    (= :el arg) [:interval arg]))
          (= :el arg) mt)))

(defn check-precondition
  [conclusion precondition]
  (if (seq? precondition)
    (precondition-transformation precondition conclusion)
    conclusion))

(defn preconditions-transformations
  "Some transformations of conclusion may be required by precondition."
  [conclusion preconditions]
  (reduce check-precondition conclusion preconditions))

;-------------------------------------------------------------------------------
(defmulti conclusion-transformation (fn [arg1 _] (first arg1)))

(defn shift-forward-let
  ([sym conclusion] (shift-forward-let sym nil conclusion nil))
  ([sym op conclusion temporal-window-duration]
   `(let [interval# ~sym
          ~@(if temporal-window-duration
              `[:t-occurrence (~op :t-occurrence ~temporal-window-duration)]
              [])
          :t-occurrence (if interval# (+ :t-occurrence interval#)
                                      :t-occurrence)]
      ~conclusion)))

(defmethod conclusion-transformation :shift-occurrence-forward
  [args concl]
  (m/match (mapv #(if (and (coll? %) (= 'quote (first %)))
                   (second %) %) (rest args))
    [(:or '=|> '==>)] concl
    ['pred-impl] `(let [:t-occurrence (+ :t-occurrence ~temporal-window-duration)] ~concl)
    ['retro-impl] `(let [:t-occurrence (- :t-occurrence ~temporal-window-duration)] ~concl)
    [sym (:or '=|> '==>)] (shift-forward-let sym concl)
    [sym 'pred-impl] (shift-forward-let sym `+ concl temporal-window-duration)
    [sym 'retro-impl] (shift-forward-let sym `- concl temporal-window-duration)))

(defn backward-interval-check [sym]
  `(and (coll? ~sym) (= (first ~sym) (quote ~'seq-conj))
        (let [cnt# (count ~sym)]
          (and (odd? cnt#) (get-in ~sym [(dec cnt#) 1])))))

(defn shift-backward-let
  ([sym conclusion] (shift-backward-let sym nil conclusion nil))
  ([sym op conclusion temporal-window-duration]
   `(let [interval# ~(backward-interval-check sym)
          ~@(if temporal-window-duration
              `[:t-occurrence (~op :t-occurrence ~temporal-window-duration)]
              [])
          :t-occurrence (if interval# (+ :t-occurrence interval#)
                                      :t-occurrence)]
      (if interval#
        (update ~conclusion :statement reduce-seq-conj)
        ~conclusion))))

(defmethod conclusion-transformation :shift-occurrence-backward
  [args concl]
  "Shifts the occurrence time of the conclusion by the specified time offset."
  (let [temporal-window-duration (- temporal-window-duration)]
    (m/match (mapv #(if (and (coll? %) (= 'quote (first %)))
                     (second %) %) (rest args))
      [(:or '=|> '==>)] concl
      ['pred-impl] `(let [:t-occurrence (+ :t-occurrence ~temporal-window-duration)] ~concl)
      ['retro-impl] `(let [:t-occurrence (- :t-occurrence ~temporal-window-duration)] ~concl)
      [sym (:or '=|> '==>)] (shift-backward-let sym concl)
      [sym 'pred-impl] (shift-backward-let sym `+ concl temporal-window-duration)
      [sym 'retro-impl] (shift-backward-let sym `- concl temporal-window-duration))))
