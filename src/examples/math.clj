(ns examples.math
  (:require [narjure.global-atoms :refer :all]
            [narjure.core :as nar]
            [narjure.narsese :refer [parse2]]
            [narjure.sensorimotor :refer :all]
            [narjure.debug-util :refer :all]
            [narjure.string :as n]))

; --- Support functions ---
(defn apply_args
    "Like apply but assumes the last item is a list,
     the first item is the operation, and the rest are constant.
     Ex: (apply * (into [1 2] args)) --> (apply_args * 1 2 args)"
    [op & args]
    (apply op (into (butlast args) (last args))))

(defn map-hash-list [op x]
  "Applies op to every item in the map of lists x.
   op is a function taking only the value as a parameter."
  (into {} (map (fn [[k v]] [k (map op v)]) (seq x))))
(defn map-hash-list2 [op x]
  "Applies op to every item in the map of lists x.
   op is a function taking the key and value as a parameter."
  (into {} (map (fn [[k v]] [k (map #(op k %) v)]) (seq x))))

(defn template
    "The template to register an op with NARS"
    [op]
    (fn [args operationgoal]
      (apply_args op args)))
      ;(catch Exception e [false]))))

; TODO pass values natively as objects
(defn process-map1 [map_name m]
  (for [[k v] (seq m)] (n/belief (n/<-> (n/* map_name k) v))))
(defn process-map2 [map_name m]
  (for [[k v] (seq m)] (n/belief (n/--> (n/* map_name v) k))))
(defn rmap1 [k v]
  (println k v)
  (flatten
  (if-not (map? v)
    [(n/belief (n/<-> k (n/quotes v)))]
    (when-not (empty? (seq v))
      (map (fn [[a b]] (rmap1 (n/* k (n/quotes a)) b)) (seq v))))))
(defn rmap2 [parent k v]
  (println k v)
  (flatten
  (if-not (map? v)
    [(n/belief (n/--> (n/* parent (n/quotes v)) k))]
    (when-not (empty? (seq v))
      (map (fn [[a b]] (rmap2 (n/* parent k) (n/quotes a) b)) (seq v))))))

; List the ops we desire by their library
;(defmacro qt_lst [& x] (let [out (map #(list 'quote %) x)] (into [] out)))
(defmacro meta_fun [fun] (list 'meta (list 'var fun)))
(defmacro var_lst [& x] (let [out (map #(list 'var %) x)] (into [] out)))
 ; for example


; register all operations
(defn requirements [fvar]
  (rmap1 (n/quotes fvar) (meta fvar)))
(defn register [fvar]
  (nars-register-operation
      (symbol (str fvar))
      (template fvar))
  (for [x (requirements fvar)] (do
      (println (str "Sending " x))
      (nars-input-narsese x))))

(def ops (var_lst + - * / quot rem float double int))
(defn -main [& args]
  (nars-register-answer-handler (fn [task solution]
                                  (let [msg (str "NARS answer on " (narsese-print (:statement task)) "? is " (task-to-narsese solution))]
                                    (println msg))))
  (for [x ops] (do (println "Registering " x) (register x)))
  (nars-input-narsese "<(^\"#'clojure.core/+\",1,2) --> ?>?")
  (nars-input-narsese "(^\"#'clojure.core/+\",1,2)?"))
