(ns examples.math
  (:require [narjure.sensorimotor :refer [nars-register-operation]]))

; --- Support functions ---
(defn apply_args
    "Like apply but assumes the last item is a list,
     the first item is the operation, and the rest are constant.
     Ex: (apply * (into [1 2] args)) --> (apply_args * 1 2 args)"
    [op & args]
    (apply op (into (butlast x) (last x))))

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
      (try (apply_args op args)
      (catch Exception e [false]))))

; List the ops we desire by their library
(def ops (seq {"math" ['+ '- '* '/ 'quot 'rem 'float 'double 'int]}))

; Get their names by appending their op name to the library name 
(def names (map-hash-list2 (fn [lib op] (str lib "_" op)) ops))

; zip the ops and their names
(def op-names (for [x (keys ops)] [(get ops x) (get names x)]))

; register all operations
(defn register []
  (for [[op n] op-names] (nars-register-operation n (template op))))
