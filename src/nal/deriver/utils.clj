(ns nal.deriver.utils
  (:require [clojure.walk :as w]))

(defn not-operator?
  "Checks if element is not operator"
  [el] (re-matches #"[akxA-Z$]" (-> el str first str)))

(def operator? (complement not-operator?))

(defn postwalk-depth1
  "Clojure postwalk but with depth 1 only as demanded by NAL4 rules"
  ([f form]
   (postwalk-depth1 f 0 form))
  ([f depth form]
   (if (> depth 1)
     (w/walk (partial postwalk-depth1 identity (inc depth)) identity form)
     (w/walk (partial postwalk-depth1 f (inc depth)) f form))))

(defmacro walk
  "Macro that helps to replace elements during walk. The first argument
  is collection, rest of the arguments are cond-like
  expressions. Default result of cond is element itself.
  el (optionally :el) is reserved name for current element of collection."
  ([coll & conditions]
   (let [el (gensym)
         replace-el (fn [coll]
                      (w/postwalk #(if (or (= 'el %) (= :el %)) el %) coll))]
     `(~(if (= (last conditions) :single-level-hack)
         postwalk-depth1
         w/postwalk)
        (fn [~el] (cond ~@(replace-el (if (= (last conditions) :single-level-hack) (drop-last conditions) conditions))
                        :else ~el))
        ~coll))))