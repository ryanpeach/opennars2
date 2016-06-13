(ns narjure.runtime-utils
     (:require
       [co.paralleluniverse.pulsar
        [core :refer :all]
        [actors :refer :all]]
       [narjure.narsese :refer [parse2]        ]
       [narjure.global-atoms :refer [nars-time]])
     (:refer-clojure :exclude [promise await]))

(defn get-bag-atom [atom]
  atom)

(defn get-concept-count []
  (let [bag (get-bag-atom narjure.global-atoms/c-bag)]
    (narjure.bag/count-elements @bag)))

(defn get-lines [file]
  (clojure.string/split-lines (slurp file)))

(defn load-NAL-file [file]
  (doseq [narsese-str (get-lines file)]
    (println (str narsese-str))
    (cast! (whereis :sentence-parser) [:narsese-string-msg narsese-str]))
  )

(defn test-parser [n]
  (doseq [i (range n)]
    (parse2 "<(*,{SELF}) --> op_down>!")))

(defn test [n]
  (time
    (test-parser n)))
