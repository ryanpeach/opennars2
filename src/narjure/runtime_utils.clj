(ns narjure.runtime-utils
     (:require
       [co.paralleluniverse.pulsar
        [core :refer :all]
        [actors :refer :all]]
       [narjure.narsese :refer [parse2]]
       [nal.deriver.truth :refer [expectation]]
       [nal.deriver.projection-eternalization :refer [project-eternalize-to]]
       [narjure.global-atoms :refer [lense-taskbags nars-time]])
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

(defn max-statement-confidence-projected-to-now [concept-term task-type]
  (let [li (filter (fn [z] (= (:task-type (:task (second z))) task-type))
                             (:elements-map ((deref lense-taskbags) concept-term)))]
              (if (= (count li) 0)
                {:truth [0.5 0.0]}
                (project-eternalize-to
                  (deref nars-time)
                  (:task (second (apply max-key (fn [y]
                                                  (second (:truth (:task (second y)))))
                                        li)))
                  (deref nars-time)))))