(ns narjure.global-atoms
  (:require
    [narjure.bag :as b]
    [narjure.defaults :refer :all]
    [nal.deriver.projection-eternalization :refer [project-eternalize-to]]))

(def c-bag (atom (b/default-bag max-concepts)))

(def nars-time (atom 0))

(def nars-id (atom -1))

(def output-display (atom '()))
(def output-search (atom ""))

(def lense-taskbags (atom {}))                                     ;mapping concept term to its task bag
;this variable is purely for visualization/debugging purposes!!

(def lense-termlinks (atom {}))                                     ;mapping concept term to its task bag
;this variable is purely for visualization/debugging purposes!!

(defn lense-max-statement-confidence-projected-to-now [concept-term task-type event]
  (project-eternalize-to @nars-time (if (= task-type :goal)
     (:strongest-desire-about-now ((:elements-map @c-bag) concept-term))
     (if event
       (:strongest-belief-event-about-now ((:elements-map @c-bag) concept-term))
       (:strongest-belief-about-now ((:elements-map @c-bag) concept-term)))) @nars-time))

