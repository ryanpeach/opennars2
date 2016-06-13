(ns narjure.global-atoms
  (:require
    [narjure.bag :as b]
    [narjure.defaults :refer :all]))

(def c-bag (atom (b/default-bag max-concepts)))
(def e-bag (atom (b/default-bag max-events)))

(def nars-time (atom 0))

(def nars-id (atom -1))

(def output-display (atom '()))
(def output-search (atom ""))

(def lense-taskbags (atom {}))                                     ;mapping concept term to its task bag
;this variable is purely for visualization/debugging purposes!!

(def lense-termlinks (atom {}))                                     ;mapping concept term to its task bag
;this variable is purely for visualization/debugging purposes!!