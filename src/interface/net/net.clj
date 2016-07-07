; TODO< suck up entropy from https://github.com/Raynes/irclj/blob/master/src/irclj/connection.clj >

(ns interface.net
    (:require [quil.core :as q]
              [quil.middleware :as m]
              [nal.deriver.projection-eternalization :refer [project-eternalize-to]]
              [seesaw.core :refer :all]
              [narjure.core :as nar]
              [co.paralleluniverse.pulsar.actors :refer [whereis cast!]]
              [narjure.general-inference.inference-request-router :as inference-request-router]
              [narjure.general-inference.concept-selector :as concept-selector]
              [narjure.general-inference.general-inferencer :as general-inferencer]
              [narjure.memory-management.concept-manager :as concept-manager]
              [narjure.memory-management.task-dispatcher :as task-dispatcher]
              [narjure.perception-action.operator-executor :as operator-executor]
              [narjure.perception-action.sentence-parser :as sentence-parser]
              [narjure.perception-action.task-creator :as task-creator]
        ;[narjure.perception-action.input-load-reducer :as input-load-reducer]
              [narjure.perception-action.derived-load-reducer :as derived-load-reducer]
              [narjure.memory-management.concept :as concepts]
              [narjure.global-atoms :refer :all]
              [narjure.debug-util :refer :all]
              [narjure.bag :as b]
              [narjure.defaults :refer [priority-threshold]]
              [clojure.set :as set]
              [clojure.string :as str]

              [clojure.java.io :as io]
              )

    )

(require '[clojure.java.io :as io])
(import '[java.net ServerSocket])

(defn receive
    "Read a line of textual data from the given socket"
    [socket]
    (.readLine (io/reader socket)))

(defn send2
    "Send the given string message out over the given socket"
    [socket ^String msg]
    (let [writer (io/writer socket)]
        (.write writer msg)
        (.flush writer)))

(defn serve [port handler]
    (with-open [server-sock (ServerSocket. port)
                sock (.accept server-sock)]
        (let [msg-in (receive sock)
              msg-out (handler msg-in)]
            (send2 sock msg-out))))

(serve 4000 nil)
