(ns gui.actors
  (:require [seesaw.core :refer :all]))

(def actor-level-width 175)
(def actor-level-height 100)

(def concept-color [200 255 255])
(def derived-task-color [200 200 255])
(def task-color [255 255 200])
(def util-color [200 255 200])
(def gui-color [240 240 255])

(def nodes [{:name :concept-manager :px 75 :py 0 :backcolor concept-color}
            {:name :concepts :px -100 :py 150 :backcolor concept-color}
            {:name :task-dispatcher :px 600 :py 0 :backcolor task-color}
            {:name :input :px 400 :py -400 :displaysize 10.0 :backcolor gui-color} ;-600
            {:name :sentence-parser :px 400 :py -300 :backcolor util-color}       ;-500
            {:name :task-creator :px 400 :py -150 :backcolor util-color}
            ;{:name :input-load-reducer :px 400 :py -325}
            {:name :operator-executor :px -100 :py -150 :backcolor [255 200 200]}
            ;{:name :event-buffer :px 200 :py 150}
            {:name :general-inferencer :px 400 :py 300 :backcolor derived-task-color}
            {:name :derived-load-reducer :px 400 :py 150 :backcolor util-color}
            {:name :event-selector :px 600 :py 150 :backcolor task-color}
            {:name :event-bag :px 775 :py 150 :backcolor task-color}
            {:name :concept-selector :px -100 :py 300 :backcolor concept-color}
            {:name :concept-bag :px 75 :py 300 :backcolor concept-color}
            {:name :output :px 600 :py -400 :backcolor gui-color}])

(def edges [{:name ":create-concept-msg" :from :task-dispatcher :to :concept-manager :unidirectional true}
            {:name ":task-msg" :from :concept-manager :to :task-dispatcher :unidirectional true}
            ;{:from :task-dispatcher :to :event-buffer :unidirectional true}
            {:name "              :do-inference-msg" :from :concepts :to :general-inferencer :unidirectional true}
            {:name ":operator-execution-msg" :from :concepts :to :operator-executor :unidirectional true}
            {:name ":derived-sentence-msg" :from :operator-executor :to :task-creator :unidirectional true}
            {:name ":do-inference-msg" :from :event-selector :to :general-inferencer :unidirectional true}
            {:name ":narsese-string-msg" :from :sentence-parser :to :task-creator :unidirectional true}
               ;{:from :sentence-parser :to :input-load-reducer}
               ;{:from :input-load-reducer :to :task-creator}
            {:name ":derived-sentence-msg" :from :general-inferencer :to :derived-load-reducer :unidirectional true}
            {:name ":derived-sentence-msg" :from :derived-load-reducer :to :task-creator :unidirectional true}
            {:name "         :task-msg" :from :task-creator :to :task-dispatcher :unidirectional true}
            {:name ":inference-request-msg" :from :concept-selector :to :concepts :unidirectional true}
            {:name "add element" :from :concept-manager :to :concept-bag :unidirectional true}
            {:name ":task-msg" :from :task-dispatcher :to :concepts :unidirectional true}
            {:name "         add element" :from :task-dispatcher :to :event-bag :unidirectional true}])

(def graph-actors [nodes edges actor-level-width actor-level-height])