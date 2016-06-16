(ns gui.lense
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [gui.actors :refer [graph-actors]]
            [gui.gui-utils :refer :all]
            [gui.gui :refer [graph-gui]]
            [nal.deriver.projection-eternalization :refer [project-eternalize-to]]
            [gui.hud :refer [hud]]
            [gui.hnav :as hnav]
            [seesaw.core :refer :all]
            [gui.globals :refer :all]
            [narjure.core :as nar]
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
            [narjure.defaults :refer [priority-threshold]]))

(defn bag-format [st]
  (clojure.string/replace st "}" "}\n"))

(defn bagfilter [fil bag]
  (apply vector (filter (fn [x] (.contains (str x) (deref fil))) bag)))

(defn bagshow [bag filteratom]
  (bag-format (limit-string
                (str (bagfilter filteratom
                                (:priority-index bag))) 20000)))

(def debugmessage {:inference-request-router [(fn [] (deref inference-request-router/display)) inference-request-router/search]
                   :concept-selector     [(fn [] (deref concept-selector/display)) concept-selector/search]
                   :general-inferencer   [(fn [] (deref general-inferencer/display)) general-inferencer/search]
                   :concept-manager      [(fn [] (deref concept-manager/display)) concept-manager/search]
                   :task-dispatcher      [(fn [] (deref task-dispatcher/display)) task-dispatcher/search]
                   :operator-executor    [(fn [] (deref operator-executor/display)) operator-executor/search]
                   :sentence-parser      [(fn [] (deref sentence-parser/display)) sentence-parser/search]
                   :task-creator         [(fn [] (deref task-creator/display)) task-creator/search]
                   :concepts             [(fn [] (deref concepts/display)) concepts/search]
                   :concept-bag          [(fn [] (bagshow @c-bag concept-filter)) concept-filter]
                   :derived-load-reducer [(fn [] (deref derived-load-reducer/display)) derived-load-reducer/search]
                   :input                [(fn [] "") inputstr]
                   :output               [(fn [] (deref output-display)) output-search]
                   :+prioTh. [(fn [] (deref prio-threshold))]})

(def static-graphs [graph-actors graph-gui])
(def graphs (atom static-graphs))

(defn setup []
  (q/frame-rate 30)
  ;(nar/run)
  (merge hnav/states {}))

(defn update [state] state)

(defn nameof [a]
  (if (string? a) a (name a)))

(defn draw-actor [{:keys [name px py backcolor frontcolor displaysize titlesize stroke-weight custom-w custom-h]} node-width node-height]
  (q/stroke-weight (if (= nil stroke-weight) 1.0 stroke-weight))
  (apply q/fill (invert-color (if (= backcolor nil) [255 255 255] backcolor)))
  (q/rect px py (if custom-w custom-w node-width) (if custom-h custom-h node-height))
  (apply q/fill (invert-color (if (= frontcolor nil) [0 0 0] frontcolor)))
  (q/text-size (if (= nil titlesize) 10.0 titlesize))
  (q/text (nameof name) (+ px 5) (+ py (if (= nil titlesize) 10.0 titlesize)))
  (q/text-size (if (= displaysize nil) 2.0 displaysize))
  (when (contains? debugmessage name)
    (q/text (hnav/display-string debugmessage name)
            (+ px 5) (+ py 20)))
  (q/text-size 2.0))

(defn in-picture [state p hud]
  (if hud
    true
    (if (and (> (:px p) (hnav/mouse-to-world-coord-x state 0))
            (< (:px p) (hnav/mouse-to-world-coord-x state (hnav/width)))
            (> (:py p) (hnav/mouse-to-world-coord-y state 0))
            (< (:py p) (hnav/mouse-to-world-coord-y state (hnav/height))))
     true
     false)))

(defn draw-graph [state [nodes edges node-width node-height] hud]
  (let [prefer-id (fn [n] (if (= nil (:id n))
                            (:name n)
                            (:id n)))]
    (doseq [c edges]
      (when (and (some #(= (:from c) (prefer-id %)) nodes)
                 (some #(= (:to c) (prefer-id %)) nodes))
        (let [pxtransform (fn [x] (+ (:px x) (/ node-width 2.0)))
              pytransform (fn [y] (+ (:py y) (/ node-height 2.0)))
              left (first (filter #(= (:from c) (prefer-id %)) nodes))
              right (first (filter #(= (:to c) (prefer-id %)) nodes))
              middle {:px (/ (+ (:px left) (:px right)) 2.0)
                      :py (/ (+ (:py left) (:py right)) 2.0)}
              namepos {:px (/ (+ (* 0.6 (:px left)) (* 0.4 (:px right))) 1.0)
                       :py (+ 3 (/ (+ (* 0.6 (:py left)) (* 0.4 (:py right))) 1.0))}
              target (if (not= true (:unidirectional c))
                       right middle)
              weight (if (not= nil (:stroke-weight c))
                       (:stroke-weight c)
                       0.5)
              left-x (pxtransform left) left-y (pytransform left)
              right-x (pxtransform right) right-y (pytransform right)
              target-x (pxtransform target) target-y (pytransform target)
              middle-x (pxtransform middle) middle-y (pytransform middle)
              namepos-x (pxtransform namepos) namepos-y (pytransform namepos)
              pointf (fn [a b] {:px a :py b})
              name (:name c)]
          (when (or (in-picture state (pointf left-x left-y) hud)
                    name
                    (and @link-labels
                         (in-picture state (pointf namepos-x namepos-y) hud)))

            (let [eval-color (if (= nil (:link-color c) )
                               (invert-color [0 0 0])
                               (invert-color (:link-color c)))
                  col eval-color
                  r (first col)
                  g (second col)
                  b (nth col 2)]

              (q/stroke r g b)
            (q/stroke-weight (* weight 2.0))
            (q/line left-x left-y
                    middle-x target-y)
            (when (and (:unidirectional c)
                       (not (:opposite-edge-exists c)))
              (if (:ghost-opposite c)
                (do
                  (q/stroke (invert-comp 220.0))
                  (q/stroke-weight 0.009))
                (do
                  (q/stroke-weight weight)))
              (q/line right-x right-y
                      middle-x middle-y))
            (when (not= nil name)
              (q/text (str name) namepos-x namepos-y))))))))
  (q/stroke (first (invert-color [0 0 0])))
  (doseq [a nodes]
    (when (in-picture state (assoc a :px (+ (:px a) (/ node-width 2.0))
                                     :py (+ (:py a) (/ node-height 2.0))) hud)
      (draw-actor a node-width node-height))))

(def selected-concept (atom []))

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

;copy of draw below marked as draw2
(defn draw [state]
  (q/background (first (invert-color [255 255 255])))
  (q/reset-matrix)
  (q/push-matrix)
  (hnav/transform state)
  (doseq [g @graphs]
    (draw-graph state g false))
  ;(reset! graphs static-graphs)
  ;concept graph
  (when (> (hnav/mouse-to-world-coord-x state (hnav/width)) 1600)
    (try (let [elems (apply vector (:priority-index (deref c-bag)))
              nodes (for [i (range (count elems))]
                      (let [elem (elems i)
                            ratio (* 30.0 (+ 0.10 (/ i (count elems))))
                            a 75.0
                            id (:id elem)
                            priority (:priority elem)
                            quality (:quality ((:elements-map (deref c-bag)) id))]
                        (when (and (.contains (str id) (deref concept-filter))
                                   (> priority priority-threshold)
                                   (> priority @prio-threshold))
                          {:name          (str "\n" (narsese-print id)
                                               (if (= id @selected-concept)
                                                 (str "\npriority: " priority " " "quality: " quality " "
                                                      "truth: " (:truth (max-statement-confidence-projected-to-now id :belief)) " "
                                                      "desire: " (:truth (max-statement-confidence-projected-to-now id :goal)) "\n"
                                                      (bag-format
                                                        (limit-string (str (apply vector
                                                                                  (:elements-map (@lense-taskbags id)))) 20000)))
                                                 ""))       ;"\n" @lense-termlinks
                           :px            (+ 3500 (* a ratio (Math/cos ratio)))
                           :py            (+ 200 (* a ratio (Math/sin ratio)))
                           :displaysize   1.0
                           :backcolor     [(- 255 (* priority 255.0)) 255 255]
                           :titlesize     2.0
                           :stroke-weight 0.5
                           :id            id
                           :onclick       (fn [state]
                                            (reset! selected-concept id))})))
              edges (for [n nodes
                          [k [freq conf]] (@lense-termlinks (:id n))]
                      (let [disttomiddle (Math/abs (- 0.5 freq))
                            rterm (/ (if (>= freq 0.5) (* 510.0 disttomiddle) 0.0) 2.0)
                            bterm (/ (if (< freq 0.5) (* 510.0 disttomiddle) 0.0) 2.0)]
                        {:from                 (:id n)
                         :to                   k :unidirectional true
                         :stroke-weight        (* 0.5 conf)
                         :link-color           (if @invert-colors
                                                 (invert-color [(+ 128.0 rterm) 128.0 (+ 128.0 bterm)])
                                                 [0.0 (+ 0 rterm) (+ 0 bterm)])
                         :name                 (when @link-labels [freq conf])
                         :opposite-edge-exists (some (fn [[tl2 _]]
                                                       (= tl2 (:id n)))
                                                     (@lense-termlinks k))
                         :ghost-opposite       true}))
              concept-graph [(filter #(not= % nil) nodes) edges 10 10]]
          (reset! graphs (concat static-graphs [concept-graph])))
        (catch Exception e (println e))))
  (q/pop-matrix)
  (draw-graph state hud true)
  )

(defn key-pressed [state event]
  (let [name (name (:key event))
        code (:key-code event)]
    (swap! (deref input-string) (fn [inputstr] (str (if (not (= code 8))
                                                      inputstr (subs inputstr 0 (max 0 (dec (count inputstr)))))
                                                    (if (not (> (count name) 1))
                                                      (if (not (= code 8))
                                                        name "") ""))))
    state))

(defn lense-mousepress [state event]
  (hnav/mouse-pressed graphs debugmessage false state event)
  (hnav/mouse-pressed (atom [hud]) debugmessage true state event))

(q/defsketch example
             :size [(hnav/width) (hnav/height)]
             :setup setup
             :draw draw
             :update update
             :mouse-pressed lense-mousepress
             :mouse-dragged hnav/mouse-dragged
             :mouse-wheel hnav/mouse-wheel
             :key-pressed key-pressed
             :middleware [m/fun-mode]
             :features [ :resizable ]
             :title "OpenNARS 2.0.0: Lense")
