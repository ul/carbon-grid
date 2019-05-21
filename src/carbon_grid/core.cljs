(ns ^:figwheel-hooks carbon-grid.core
  (:require
   [carbon.rx :as rx :include-macros]
   [carbon.vdom :as vdom]
   [goog.object :as obj]
   [goog.dom :as gdom]))

;; The grid is represented as a nested vector: vector of columns where each column is a vector of rows.
;; This comes with a trade-off, but the main benefit is that we don't need to do much of index
;; conversion when traversing in horizontal or vertical direction.
;; Each element of the grid represents position of the node, which is again represented as a vector
;; of coordinates: [x y].
;; Nodes are rigidly connected to their horizontal and vertical neighbours, and the length of
;; connection is hardcoded to be one unit. You need to scale appropriately when rendering and moving.
(defn init-grid [width height]
  (vec (for [x (range 1 (inc width))]
    (vec (for [y (range 1 (inc height))]
      [x y])))))

(defn enumerate [xs]
  (map vector (range) xs))

;; Manually unrolled and specialized functions for a better performance.

(defn vec2+ [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn vec2- [[x1 y1] [x2 y2]] 
  [(- x1 x2) (- y1 y2)])

(defn vec2div [[x y] m]
  [(/ x m) (/ y m)])

(defn vec2mag [[x y]] 
  (js/Math.sqrt (+ (* x x) (* y y))))

(defn vec2norm [v]
  (let [m (vec2mag v)
        [x y] v]
    (if (pos? m)
      [(/ x m) (/ y m)]
      v)))

;;

;; How much we should move the neighbour w to keep it at unit distance from v moved by delta.
(defn neighbour-delta [delta v w]
  (let [connection (vec2- v w)
        new-distance (vec2+ delta connection)
        new-connection (vec2norm new-distance)]
    (vec2- new-distance new-connection)))

(def initial-grid (init-grid 20 20))

;; :grid is duplicated as a :flux-grid to avoid error accumulation during dragging:
;; we just treat :grid as the grid on drag start, and :flux-grid as a temporary dynamic grid
;; and take a delta between them instead of applying delta straight to the :grid on each step.
;; Replace def with defonce when structure is stabilized and you don't want data to be reset on hot
;; reload in devmode.
(def app-state (rx/cell
  {:grid initial-grid
   :flux-grid initial-grid
   :scale 50
   :node-radius 5
   :drag nil}))

(defn get-app-element []
  (gdom/getElement "app"))

(defn on-drag-move [e]
  (let [screen-coords' [(obj/get e "clientX") (obj/get e "clientY")]]
    (swap! app-state (fn [state]
      (let [{:keys [drag scale grid]} state
            [node screen-coords] drag
            delta (vec2div (vec2- screen-coords' screen-coords) scale)
            width (count grid)
            height (count (first grid))]
        ;; The main idea of this simulation is that connections between nodes are rigid
        ;; and of a constant length, but could be rotated freely. And nodes are freely movable.
        ;; It means that when you move a node, it pulls connected nodes to maintain a constant
        ;; (hardcoded to be unit in our case) distance to them.
        (assoc state :flux-grid
          (loop [[nodes-to-process visited-nodes] [#queue [[node delta]] #{node}]
                 grid grid]
            (let [[node delta] (first nodes-to-process)]
              (if (and node (pos? (vec2mag delta)))
                (recur
                  (reduce (fn [acc direction]
                            (let [[nodes-to-process visited-nodes] acc
                                  node' (vec2+ node direction)
                                  [i' j'] node']
                              (if (and (< -1 i' width)
                                       (< -1 j' height)
                                       (not (contains? visited-nodes node')))
                                [(conj nodes-to-process
                                       [node' (neighbour-delta delta
                                                               (get-in grid node)
                                                               (get-in grid node'))])
                                 (conj visited-nodes node')]
                                acc)))
                          [(pop nodes-to-process) visited-nodes]
                          [[-1 0] [0 1] [1 0] [0 -1]])
                  (update-in grid node #(vec2+ % delta)))
                grid)))))))))

(defn freeze-grid [state]
  (-> state 
      (assoc :grid (get state :flux-grid))
      (assoc :drag nil)))

(defn on-drag-stop []
  (.removeEventListener js/window "mousemove" on-drag-move)
  (.removeEventListener js/window "mouseup" on-drag-stop)
  (swap! app-state freeze-grid))

(defn on-drag-start [e]
  (let [dataset (obj/getValueByKeys e "target" "dataset")
        i (js/parseInt (obj/get dataset "i") 10)
        j (js/parseInt (obj/get dataset "j") 10)]
    (.addEventListener js/window "mousemove" on-drag-move)
    (.addEventListener js/window "mouseup" on-drag-stop)
    (swap! app-state assoc :drag [[i j] [(obj/get e "clientX") (obj/get e "clientY")]])))

(defn range-input [label attribute min max]
  [:label {:style {:flex "1" :display "flex" :flex-direction "column"}}
    label
    [:input
     {:type "range"
      :min min
      :max max
      :value (get @app-state attribute)
      :on-input #(swap! app-state assoc attribute (js/parseInt (obj/getValueByKeys % "target" "value") 10))}]])

(defn app []
  (let [{:keys [flux-grid scale node-radius]} @app-state
        width (count flux-grid)
        height (count (first flux-grid))]
    [:div
      [:div {:style {:display "flex"}}
       [range-input "Scale" :scale 1 100]
       [range-input "Node radius" :node-radius 1 10]]
      [:svg {:style {:width (* (inc width) scale) :height (* (inc height) scale)}}
          (for [[i column] (enumerate flux-grid)
                row (enumerate column)
                :let [[j [x y]] row]]
            [:circle
              {:key (str i ":" j)
               :cx (* scale x)
               :cy (* scale y)
               :r node-radius
               ;; i & j are passed via data attribute to have static callback for better performance
               :data-i i
               :data-j j
               :on-mousedown on-drag-start}])]]))

(vdom/mount [app] (get-app-element))

;; specify reload hook with ^;after-load metadata
(defn ^:after-load on-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
