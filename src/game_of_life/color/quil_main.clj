(ns game-of-life.color.quil-main
  (:require [quil.core :as q]
            [quil.middleware :as m]

            [helpers.general-helpers :as g]
            [helpers.quil-helpers :as qh]

            [game-of-life.color.environment :as e])
  (:gen-class))

(defrecord Game [env drop-size color-index])

(def width 1500)
(def height 1500)

(def board-side-length 75)

(def board-width board-side-length)
(def board-height board-side-length)

(def width-ratio (/ width board-width))
(def height-ratio (/ height board-height))

(def cell-size (min width-ratio height-ratio))

(def color-sample-size (* 5 cell-size))

(def rand-gen (g/new-rand-gen 99))

(def drop-step-size 3)

(def cell-states [[255 0 0] [100 0 0]
                  [0 255 0] [0 100 0]
                  [0 0 255] [0 0 100]
                  [255 255 255] [100 100 100]])

(defn color-at-index [i]
  (cell-states i))

(defn setup-state []
  (q/frame-rate 20)

  (->Game
    (e/random-environment cell-states [board-width board-height] 0.3 rand-gen)
    2
    0))

(defn update-state [state]
  (-> state
    (update :env e/advance-environment)))

(defn draw-cell [x y]
  (q/point x y))

(defn draw-color-drop-sample [color]
  (q/with-stroke color
    (qh/with-weight color-sample-size
      (q/point 0 0))))

(defn draw-state [state]
  (q/background 0 0 0)
  (let [{e :env i :color-index} state
        {[w h] :dimensions} e]

    (draw-color-drop-sample (color-at-index i))

    (qh/with-weight cell-size
      (doseq [y (range h)
              x (range w)]

          (let [state (e/state-at e x y)
                x' (* x width-ratio)
                y' (* y height-ratio)]
            (when (not= state ::e/dead)
              (q/with-stroke state
                (draw-cell x' y'))))))))

(defn screen-to-board-coords [screen-x screen-y]
  [(int (/ screen-x width-ratio))
   (int (/ screen-y height-ratio))])

(defn mouse-handler [state event]
  (let [{mx :x my :y} event
        [x y] (screen-to-board-coords mx my)
        {e :env d :drop-size i :color-index} state
        hd (int (/ d 2))
        coords (e/wrapping-coords-surrounding e hd x y)
        drop-color (cell-states i)]

    (update state :env
      #(reduce
         (fn [acc-env [x y]]
           (e/set-state-at acc-env x y drop-color))
         %
         coords))))

(defn random-random-env [old-env rand-gen]
  (e/random-environment cell-states
                        (:dimensions old-env)
                        (g/random-double 0.05 0.7 rand-gen)
                        rand-gen))

(defn key-handler [state event]
  (let [{key :raw-key} event
        u (partial update state)
        n-colors (count cell-states)]
    (case key
      \c (u :env e/kill-all-cells)

      \= (u :drop-size #(min width height (+ % drop-step-size)))
      \- (u :drop-size #(max 0 (- % drop-step-size)))

      \n (u :color-index #(g/wrap (dec %) 0 (dec n-colors)))
      \m (u :color-index #(g/wrap (inc %) 0 (dec n-colors)))

      \r (u :env #(random-random-env % rand-gen))

      state)))

(defn -main [& args]
  (let []

    (q/defsketch Game-of-Life
                 :size [width height]

                 :setup setup-state
                 :update update-state
                 :draw draw-state

                 :mouse-clicked mouse-handler
                 :mouse-dragged mouse-handler

                 :key-pressed key-handler

                 :middleware [m/fun-mode])))