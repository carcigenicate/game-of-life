(ns game-of-life.edmonton.quil-main
  (:require [quil.core :as q]
            [quil.middleware :as m]

            [helpers.general-helpers :as g]
            [helpers.quil-helpers :as qh]

            [game-of-life.edmonton.environment :as e]))

(defrecord Game [env drop-size])

(def width 1800)
(def height 1300)

(def board-width 100)
(def board-height 100)

(def width-ratio (/ width board-width))
(def height-ratio (/ height board-height))

(def cell-size (min width-ratio height-ratio))

(def rand-gen (g/new-rand-gen 99))

(def drop-step-size 1)

(defn setup-state []
  (q/frame-rate 2)

  (->Game
    (e/random-environment [board-width board-height] 0.5 rand-gen)
    25))

(defn update-state [state]
  (-> state
    (update :env e/advance-environment)))

(defn draw-cell [x y]
  (q/point x y))

(defn draw-state [state]
  (q/background 0 0 0)
  (let [{e :env} state
        {[w h] :dimensions} e]

    (qh/with-weight cell-size
      (doseq [y (range h)
              x (range w)]

        (when (e/alive-at? e x y)
          (let [x' (* x width-ratio)
                y' (* y height-ratio)]
            (q/with-stroke [255 255 255] #_(qh/random-color rand-gen)
              (draw-cell x' y'))))))))

(defn screen-to-board-coords [screen-x screen-y]
  [(int (/ screen-x width-ratio))
   (int (/ screen-y height-ratio))])

(defn mouse-handler [state event]
  (let [{mx :x my :y} event
        [x y] (screen-to-board-coords mx my)
        {e :env d :drop-size} state
        hd (int (/ d 2))
        coords (e/coords-surrounding e hd x y)]

    (update state :env
      #(reduce
         (fn [acc-env [x y]]
           (e/set-state-at acc-env x y ::e/alive))
         %
         coords))))

(defn key-handler [state event]
  (let [{key :raw-key} event
        u (partial update state)]
    (case key
      \c (u :env e/kill-all-cells)

      \= (u :drop-size #(min width height (+ % drop-step-size)))

      \- (u :drop-size #(max 0 (- % drop-step-size)))

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