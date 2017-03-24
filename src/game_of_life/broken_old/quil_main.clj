(ns game-of-life.broken_old.quil-main
  (:require [helpers.general-helpers :as g]
            [game-of-life.broken_old.enviro :as e]
            [quil.core :as q]
            [quil.middleware :as m]))

(def screen-dims [1800 2000])
(def board-dims [100 100])

(def starting-pop-perc 0.3)

(def rand-gen (g/new-rand-gen 993061001))

(def board-screen-ratio
  (let [smallest-scr-dim (apply min screen-dims)
        largest-brd-dim  (apply max board-dims)]
    (/ largest-brd-dim smallest-scr-dim)))

(def dot-radius (* board-screen-ratio 300))

(defn setup-state []
  (q/frame-rate 30)

  (let [[bw bh] board-dims]
    (e/random-alive-enviro bw bh starting-pop-perc rand-gen)))

(defn board-pos->screen-pos [board-position]
  (map #(/ % board-screen-ratio) board-position))

(defn update-state [state]
  (e/gen-next-enviro state))

(defn- draw-dot [x y]
  (q/with-stroke [50 50 200]
    (q/stroke-weight dot-radius)
    (q/point x y)))

(defn draw-state [state]
  (q/background 200 200 200)

  (e/for-cells state
    (fn [[x y :as cell-pos] state]
      (if (= state :alive)
        (let [[s-x s-y] (board-pos->screen-pos cell-pos)]
          (draw-dot s-x s-y))))))

(defn -main []
  (q/defsketch quil-game-of-life
               :size screen-dims
               :middleware [m/fun-mode]
               :setup setup-state
               :update update-state
               :draw draw-state
               :features [:keep-on-top]))