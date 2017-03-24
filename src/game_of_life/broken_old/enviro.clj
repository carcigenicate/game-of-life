(ns game-of-life.broken_old.enviro
  (:require [clojure.string :refer [join]]
            [helpers.general-helpers :as g]))

(def alive-states #{:alive :dead})

(def aliveCellString "#")
(def deadCellString " ")

; Represents the Game of Life world.
; cells is a vector representing a 2D matrix of cells
; dims is the dimensions of the world as a Point
(defrecord Enviro [cells dims])

(declare create-dead-enviro)

(defn create-dead-enviro [width height]
  (->Enviro
    (vec (replicate (* width height) :dead))
    [width height]))

(def testDeadE (create-dead-enviro 60 20))

(defn index-of [width x y]
  (+ (* y width) x))

(defn enviro-index-of [enviro x y]
  (let [width (-> enviro :dims first)]
    (index-of width x y)))

(defn enviro-coord-from-index [enviro index]
  (let [width (-> enviro :dims first)]
    [(rem index width)
     (int (Math/floor (/ index width)))]))

(defn cell-at
  ([enviro x y]
   (cell-at enviro (enviro-index-of enviro x y)))

  ([enviro index]
   ((:cells enviro) index)))


(defn cells-at [enviro points]
  (for [[x y] points]
    (cell-at enviro x y)))

(defn set-cell-at
  ([enviro x y cell]
   (set-cell-at enviro (enviro-index-of enviro x y) cell))

  ([enviro index cell]
   (let [cells (:cells enviro)]
     (assoc enviro :cells
                   (assoc cells index cell)))))

(defn generate-neighbor-points
  ([cx cy r]
   (let [start-x (- cx r)
         end-x (+ cx r 1) ; Adding 1 so it's inclusive
         start-y (- cy r)
         end-y (+ cy r 1)]
     (for [y (range start-y end-y)
           x (range start-x end-x)
           :when (or (not= x cx) (not= y cy))]
       [x y])))

  ([center-point r]
   (let [cx (first center-point)
         cy (second center-point)]
     (generate-neighbor-points cx cy r))))

(defn filter-oob-neighbor-points [enviro neighbor-points]
  (let [width (-> enviro :dims first)
        height (-> enviro :dims second)]
    (filter (fn [point]
              (let [[x y] point]
                   (and
                     (<= 0 x (- width 1))
                     (<= 0 y (- height 1)))))
            neighbor-points)))


(defn count-alive-neighbors [enviro x y]
  (let [neighbor-points (filter-oob-neighbor-points
                          enviro
                          (generate-neighbor-points x y 1))
        neighbors (cells-at enviro neighbor-points)]
    (reduce (fn [acc neighbor]
              (if (= neighbor :alive)
                (inc acc)
                acc))
            0
            neighbors)))

(defn- enviro-specs-str [enviro]
  (let [[width height] (:dims enviro)]
    (str width " x " height)))

(defn- add-row-padding [paddingN row]
  (let [paddingStr (apply str (replicate paddingN " "))]
    (join paddingStr row)))

(defn- enviro-board-row [enviro rowN]
  (let [width (-> enviro :dims first)
        start-x (index-of width 0 rowN)
        end-x (index-of width width rowN)]
    (map (fn [index] (if (= (cell-at enviro index) :alive) aliveCellString deadCellString))
         (range start-x end-x))))

(defn- enviro-rows [enviro]
  (let [height (-> enviro :dims second)]
    (map #(enviro-board-row enviro %) (range 0 height))))

(defn enviro-board-str [enviro hor-padding]
  (let [rows (enviro-rows enviro)
        padded-rows (map (partial add-row-padding hor-padding) rows)]
    (str (clojure.string/join "\n" padded-rows) "\n")))

(defn- print-enviro [enviro]
  (println (str
             (enviro-specs-str enviro) "\n"
             (enviro-board-str enviro 0))))

(defn for-cells
  "Iterates over every cell on the board.
  The callback should take 2 arguments: [[x-pos y-pos] cell-state]"
  [enviro f]
  (let [{[width height] :dims cells :cells} enviro]
    (doseq [i (range (* width height))
            :let [cell-pos (enviro-coord-from-index enviro i)
                  state (cell-at enviro i)]]
      (f cell-pos state))))

(defn- future-cell-state-by-neighbors [enviro nNeighbors x y]
  (let [state (cell-at enviro x y)]
    (cond
      (or (< nNeighbors 2) (> nNeighbors 3)) :dead
      (and (<= 2 nNeighbors 3) (= state :alive)) :alive
       :else :dead)))

(defn- future-cell-state [enviro x y]
  (future-cell-state-by-neighbors enviro
    (count-alive-neighbors enviro x y)
    x y))

(defn gen-next-enviro [starting-enviro]
  (let [cells (:cells starting-enviro)]
    (reduce (fn [acc i]
              (let [cell (cell-at starting-enviro i)
                    [x y] (enviro-coord-from-index starting-enviro i)
                    new-state (future-cell-state starting-enviro x y)]
                (set-cell-at acc x y new-state)))
            starting-enviro
            (range 0 (count cells)))))

(defn sim-gens [enviro n-gens gen-f]
  (reduce (fn [acc i] ()
              (let [current-e (last acc)
                    next-e (gen-next-enviro current-e)]
                (do
                  (gen-f next-e)
                  (conj acc next-e))))
          [enviro]
          (range 0 n-gens)))


(defn random-alive-enviro [width height alive-perc rand-gen]
    (let [enviro (create-dead-enviro width height)]
      (reduce
        (fn [acc i]
          (if (g/random-perc alive-perc rand-gen)
            (set-cell-at acc i :alive)
            acc))
        enviro
        (range 0 (count (:cells enviro))))))
