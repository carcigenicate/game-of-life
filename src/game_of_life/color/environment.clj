(ns game-of-life.color.environment
  (:require [helpers.general-helpers :as g]
            [clojure.string :as s]))

(declare string-format-env)

(defrecord Enviroment [cells dimensions]
  Object
  (toString [self] (string-format-env self)))

(def dead-state ::dead)

(defn index-of [area-width x y]
  (+ (* y area-width)
     x))

(defn env-index-of [env x y]
  (let [w (-> env :dimensions (get 0))]
    (index-of w x y)))

(defn checked-index-of [area-dimensions x y]
  (let [[w h] area-dimensions]
    (if (and (<= 0 x w)
             (<= 0 y h))
      (index-of w x y)

      (throw (IndexOutOfBoundsException. (str "["x " " y
                                              "] out of bounds. Dimensions: ["
                                              w " " h "]"))))))

(defn random-cells [dimensions cell-states chance-of-life rand-gen]
  (let [[w h] dimensions
        len (* w h)]
    (vec
      (for [_ (range len)]
        (if (g/random-perc chance-of-life rand-gen)
          (g/random-from-collection cell-states rand-gen)
          dead-state)))))

(defn state-at [env x y]
  ((:cells env) (env-index-of env x y)))

(defn alive-at? [env x y]
  (not= (state-at env x y)
        dead-state))

(defn set-state-at [env x y new-state]
  (assoc-in env
            [:cells (env-index-of env x y)]
            new-state))

(defn coords-surrounding [env depth x y]
  (let [{[w h] :dimensions} env
        ; Bound checks to ensure oob cells aren't checked
        x-min (max 0 (- x depth)) ; TODO: Eww
        x-max (min w (inc (+ x depth)))
        y-min (max 0 (- y depth))
        y-max (min h (inc (+ y depth)))]
    (for [ry (range y-min y-max)
          rx (range x-min x-max)
          :when (not (and (= x rx) (= y ry)))]
      [rx ry])))

(defn wrap-dimension [dimension-length position]
  (g/wrap position 0 (dec dimension-length)))

(defn wrapping-coords-surrounding [env depth x y]
  (let [{[w h] :dimensions} env
        ; Bound checks to ensure oob cells aren't checked
        x-min (- x depth) ; TODO: Eww
        x-max (inc (+ x depth))
        y-min (- y depth)
        y-max (inc (+ y depth))]
    (for [ry (range y-min y-max)
          rx (range x-min x-max)
          :when (not (and (= x rx) (= y ry)))]
      [(wrap-dimension w rx)
       (wrap-dimension h ry)])))

(defn cell-states-surrounding [env depth x y]
    (for [[rx ry] (wrapping-coords-surrounding env depth x y)]
      (state-at env rx ry)))

(defn count-alive-neighbors [neighbors]
  (->> neighbors
      (remove #(= dead-state %))
      (count)))

(defn cell-should-live? [env neighbors x y]
  (let [n (count-alive-neighbors neighbors)
        alive? (alive-at? env x y)]
    (or (and (not alive?) (= n 3))
        (and (<= 2 n 3) alive?))))

(defn decide-child-state [neighbors]
  (let [state-freqs (frequencies neighbors)
        alive-freqs (dissoc state-freqs dead-state)]
    (key
      (apply max-key val alive-freqs))))

(defn advance-environment [env]
  (let [{[w h] :dimensions} env
        cells (for [y (range 0 h)
                    x (range 0 w)]
                [x y])]
    (reduce
      (fn [acc-env [x y]]
        (let [neighbors (cell-states-surrounding env 1 x y)]
          (set-state-at acc-env x y
            (if (cell-should-live? env neighbors x y)
              (decide-child-state neighbors)
              dead-state))))
      env
      cells)))

(defn kill-all-cells [env]
  (update env :cells
    #(mapv (constantly dead-state) %)))

(defn random-environment [cell-states dimensions chance-of-life rand-gen]
  (->Enviroment
    (random-cells dimensions cell-states chance-of-life rand-gen)
    dimensions))

(defn dead-environment [dimensions]
  (->Enviroment (vec (repeat (apply * dimensions) dead-state))
                dimensions))

(defn string-format-env [env]
  (let [{cells :cells [w] :dimensions} env
        neat-cells (map #(if (= % dead-state) " " %) cells)
        part-cells (map #(s/join " " %) (partition w neat-cells))
        joined (s/join "\n" part-cells)]
    joined))

(defn select-pop-env [dims position-map]
  (let [env (dead-environment dims)]
    (reduce
      (fn [e [[x y] cell-state]]
        (set-state-at e x y cell-state))
      env
      position-map)))

(defn select-pop-env-rand-states [dims cells-states rand-gen positions]
  (let [pos-map (reduce #(assoc % %2
                           (g/random-from-collection cells-states rand-gen))
                        {} positions)]
    (select-pop-env dims pos-map)))

(def simple-env
  (select-pop-env-rand-states [5 5] #{\A \B \C} (g/new-rand-gen 99)
    [[2 0]
     [0 1] [1 1] [2 1]
     [2 2]
     [1 3] [3 3] [4 3]
     [3 4]]))

(def struct-env
  (select-pop-env-rand-states [25 25] #{\A \B \C} (g/new-rand-gen 99)
    [; Blinker
     [2 2]
     [2 3]
     [2 4]

     ; Toad
     [3 7]
     [4 7]
     [5 7]
     [2 8]
     [3 8]
     [4 8]

     ; Glider
     [6 2]
     [7 2]
     [8 2]
     [8 1]
     [7 0]

     ; Beacon
     [9 9]
     [9 10]
     [10 9]
     [10 10]
     [11 11]
     [11 12]
     [12 11]
     [12 12]]))
