(ns game-of-life.edmonton.environment
  (:require [helpers.general-helpers :as g]
            [clojure.string :as s]))

(declare string-format-env)

(defrecord Enviroment [cells dimensions]
  Object
  (toString [self] (string-format-env self)))

(def cell-states #{::alive ::dead})

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

(defn random-cells [dimensions chance-of-life rand-gen]
  (let [[w h] dimensions
        len (* w h)]
    (vec
      (for [_ (range len)]
        (if (g/random-perc chance-of-life rand-gen)
          ::alive
          ::dead)))))

(defn state-at [env x y]
  ((:cells env) (env-index-of env x y)))

(defn alive-at? [env x y]
  (= (state-at env x y)
     ::alive))

(defn set-state-at [env x y new-state]
  (assoc-in env
            [:cells (env-index-of env x y)]
            new-state))

(defn coords-surrounding [env depth x y]
  (let [{[w h] :dimensions} env
        x-min (max 0 (- x depth)) ; TODO: Eww
        x-max (min w (inc (+ x depth))) ; Bound checks to ensure oob cells aren't checked
        y-min (max 0 (- y depth))
        y-max (min h (inc (+ y depth)))]
    (for [ry (range y-min y-max)
          rx (range x-min x-max)
          :when (not (and (= x rx) (= y ry)))]
      [rx ry])))

(defn cell-states-surrounding [env depth x y]
  (for [[rx ry] (coords-surrounding env depth x y)]
    (state-at env rx ry)))

(defn count-alive-neighbors [env x y]
  (-> env
      (cell-states-surrounding 1 x y)
      (frequencies)
      ::alive
      (or 0)))

(defn cell-should-live? [env x y]
  ; TODO: Add rule-set object to allow rule changes
  (let [n (count-alive-neighbors env x y)
        alive? (alive-at? env x y)]
    (or (and (not alive?) (= n 3))
        (and (<= 2 n 3) alive?))))

(defn advance-environment [env]
  (let [{[w h] :dimensions} env
        cells (for [y (range 0 h)
                    x (range 0 w)]
                [x y])]
    (reduce
      (fn [acc-env [x y]]
        (set-state-at acc-env x y
          (if (cell-should-live? env x y) ::alive ::dead)))
      env
      cells)))

(defn kill-all-cells [env]
  (update env :cells
    #(mapv (fn [_] ::dead) %)))

(defn random-environment [dimensions chance-of-life rand-gen]
  (->Enviroment
    (random-cells dimensions chance-of-life rand-gen)
    dimensions))

(defn string-format-env [env]
  (let [{cells :cells [w h] :dimensions} env
        neat-cells (map #(if (= % ::alive) \# \space) cells)
        part-cells (map #(s/join " " %) (partition w neat-cells))
        joined (s/join "\n" part-cells)]
    joined))

(def test-env
  (let [dims [25 25]
        a #(set-state-at % %2 %3 ::alive)
        env (->Enviroment
              (random-cells dims 0 (g/new-rand-gen))
              dims)]
    (-> env
        ; Blinker
        (a 2 2)
        (a 2 3)
        (a 2 4)

        ; Toad
        (a 3 7)
        (a 4 7)
        (a 5 7)
        (a 2 8)
        (a 3 8)
        (a 4 8)

        ; Glider
        (a 6 2)
        (a 7 2)
        (a 8 2)
        (a 8 1)
        (a 7 0)

        ; Blinker
        (a 9 9)
        (a 9 10)
        (a 10 9)
        (a 10 10)
        (a 11 11)
        (a 11 12)
        (a 12 11)
        (a 12 12))))