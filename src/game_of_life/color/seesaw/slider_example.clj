(ns game-of-life.color.seesaw.slider-example
  (:require [seesaw.core :as sc])
  (:import (javax.swing.event ChangeEvent)))

(defn option-panel []
  (let [prog (sc/progress-bar :orientation :horizontal,
                              :min 0, :max 100, :value 2)
        slid (sc/slider :orientation :vertical
                        :min 0, :max 100, :value 2
                        :listen [:change
                                 #(sc/value! prog
                                             (sc/value (.getSource ^ChangeEvent %)))])]
    (sc/vertical-panel
      :items [prog slid])))