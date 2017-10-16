(ns game-of-life.color.seesaw.board
  (:require [seesaw.core :as sc]
            [seesaw.dev :as sd]
            [game-of-life.color.environment :as e])
  (:import (java.awt.event ActionEvent)
           (javax.swing.event ChangeEvent)))

(def running?! (atom true))

(defn main-loop-thread ^Thread [env-atom]
  (let [^Runnable main-loop
        #(while running?!
           (swap! env-atom e/advance-environment))]

    (Thread. main-loop)))

; TODO: Pass in handler so we don't need to reiterate later?
; TODO: Pass in the env cells and make the cells correct from the beginning.
(defn new-button-cells [width height]
  (for [y (range height)
        x (range width)]
    (sc/button :user-data [x y])))

(defn raw-cell-grid [env-atom]
  (let [{[w h] :dimensions :as env} @env-atom
        btns (new-button-cells w h)]

    (sc/grid-panel :columns w, :items btns)))

; TODO: Have the reset button take the settings, and
  ; create a new instance if resizing? Or just expand the grid?
(defn option-panel []
  (let [prog (sc/progress-bar :orientation :vertical,
                              :min 0, :max 100, :value 2)
        slid (sc/slider :orientation :horizontal
                        :min 0, :max 100, :value 2
                        :listen [:change
                                 #(sc/value! prog
                                    (sc/value (.getSource ^ChangeEvent %)))])
        btn (sc/button :test "Reset")]
    (sc/vertical-panel
      :items [prog slid])))


(defn main-panel [env-atom]
  (let [grid (raw-cell-grid env-atom)
        opt-panel (option-panel)]

    (sc/border-panel :center grid,
                     :east opt-panel)))

; TODO: Who will deal with starting the thread?
; TODO: Should creating the frame and starting the Thread be handled together?

(defn test-frame [env]
  (let [env-atom (atom env)
        panel (main-panel env-atom)
        f (sc/frame :content panel, :size [1000 :by 1000])]

    (-> f
        (sc/show!))))