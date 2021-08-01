(ns sketches.yessir
  (:require [quil.core :as q]
            [quil.middleware :as middleware]))

(def body (.-body js/document))
(def w (.-clientWidth body))
(def h (.-clientHeight body))
(def buffer 0.2)

(def MAX_CRAZY 10)

(def matrix_space (- 1 (* buffer 2)))
(def matrix_deets
  {:rows 3
   :columns 3})

(defn get_matrix_coordinates [id]
  [(mod id (:columns matrix_deets))
   (Math/floor (/ id (:rows matrix_deets)))])

(defn get_init_coordinates [id]
  (let [[x y] (get_matrix_coordinates id)]
    [(+ (* w buffer) (* matrix_space w (/ x (:columns matrix_deets))))
     (+ (* h buffer) (* matrix_space h (/ y (:rows matrix_deets))))]))

(defn particle
  "creates a single particle within the matrix"
  [id]
  (let [[x y] (get_init_coordinates id)]
    (println x y)
    {:id id
     :crazy 1
     :vx 0
     :vy 0
     :direction 0
     :size 40
     :x x
     :y y}))

(defn sketch-setup
  "returns the initial state"
  []
  (map particle (range 0 (* (:rows matrix_deets) (:columns matrix_deets)))))


(defn craziness [c]
  (min MAX_CRAZY
       (if (< (rand-int 2) 1)
         (* c 0.9)
         (* c 1.11))))

(defn velocity
  "Calculates the next velocity by averaging the current velocity and the added delta."
  [current delta crazy]
  (/ (+ current (* delta (/ crazy MAX_CRAZY))) 2))

(defn direction
  "Calculates the next direction based on the previous position and id of each particle."
  [c]
  (* 2
     Math/PI
     (* c (rand))))

(defn position
  "Calculates the next position based on the current, the speed and a max."
  [current delta max]
  (mod (+ current delta) max))

(defn sketch-update [particles]
  (map (fn [p]
        ;;  (println (:vx p) (:vy p))
         (assoc p
                :x         (position (:x p) (:vx p) w)
                :y         (position (:y p) (:vy p) h)
                :vx        (velocity (:vx p) (Math/cos (:direction p)) (:crazy p))
                :vy        (velocity (:vy p) (Math/sin (:direction p)) (:crazy p))
                :direction (direction (:crazy p))
                :crazy     (craziness (:crazy p)))) particles))


(defn sketch-draw [particles]
  (q/background 30)
  (doseq [p particles]
    (q/ellipse (:x p) (:y p) (:size p) (:size p))))


(defn create [canvas]
  (q/sketch
   :host canvas
   :size [w h]
   :draw #'sketch-draw
   :setup #'sketch-setup
   :update #'sketch-update
   :middleware [middleware/fun-mode]
   :settings (fn []
               (q/random-seed 666)
               (q/noise-seed 666))))

(defonce sketch (create "sketch"))