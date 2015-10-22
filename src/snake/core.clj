(ns snake.core
  (:gen-class)
  (:require [seesaw.core :as sc]
            [seesaw.graphics :as sg]))

;; Global parameters --------

(def key-map {java.awt.event.KeyEvent/VK_UP :up
             java.awt.event.KeyEvent/VK_DOWN :down
             java.awt.event.KeyEvent/VK_LEFT :left
             java.awt.event.KeyEvent/VK_RIGHT :right})

(def game-width 20)
(def game-height 20)
(def frame-title "Snake! Score: ")

;; Model -----------

(defn make-snake []
  {:body (list [4 1] [3 1] [2 1] [1 1])
   :dir :right
   :type :snake})

(defn make-apple []
  {:location [(rand-int game-width) (rand-int game-height)]
   :type :apple})

(def dir-map {:up [0 1]
              :down [0 -1]
              :left [-1 0]
              :right [1 0]})

(def opposite-dir {:up :down
                   :down :up
                   :left :right
                   :right :left})

(defn add-point [pt1 pt2]
  (map #(mod (+ %1 %2) %3)
       pt1 pt2 [game-width game-height]))

(defn move-snake [snake & grows]
  (let [body (:body snake)
        head (first body)
        dir  (:dir snake)]
    (assoc snake :body (cons (add-point head (dir-map dir))
                         (if grows body (butlast body))))))

(defn turn [snake newdir]
  (if-not (= (snake :dir) (opposite-dir newdir))
    (assoc snake :dir newdir)
    snake))

(defn head-in-body? [snake]
  (let [head (first (:body snake))
        body (:body snake)]
    (contains? (set (rest body)) head)))

(def lose? head-in-body?)

(defn eats? [snake apple]
  (= (first (:body snake))
     (:location apple)))


;; Mutator functions ------

(defn update-direction [snake newdir]
  (when newdir
    (dosync (alter snake turn newdir))))

(defn update-positions [snake apple]
  (dosync
   (if (eats? @snake @apple)
     (do (ref-set apple (make-apple))
         (alter snake move-snake :grow))
     (alter snake move-snake)))
  nil)

(defn restart-game [frame snake apple dir-atom score]
  (dosync (ref-set snake (make-snake))
          (ref-set apple (make-apple)))
  (reset! dir-atom nil)
  (reset! score 0)
  (sc/config! frame :title (str frame-title @score)))

;; GUI Paint functions -------------

(defmulti paint (fn [c g obj] (:type obj)))

(defmethod paint :apple [c g apple]
  (let [w (sc/width c)
        h (sc/height c)
        cellw (/ w game-width)
        cellh (/ h game-height)]
    (let [[x y] (:location apple)]
      (sg/draw g (sg/rect (* x cellw) (- (- h cellh) (* y cellh)) cellw cellh)
               (sg/style :background :red)))))

(defmethod paint :snake [c g snake]
  (let [w (sc/width c)
        h (sc/height c)
        cellw (/ w game-width)
        cellh (/ h game-height)]
    (doseq [[x y] (:body snake)]
      (sg/draw g (sg/rect (* x cellw) (- (- h cellh) (* y cellh)) cellw cellh)
               (sg/style :background :yellow)))))

(defn update-and-draw [frame snake apple dir-atom score]
  (update-direction snake @dir-atom)
  (reset! dir-atom nil)

  (when (eats? @snake @apple)
    (swap! score inc)
    (sc/config! frame :title (str frame-title @score)))

  (update-positions snake apple)

  (when (lose? @snake)
    (restart-game frame snake apple dir-atom score))

  (sc/repaint! frame))


;; Main function ---------

(defn main []
  (let [snake (ref (make-snake))
        apple (ref (make-apple))
        dir-atom (atom nil)
        score (atom 0)

        canvas (sc/canvas :id :canvas
                               :background :black
                               :paint (fn [c g]
                                        (paint c g @snake)
                                        (paint c g @apple)))

        panel (sc/vertical-panel :id :panel
                                 :items [canvas])

        f (sc/frame :title (str frame-title @score)
                    :size [400 :by 400]
                    :content panel
                    :id :frame
                    :listen [:key-pressed
                              (fn [e] (let [k (.getKeyCode e)]
                                        (reset! dir-atom (key-map k))))]
                    :on-close :exit)

        looper (sc/timer (fn [e] (update-and-draw f snake apple dir-atom score)) :delay 90 :start? false)]

    (sc/native!)
    (sc/show! f)
    (.start looper)))

(defn -main
  [& args]
  (main))
