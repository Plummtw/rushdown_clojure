(ns rushhour.car
  (:refer-clojure)
  (:require [clojure.string :as string]
            [rushhour.constants :as constants]))

(def ^:const H "H")
(def ^:const V "V")

(defrecord Car [id x y len dir])

(defn parse-car [car-str]
  (let [[id x y len dir] (string/split car-str #"\s+")]
    (->Car (parse-long id) (parse-long x) (parse-long y) (parse-long len) dir)))

(defn inside? [block car]
  ;; (println "inside?")
  ;; (println block)
  ;; (println car)
  ;; (println (= (:dir car) V))
  ;; (println (= (:x block) (:x car)))
  ;; (println (>= (:y block) (:y car)))
  ;; (println (< (:y block) (+ (:y car) (:len car))))
  (or
   (and
    (= (:dir car) V)
    (= (:x block) (:x car))
    (>= (:y block) (:y car))
    (< (:y block) (+ (:y car) (:len car))))
   (and
    (= (:dir car) H)
    (>= (:x block) (:x car))
    (< (:x block) (+ (:x car) (:len car)))
    (= (:y block) (:y car)))))

(defn block-valid? [block cars]
  (let [x (:x block)
        y (:y block)]
    (and (>= x 0)
         (< x constants/X)
         (>= y 0)
         (< y constants/Y)
         (not-any? #(inside? block %) cars))))

(defn can-move? [{car :car move :move} board]
  (let [x (:x car)
        y (:y car)
        car-dir (:dir car)
        cars (filter #(not= (:id car) (:id %)) (:cars board))]
    (case move
      :north (and (= car-dir V) (block-valid? (assoc car :y (dec y)) cars))
      :south (and (= car-dir V) (block-valid? (assoc car :y (+ y (:len car))) cars))
      :east  (and (= car-dir H) (block-valid? (assoc car :x (+ x (:len car))) cars))
      :west  (and (= car-dir H) (block-valid? (assoc car :x (dec x)) cars))
      false))); dir = V or H

(defn move [{car :car move :move}]
  (let [x (:x car)
        y (:y car)]
    (case move
      :north (assoc car :y (dec y))
      :south (assoc car :y (inc y))
      :east  (assoc car :x (inc x))
      :west  (assoc car :x (dec x))
      car)))

(defn car-all-moves [car]
  (map #(hash-map :move % :car car) [:north :south :east :west]))
