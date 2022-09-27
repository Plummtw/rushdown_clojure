(ns rushhour.board
  (:refer-clojure)
  (:require [rushhour.car :as car]))

(def global-history (atom {}))

(defrecord Board [cars history next-move])

(defn parse-cars [cars-str]
  (->Board (mapv car/parse-car cars-str)
           []
           nil))

(defn board-all-moves [board]
  (let [moves (map car/car-all-moves (:cars board))]
    (flatten moves)))

(defn board-valid-moves [board]
  (let [moves (board-all-moves board)]
    (filter #(car/can-move? % board) moves)))

(defn moves-to-boards [board moves]
  (map #(->Board (:cars board) (:history board) %) moves))

(defn find-car-by-id [cars id]
  (let [cars (map-indexed vector cars)
        car-found (filter #(= (:id (second %)) id) cars)]
    (when (not-empty car-found)
      (first car-found))))

(defn apply-car [board car]
  (let [car-found (find-car-by-id (:cars board) (:id car))]
    (when car-found
      ;(println car-found)
      (let [index (first car-found)]
        ;(println index)
        ;(println car)
        (update-in board [:cars index] (constantly car))))))

(defn apply-move [board full-move]
  (let [board-updated-car (apply-car board (car/move full-move))
        board-updated-history (update-in board-updated-car [:history] conj full-move)]
    board-updated-history))

(defn push-all [list-a list-b]
  (loop [list-a list-a
         [head-b & rest-b] list-b]
    (if head-b
      (recur (cons head-b list-a) rest-b)
      list-a)))

(defn generate-new-boards [board]
  (let [moves (board-valid-moves board)
        boards (moves-to-boards board moves)]
    ;; (println (count (:moves board)))
    boards))

(defn dfs [board limit]
  (reset! global-history {(:cars board) 0})
  (let [boards (generate-new-boards board)]
    (loop [boards boards]
      (let [[board & rest-board] boards
            move (:next-move board)]
        (if (empty? board) ; no new move
          nil
          ;else
          (let [new-board (apply-move board move)
                cars (:cars new-board)
                count-history (count (:history new-board))]
            ;; (println move)
            ;; (println new-board)
            ;; (println (contains? @global-history (:cars new-board)))
            (if (-> (find-car-by-id cars 0) second :x (= 4)) ; found
              new-board
            ;else
              (if (and (contains? @global-history cars)
                       (>= count-history (get @global-history cars))) ; history contains
                (recur rest-board)
              ;else
                (do
                  (dosync (swap! global-history assoc cars count-history))
                ;; (println (count (:history board)))
                  (if (>= count-history limit) ; over limit
                    (recur rest-board)
                  ;else
                    (recur (push-all rest-board (generate-new-boards new-board)))))))))))))

(defn dfs-to-limit [board limit]
  (loop [current-limit 0]
    (if (> current-limit limit) nil
        (let [result (dfs board current-limit)]
          (if result
            result
            (recur (inc current-limit)))))))

(defn key-str [key]
  (.substring (.toUpperCase (str key)) 1))

(comment
  (parse-cars ["0 1 2 2 H"])

  (println (format "%s" (.toUpperCase (str :a))))

  ; Case 0
  (def a (parse-cars ["0 1 2 2 H" "1 4 2 2 V"]))
  (dfs a 4)

  (contains? {:a :b} :a)

  (def a (parse-cars ["0 1 2 2 H"
                      "1 4 4 2 H"
                      "12 5 0 3 V"
                      "13 3 1 3 V"
                      "14 2 5 3 H"
                      "15 0 1 3 V"
                      "2 0 0 2 H"
                      "3 0 4 2 V"]))

  (def a (parse-cars (clojure.string/split-lines
"0 0 2 2 H
1 0 4 2 V
10 3 1 2 V
11 2 5 2 H
13 3 0 3 H
14 5 2 3 V
3 1 4 2 V
4 0 3 2 H
6 2 2 2 V
7 2 0 2 V
8 3 3 2 H
9 2 4 2 H" )))
  (dfs a 100)

  (dfs a 6)

  (repeatedly 3 read-line)

  (def result (dfs-to-limit a 100))

  result

  (time (dfs-to-limit a 100))

  (count (:history result))

  (:history result)

  (rest [0 1 2 3])

  (def a (parse-cars ["0 1 2 2 H" "3 4 3 2 V" "13 0 4 3 H"]))

  (dfs a 4)

  (key-str :a)

  @global-history

  (car/block-valid? {:x 4 :y 2} (:cars a))

  (car/can-move? {:car {:id 0, :x 2, :y 3, :len 2, :dir "H"}, :move :east} a)

  (-> (find-car-by-id (:cars a) 0))

  (+ 2 2)

  (def moves (board-all-moves a))

  moves

  (car/can-move? (first moves) a)

  (board-valid-moves a)

  (def moved (map car/move (board-valid-moves a)))

  moved

  @global-history

  (map #(apply-move a %) moved)

  (contains? #{1 2 3} 1)

  a
  (generate-new-boards a)

  (def b (map-indexed vector (:cars a)))
  b

  (filter #(= (:id %) 0) b)

  (apply-car a {:id 0 :x 2 :y 2 :len 2 :dir "H"})

  (get-in a [:cars 0])

  (not-empty '(10))

  (true? nil)

  (when nil (println 'a))

  (push-all '(1 2 3) '(4 5 6))

  a

  @global-history

  (update-in [1 2 3] [0] (constantly 3))

  (= {:a 1 :b 2 :c [0 1 2]} {:a 1 :b 2 :c '(0 1 2)}))


