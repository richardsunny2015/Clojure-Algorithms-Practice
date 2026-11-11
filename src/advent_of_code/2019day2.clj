(ns advent-of-code.2019day2
  (:require
    [clojure.java.io :as io]
    [clojure.string :as cstr]))

(defn parse-nums
  []
  (-> (mapv Integer/parseInt
            (-> (io/resource "2019_day2_input.txt")
                (slurp)
                (cstr/split #",")))
      (assoc 1 12)
      (assoc 2 2)))

(defn part-1
  []
  (let [parsed-nums (parse-nums)]
    (loop [nums parsed-nums
           instructions (partition-all 4 nums)]
      (if (empty? instructions)
        nums
        (let [[command i j k] (first instructions)
              x (nth nums i)
              y (nth nums j)
              result (case command
                       1 (+ x y)
                       2 (* x y)
                       99 nil)]
          (if (nil? result)
            nums
            (recur (assoc nums k result)
                   (rest instructions))))))))

(comment

  (first (part-1)))