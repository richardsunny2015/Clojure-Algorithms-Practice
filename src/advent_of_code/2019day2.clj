(ns advent-of-code.2019day2
  (:require
    [clojure.java.io :as io]
    [clojure.string :as cstr]))

(defn parse-nums
  []
  (mapv Integer/parseInt
        (-> (io/resource "2019_day2_input.txt")
            (slurp)
            (cstr/split #","))))

(defn- calculate
  [parsed-nums]
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
                 (rest instructions)))))))

(defn part-1
  []
  (let [parsed-nums (-> (parse-nums)
                        (assoc 1 12)
                        (assoc 2 2))]
    (first (calculate parsed-nums))))

(defn noun-verb
  [noun verb]
  (+ (* 100 noun) verb))

(defn part-2
  []
  (let [pairs (for [x (range 0 100)
                    y (range 0 100)]
                (list x y))
        parsed-nums (parse-nums)]
    (->> pairs
         (keep
          (fn [[x y]]
            (let [nums (-> parsed-nums
                           (assoc 1 x)
                           (assoc 2 y))
                  result (first (calculate nums))]
              (when (= 19690720 result)
                [x y]))))
         (first)
         (apply noun-verb))))

(comment

  (part-1)
  (part-2)

  )