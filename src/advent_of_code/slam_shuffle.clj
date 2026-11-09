(ns advent-of-code.slam-shuffle
  (:require [clojure.java.io :as io]
            [clojure.string :as cstr]))

(defn deal-new-stack
  [cards]
  (loop [old-stack cards
         new-stack (list)]
    (if (empty? old-stack)
      new-stack
      (recur (drop 1 old-stack)
             (conj new-stack (first old-stack))))))

(defn cut
  [n cards]
  (let [n (cond-> n
            (neg-int? n) (+ (count cards)))
        left (drop n cards)
        right (take n cards)]
    (concat left right)))

(defn- set-array-ind
  [a i v]
  (aset a i v)
  a)

(defn deal-with-inc
  [n cards]
  (let [l (count cards)]
    (loop [i 0
           old-stack cards
           new-stack (int-array l)]
      (if (empty? old-stack)
        (->> new-stack
             (into (list))
             (reverse))
        (recur (mod (+ i n) l)
               (drop 1 old-stack)
               (set-array-ind new-stack i (first old-stack)))))))

(defn- find-2019
  [cards]
  (loop [i 0]
    (if (= (nth cards i) 2019)
      i
      (recur (inc i)))))

(defn- parse-n
  [line]
  (-> line
      (cstr/split #" ")
      (last)
      (Integer/parseInt)))

(defn shuffle-factory
  []
  (with-open [r (io/reader "resources/slam_shuffle_input")]
    (loop [lines (line-seq r)
           cards (range 10007)]
      (if (empty? lines)
        (find-2019 cards)
        (recur
         (drop 1 lines)
         (cond
           ;; starts with what?
           (cstr/starts-with? (first lines) "cut")
           (cut (parse-n (first lines)) cards)

           (cstr/starts-with? (first lines) "deal with increment")
           (deal-with-inc (parse-n (first lines)) cards)

           (cstr/starts-with? (first lines) "deal into new stack")
           (deal-new-stack cards)))))))