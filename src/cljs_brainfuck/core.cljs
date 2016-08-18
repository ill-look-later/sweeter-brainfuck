(ns cljs-brainfuck.core
  (:require [clojure.string :as str]))

(defn bf-loop [direction pointer commands]
  (let [val (if (= direction :forward) 1 -1)]
    (loop [count 1]
      (when-not (= count 0)
        (swap! pointer + val)
        (case (nth commands @pointer)
          \[ (recur (+ count val))
          \] (recur (- count val))
          (recur count))))))

(defn char-list-to-int [char-list]
  (js/parseInt (apply str char-list)))

(defn sugar [cell cells pointer commands]
  (loop [buf [], c (nth commands @pointer)]
    (case c
      (\0 \1 \2 \3 \4 \5 \6 \7 \8 \9) (do (swap! pointer inc)
                                          (recur (conj buf c) (nth commands @pointer)))
      \> (swap! cell + (char-list-to-int buf))
      \< (swap! cell - (char-list-to-int buf))
      \+ (swap! cells update @cell + (char-list-to-int buf))
      \- (swap! cells update @cell - (char-list-to-int buf))
      ())))

(defn interpret [commands input-func output-func]
  (let [cell  (atom 0)
        cells (atom (sorted-map))
        pointer (atom 0)]
    (loop []
      (case (nth commands @pointer)
        \> (swap! cell inc)
        \< (swap! cell dec)
        \+ (swap! cells update @cell inc)
        \- (swap! cells update @cell dec)
        (\0 \1 \2 \3 \4 \5 \6 \7 \8 \9) (sugar cell cells pointer commands)
        \. (output-func (get @cells @cell))
        \, (swap! cells assoc @cell (input-func))
        \[ (if (= (get @cells @cell) 0) (bf-loop :forward  pointer commands))
        \] (if-not (= (get @cells @cell) 0) (bf-loop :backward pointer commands))
        ())
      (swap! pointer inc)
      (if-not (= @pointer (count commands)) (recur)))))

(enable-console-print!)

(interpret "72+.>101+.>108+..>111+.>32+.>87+.2<.3>114+.4<.5>100+.>33+."
           (fn [] 0)
           (fn [cell] (prn (char cell))))
