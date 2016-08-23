(ns cljs-brainfuck.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [clojure.string :as str]
            [cljs.core.async :refer [chan <! >! timeout alts!]]
            [reagent.core :as r]))

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

(defn make-cell-div [index value highlight?]
  ^{:key index} ; http://stackoverflow.com/a/33458370
  [(if highlight? :div.cell.highlight :div.cell)
   [:div.value value] [:div.index index]])

(defn make-cells-div [cells cell]
  [:div#cells-box
   (loop [elms [], cells cells, prev-i -1]
     (if-let [c (first cells)]
       (let [[i v] c
             zero-elms (for [i (range (inc prev-i) i)]
                         (make-cell-div i 0 (= i cell)))
             elm (make-cell-div i v (= i cell))
             elms (concat elms zero-elms [elm])]
         (recur elms (next cells) i))
       elms))])

(enable-console-print!)

(def cells-div (r/atom (make-cells-div {10 0} 0)))
(def commands (r/atom "72+.>101+.>108+..>111+.>32+.>87+.2<.3>114+.4<.5>100+.>33+."))
(def user-input (r/atom ""))
(def output (r/atom ""))
(def interpret-delay (r/atom 0))
(def cell (atom 0))
(def cells (atom (sorted-map)))
(def pointer (atom 0))
(def status (atom :done))
(def stop-chan (chan))

(defn interpret-one-step [commands input-func output-func]
  (case (nth @commands @pointer)
    \> (swap! cell inc)
    \< (swap! cell dec)
    \+ (swap! cells update @cell inc)
    \- (swap! cells update @cell dec)
    (\0 \1 \2 \3 \4 \5 \6 \7 \8 \9) (sugar cell cells pointer @commands)
    \. (output-func (get @cells @cell))
    \, (swap! cells assoc @cell (input-func))
    \[ (when (= (get @cells @cell) 0) (bf-loop :forward  pointer @commands))
    \] (when-not (= (get @cells @cell) 0) (bf-loop :backward pointer @commands))
    ()))

(defn interpret [commands input-func output-func]
  (go-loop []
    (let [[v ch] (alts! [(timeout @interpret-delay)
                         stop-chan])]
      (case ch
        stop-chan nil ; break loop
        (do (interpret-one-step commands input-func output-func)
            (reset! cells-div (make-cells-div @cells @cell))
            (swap! pointer inc)
            (if-not (>= @pointer (count @commands))
              (recur)
              (reset! status :done)))))))

(defn body []
  [:div.box
   @cells-div
   [:textarea#source {:on-change #(reset! commands (-> % .-target .-value))
                      :value @commands}]
   [:div.actions
    [:input {:type "range" :step 30 :min 0 :max 300 :value @interpret-delay
             :on-change #(reset! interpret-delay (js/parseInt (-> % .-target .-value)))}]
    [:button#run
     {:on-click #(do (when (or (>= @pointer (count @commands)) (= @status :done))
                       (reset! output "")
                       (reset! cell 0)
                       (reset! cells (sorted-map))
                       (reset! pointer 0))
                     (interpret commands
                                (fn []
                                  (let [code (.charCodeAt @user-input 0)]
                                    (reset! user-input (apply str (next @user-input)))
                                    code))
                                (fn [cell]
                                  (swap! output str (char cell)))))}
     "Run"]]
   [:textarea#input {:on-change #(reset! user-input (-> % .-target .-value))
                     :value @user-input}]
   [:div#output [:pre @output]]])

(r/render-component [body]
                    (.-body js/document))
