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

(defn cell+ [cells cell n]
  (swap! cells update @cell + n))

(defn cell- [cells cell n]
  (swap! cells update @cell - n))

(defn cell> [cells cell n]
  (swap! cell + n)
  (when-not (get @cells @cell)
    (swap! cells assoc @cell 0)))

(defn cell< [cells cell n]
  (swap! cell - n)
  (when-not (get @cells @cell)
    (swap! cells assoc @cell 0)))

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

(defn is-number? [char]
  (let [code (.charCodeAt char 0)]
    (and (>= code 48) (<= code 57))))

(defn make-source-div [commands pointer]
  (let [begin (loop [n pointer]
                (if (is-number? (nth commands (dec n)))
                  (recur (dec n))
                  n))
        end (inc pointer)]
    [:pre#source
     (subs commands 0 begin)
     [:span (subs commands begin end)]
     (subs commands end)]))

(enable-console-print!)

(def cells-div (r/atom (make-cells-div {10 0} 0)))
(def commands
  (r/atom (str "三种方式说: \"Hello World!\"\n\n"
               "原版:\n"
               "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]"
               ">>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.\n\n"
               "读输入: 2>\n"
               "13+[>,.<-]\n\n"
               "糖: 3>\n"
               "72+.>101+.>108+..>111+.>32+.>87+.2<.3>114+.4<.5>100+.>33+.")))
(def pointer (atom 0))
(def source-div (r/atom (make-source-div "" 0)))
(def user-input (r/atom "Hello World!\n"))
(def output (r/atom ""))
(def interpret-delay (r/atom 0))
(def status (r/atom :stop))
(def cell (atom 0))
(def cells (atom (sorted-map)))
(def break-chan (chan))

(defn reinit []
  (reset! output "")
  (reset! cell 0)
  (reset! cells (sorted-map))
  (reset! pointer 0))

(defn interpret-one-step [commands input-func output-func]
  (case (nth commands @pointer)
    \> (cell> cells cell 1)
    \< (cell< cells cell 1)
    \+ (cell+ cells cell 1)
    \- (cell- cells cell 1)
    (\0 \1 \2 \3 \4 \5 \6 \7 \8 \9)
    (loop [buf [], c (nth commands @pointer)]
      (case c
        (\0 \1 \2 \3 \4 \5 \6 \7 \8 \9)
        (do (swap! pointer inc)
            (recur (conj buf c) (nth commands @pointer)))
        \> (cell> cells cell (char-list-to-int buf))
        \< (cell< cells cell (char-list-to-int buf))
        \+ (cell+ cells cell (char-list-to-int buf))
        \- (cell- cells cell (char-list-to-int buf))
        (interpret-one-step commands input-func output-func)))
    \. (output-func (get @cells @cell))
    \, (swap! cells assoc @cell (input-func))
    \[ (when (= (get @cells @cell) 0) (bf-loop :forward  pointer commands))
    \] (when-not (= (get @cells @cell) 0) (bf-loop :backward pointer commands))
    (do (swap! pointer inc)
        (when-not (>= @pointer (count commands))
          (recur commands input-func output-func)))))

(defn interpret [commands input-func output-func]
  (reset! status :running)
  (go-loop []
    (interpret-one-step commands input-func output-func)
    (reset! cells-div (make-cells-div @cells @cell))
    (reset! source-div (make-source-div commands @pointer))
    (swap! pointer inc)
    (let [[v ch] (alts! [(timeout @interpret-delay)
                         break-chan])]
      (when-not (= ch break-chan)
        (if-not (>= @pointer (count commands))
          (recur)
          (reset! status :stop))))))

(defn body []
  [:div.box
   @cells-div
   (if (= @status :stop)
     [:pre#source {:on-input #(reset! commands (-> % .-target .-innerText))
                   :content-editable true}
      @commands]
     @source-div)
   [:div.actions
    [:input {:type "range" :step 30 :min 0 :max 900 :value @interpret-delay
             :on-change #(reset! interpret-delay (js/parseInt (-> % .-target .-value)))}]
    (if (= @status :running)
      [:div
       [:button {:on-click #(go (>! break-chan true)
                                (reset! status :pause))}
        "Pause"]
       [:button {:on-click #(go (>! break-chan true)
                                (reset! status :stop))}
        "Stop"]]
      [:button
       {:on-click #(do (when (= @status :stop) (reinit))
                       (interpret @commands
                              (fn []
                                (let [code (.charCodeAt @user-input 0)]
                                  (reset! user-input (apply str (next @user-input)))
                                  code))
                              (fn [cell]
                                (swap! output str (char cell)))))}
       (if (= @status :pause)
         "Continue"
         "Run")])]
   [:pre#input {:on-input #(reset! user-input (-> % .-target .-innerText))
                :content-editable true}
    @user-input]
   [:div#output [:pre @output]]])

(r/render-component [body]
                    (.-body js/document))
