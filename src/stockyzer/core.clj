(ns stockyzer.core
  (:use
     [clojure.contrib.monads]
     [clojure.contrib.prxml]
     [stockyzer.monad-parser]
     [conduit.core])
  (:import
     java.text.DateFormat
     [java.io FileWriter FileReader BufferedReader])
  (:require
     [conduit.require :as conduit]))

;; http://ichart.finance.yahoo.com/table.csv?s=SPY&d=10&e=14&f=2010&g=d&a=0&b=29&c=1993&ignore=.csv

(def candle-width 8)
(def candle-space 4)

(defn csv-data [stock-sym]
  (->> (str stock-sym ".csv")
       (FileReader.)
       (BufferedReader.)
       (line-seq)
       (map parse-csv)
       (remove nil?)))

(def max-price
  (conduit/pipe
    (conduit/proc :high)
    (conduit/reduce
      (conduit/proc (partial apply max))
      0)))

(def min-price
  (conduit/pipe
    (conduit/proc :low)
    (conduit/reduce
      (conduit/proc (partial apply min))
      100000)))

(def find-min-max
  (conduit/split
    max-price
    min-price))

(defn round [num]
	  (Math/round (double num)))

(conduit/def candlestick [{:keys [bar-index open close low high
                                  scale-price]}]
   (let [bottom (min open close)
         top (max open close)
         [color fill] (if (> open close)
                        ["red" "red"]
                        ["green" "black"])
         left-edge (* (+ candle-width candle-space)
                      bar-index)
         height (- (scale-price bottom)
                   (scale-price top))
         wick (dec (+ left-edge (round (/ candle-width 2))))
         wick-style (str "stroke:" color ";stroke-width:2")
         style (str "fill:" fill ";stroke-width:2;stroke:" color)]
     [:g
      [:line {:x1 wick :y1 (scale-price low)
              :x2 wick :y2 (scale-price high)
              :style wick-style}]
      [:rect {:x left-edge :y (scale-price top)
              :width candle-width :height height
              :style style}]]))

(defn accumulate [period]
  (a-loop
    (a-arr (fn [[xs x]]
             (if (< (count xs) period)
               (conj xs x)
               (conj (subvec xs 1 period) x))))
    []))

(def sum (a-arr (partial apply +)))

(defn moving-avg [period value-fn color]
  (a-comp
    (a-all
      (a-comp (a-arr value-fn)
              (accumulate period)
              sum
              (a-arr #(/ % period))
              (accumulate 2))
      pass-through)
    (a-arr (fn [[[y1 y2]
                 {:keys [bar-index scale-price]}]]
             (when (> bar-index period)
               (let [x1 (* (+ candle-width candle-space) bar-index)
                     y1 (scale-price y1)
                     x2 (+ x1 candle-width candle-space)
                     y2 (scale-price y2)]
                 [:line {:x1 x1 :y1 y1 :x2 x2 :y2 y2
                         :style (str "stroke:" color ";stroke-width:2")}]))))))


(def make-chart 
  (a-all
    (moving-avg 25 :close "yellow")
    candlestick
    ))

(defn chart [stock-sym days chart-height]
  (let [bars (->> (csv-data stock-sym)
                  (take days)
                  (sort-by :date)
                  (map-indexed #(assoc %2 :bar-index %1)))
        [max-price min-price] (last (conduit/map find-min-max bars))
        y-scale (/ (* -1 (- chart-height 40))
                   (- max-price min-price))
        y-offset (* y-scale max-price)
        scale-price #(- (* % y-scale) y-offset -20)]
    (binding [*out* (FileWriter. (str stock-sym ".svg"))
              *prxml-indent* 3]
      (println
        (with-out-str
          (prxml [:decl! "version=\"1.0\" encoding=\"UTF-8\""]
                 \newline
                 [:doctype! "svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\""]
                 [:svg {:xmlns "http://www.w3.org/2000/svg"
                        :height chart-height
                        :width (* days (+ candle-width candle-space))}
                  [:rect {:height "100%" :width "100%" :fill "black"}]
                  (conduit/map make-chart
                               (map #(assoc %
                                            :scale-price scale-price)
                                    bars))]))))))

(chart "spy" 600 650)
