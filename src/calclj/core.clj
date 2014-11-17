(ns calclj.core
  (:require [clojure.tools.trace])
  (:gen-class))

(declare led)

(defn lbp [token]
  (if-let
    [p (get  {"+" 50
              "-" 50
              "*" 60
              "/" 60}
         token)]
    p
    (if (number? token) 10 0)
    ))


(defn ^:dynamic result [p left]
  (assoc p :res left))

(defn ^:dynamic equal-priority [left token rbp p]
  (if token
    (if (> (lbp token) rbp)
      (let [l (led token left p)
            e (:exp l)]
        (equal-priority
         (:res l) (first e) rbp
         (assoc p :exp (rest e)))
       )
      {:res left :exp (cons token (:exp p))})
    (assoc p :res left))
  )

(defn ^:dynamic expression [rbp p]
  (if-let [left (-> p :exp first)]
    (if-let [r (-> p :exp rest)]
      (equal-priority left (first r) rbp
                      (assoc p :exp (rest r)))
      {:res left :exp []})
    {:res 0 :exp []})
  )

(defn ^:dynamic led [token left p]
  (let [r (expression (lbp token) p)]
    (assoc r :res
      [token left (:res r)]
      )
    )
  )

(clojure.tools.trace/dotrace
 [expression equal-priority led]
  (expression
   0 {:exp [1 "+" 2]
      :res []}))

(def p
  (clojure.tools.trace/dotrace
     [expression equal-priority led]
     (expression 0
                 {:exp [1 "+" 2 "*" 5 "+" 4]
                  :res []}))
  )

;(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (-> p :res println)
  )
