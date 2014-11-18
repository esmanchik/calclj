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


(defn next-token [e]
  (let [r (rest e)
        t (first e)
        ee (cond
            (sequential? t)
            (concat ["("] t [")"] r)
            :else e
            )]
    {:token (first ee)
     :exp (rest ee)}
    )
  )

(next-token [1 "+" 2])
(next-token [[1 "+" 2] "/" 3])

(defn ^:dynamic advance [p]
  (merge p (next-token (:exp p))))

(defn ^:dynamic result [p v] (assoc p :res v))

(defn ^:dynamic equal-priority [left token rbp p]
  (if token
    (if (> (lbp token) rbp)
      (let [l (led token left p)
            n (advance l)
            e (:exp l)]
        (equal-priority
         (:res l) (:token n) rbp n)
        )
      (result (assoc p :exp (cons token (:exp p))) left)
      )
    (result p left)
    )
  )

(defn ^:dynamic expression [rbp p]
  (let [n (advance p)
        nn (advance n)]
    (if-let [left (:token n)]
      (if-let [token (:token nn)]
        (equal-priority left token rbp nn)
        (result n left))
      (result n 0))
    )
  )

(defn ^:dynamic led [token left p]
  (let [r (expression (lbp token) p)]
    (result r [token left (:res r)])
    )
  )

(clojure.tools.trace/dotrace
 [expression equal-priority led]
  (expression
   0 {:exp [1 "+" 2]
      :res []}))

(clojure.tools.trace/dotrace
 [expression equal-priority led]
 (expression 0
             {:exp [1 "+" 2 "*" 5 "+" 4]
              :res []}))

(def p
  (clojure.tools.trace/dotrace
     [expression equal-priority led]
     (expression 0
                 {:exp [[1 "+" 2] "/" 3]
                  :res []}))
  )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (-> p :res println)
  )
