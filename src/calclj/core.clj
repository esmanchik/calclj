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

(defn ^:dynamic unadvance [p token]
  (assoc p
    :token token
    :exp (cons (:token p) (:exp p))
    )
  )

(defn ^:dynamic result [p v] (assoc p :res v))

(defn ^:dynamic equal-priority [left rbp p]
  (if-let [token (:token p)]
    (if (> (lbp token) rbp)
      (let [l (led token left p)
            n (advance l)
            e (:exp l)]
        (equal-priority (:res l) rbp n)
        )
      (-> p (result left) (unadvance token))
      )
    (result p left)
    )
  )

(defn ^:dynamic expression [rbp p]
  (let [n (advance p)]
    (if-let [left (:token n)]
      (equal-priority left rbp (advance n))
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

(def p
  (clojure.tools.trace/dotrace
   [expression equal-priority led]
   (expression 0
               {:exp [1 "+" 2 "*" 5 "+" 4]
                :res []}))
  )


(comment clojure.tools.trace/dotrace
 [expression equal-priority led]
 (expression 0
             {:exp [[1 "+" 2] "/" 3]
              :res []}))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (-> p :res println)
  )
