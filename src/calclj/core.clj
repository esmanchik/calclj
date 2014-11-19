(ns calclj.core
  (:require [clojure.tools.trace])
  (:gen-class))

(def origsym
  {
   :lbp 0
   :nud (fn [this p] (throw (RuntimeException. "Undefined")))
   :led (fn [this left p] (throw (RuntimeException. "Missing operator")))
   })

(defn nud [this p]
  ((:nud this) this p))

(defn led [this left p]
  ((:led this) this left p))

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

(def ^:dynamic symtab (atom {}))

(comment defn defsym [tab id sym]
  (assoc tab id
    (merge origsym (assoc sym :id id))
    )
  )

(defn defsym [tab id sym]
  (swap! tab assoc id
    (merge origsym (assoc sym :id id))
    )
  )

(defn ^:dynamic advance [p]
  (let [n (next-token (:exp p))
        tab @symtab ;(:symtab p)
        t (:token n)
        rt (get tab t)
        lt (if rt rt
             (assoc (:literal tab) :val t))]
    (assoc p :exp (:exp n) :token lt)
    )
  )

(defn ^:dynamic result [p v]
  (assoc p :res (:val v) :ret v))

(defn ^:dynamic equal-priority [left rbp p]
  (let [n (advance p)
        token (:token n)]
    (if token
      (if (> (:lbp token) rbp)
        (let [l (led token left n)]
          (equal-priority (:ret l) rbp l)
          )
        (result p left)
        )
      (result p left)
      )
    )
  )

(defn ^:dynamic expression [rbp p]
  (let [n (advance p)
        t (:token n)
        r (nud t n)]
    (if-let [left (:ret r)]
      (equal-priority left rbp r)
      (result n 0))
    )
  )

(comment defn defparsym [p id sym]
  (let [tab (:symtab p)]
    (assoc p :symtab (defsym tab id sym))
    )
  )

(defn defparsym [p id sym]
  (defsym symtab id sym)
  )

(defn ^:dynamic prefix [p id nud]
  (defparsym p id {:nud nud})
  )

(defn ^:dynamic parenthesis [p]
  (prefix
   p "("
   (fn [this p]
     (let [r (expression 0 p)
           n (advance r)]
       (if (not= ")" (-> n :token :val))
         (throw (RuntimeException. ") expected"))
         n
         )
       )
     )
   )
  )

(defn ^:dynamic infix [p id bp]
  (defparsym p id
    {:val id
     :lbp bp
     :led
     (fn [this left p]
       (let [n (expression bp p)]
         (result n
                 {:val [(:val this) (:val left) (:res n)]}
                 )
         )
       )}))

(-> {}
     parenthesis
     (prefix :literal (fn [this p] (result p this)))
     (infix "+" 50)
     (infix "-" 50)
     (infix "*" 60)
     (infix "/" 60)
     )

(get @symtab "+")
(defn parse [p e]
  (:res
   (expression
    0 (assoc p :exp e)
    )
   )
  )

(clojure.tools.trace/dotrace
 [expression equal-priority]
 (parse {} [1 "+" 2]))

(clojure.tools.trace/dotrace
 [expression equal-priority]
 (parse {} [1 "+" 2 "*" 5 "+" 4]))

(clojure.tools.trace/dotrace
 [expression equal-priority]
 (parse {} [[1 "+" 2] "/" 3]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  )
