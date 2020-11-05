(defn vectorsEqual? [args]
  (every? (partial == (count (first args))) (mapv count args))
  )

(defn Vector? [v]
  (and (vector? v) (every? number? v))
  )

(defn Matrix? [m]
  (and (vector? m) (every? Vector? m) (vectorsEqual? m))
  )

(defn vectorFunction [f args]
  (apply mapv f args)
  )

(defn v+ [& args]
  {
   :pre  [(every? Vector? args) (vectorsEqual? args)]
   :post [(vector? %) (vectorsEqual? (list % (first args)))]
   }
  (vectorFunction + args)
  )

(defn v- [& args]
  {
   :pre  [(every? Vector? args) (vectorsEqual? args)]
   :post [(vector? %) (vectorsEqual? (list % (first args)))]
   }
  (vectorFunction - args)
  )

(defn v* [& args]
  {
   :pre  [(every? Vector? args) (vectorsEqual? args)]
   :post [(vector? %) (vectorsEqual? (list % (first args)))]
   }
  (vectorFunction * args)
  )

(defn v*s [v & args]
  {
   :pre  [(Vector? v) (every? number? args)]
   :post [(vector? %) (vectorsEqual? (list % v))]
   }
  (reduce (fn [v x] (mapv (partial * x) v)) v args)
  )

(defn scalar [a b]
  {
   :pre  [(Vector? a) (Vector? b) (vectorsEqual? [a b])]
   :post [number? %]
   }
  (reduce + (v* a b))
  )

(defn vectCord [a b x y]
  (- (* (nth a x) (nth b y)) (* (nth a y) (nth b x)))
  )

(defn vect [& args]
  {
   :pre  [(every? Vector? args) (every? (fn [x] (== (count x) 3)) args)]
   :post [Vector? %]
   }
  (reduce (fn [a b]
            (as-> (partial vectCord a b) coordfn
              (vector (coordfn 1 2) (coordfn 2 0) (coordfn 0 1))
              )
            )
          args))

(defn m+ [& args]
  {
   :pre  [(every? Matrix? args) (vectorsEqual? args)]
   :post [(Matrix? %) (vectorsEqual? (list % (first args)))]
   }
  (vectorFunction v+ args)
  )

(defn m- [& args]
  {
   :pre  [(every? Matrix? args) (vectorsEqual? args)]
   :post [(Matrix? %) (vectorsEqual? (list % (first args)))]
   }
  (vectorFunction v- args)
  )

(defn m* [& args]
  {
   :pre  [(every? Matrix? args) (vectorsEqual? args)]
   :post [(Matrix? %) (vectorsEqual? (list % (first args)))]
   }
  (vectorFunction v* args)
  )

(defn m*s [m & args]
  {
   :pre  [(Matrix? m) (every? number? args)]
   :post [(Matrix? %) (vectorsEqual? (list % m))]
   }
  {:pre [(Matrix? m) (every? number? args)]}
  (reduce (fn [m s] (mapv (fn [v] (v*s v s)) m)) m args)
  )

(defn m*v [m v]
  {
   :pre  [(Matrix? m) (Vector? v)]
   :post [(Vector? %)]
   }
  (mapv (fn [a] (reduce + (v* a v))) m)
  )

(defn transpose [m]
  {
   :pre  [(Matrix? m)]
   :post [(Matrix? %)]
   }
  (apply mapv vector m)
  )

(defn checkMatrix [firstV restV]
  (if (== (count (peek firstV)) (count (first restV)))
    (recur (first restV) (rest restV))
    (if (== (count restV) 0)
      true
      false)
    )
  )

(defn m*m [& args]
  {
   :pre  [(every? Matrix? args) (checkMatrix (first args) (rest args))]
   :post [(Matrix? %)]
   }
  (reduce (fn [a b] (mapv (partial m*v (transpose b)) a)) args)
  )