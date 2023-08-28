(ns arrowish.core
  "When there is an urge for conditional details in threading expressions")


(defmacro ^{:private true} assert-args
  [& pairs]
  `(do (when-not ~(first pairs)
         (throw (IllegalArgumentException.
                 (str (first ~'&form) " requires " ~(second pairs) " in " ~'*ns* ":" (:line (meta ~'&form))))))
       ~(let [more (nnext pairs)]
          (when more
            (list* `assert-args more)))))

(defmacro ->as-when-let
   "The then-form is applied when the when-let-like 
    test-binding is truth. The first parameter, value, 
    is available as name throughout both test-binding 
    and then-form. The value is passed along as is when 
    the test isn't truth.

    Both name and test-binding support destructuring. 
    Both test of test-binding, and the last expression in 
    then-form is put in a list, if they are not already 
    lists. The then-form is a implicit do.

    The ->as-when-let is meant to be used in -> expression, 
    where the threaded value is broken out and used freely 
    Like e.g:

    (-> {:a 2}
     (->as-when-let x [a (:a x)]
      (assoc x :b (* 3 a))))
    
    ..which would result in {:a 2 :b 6}
    
    The name of the test-form will shadow the name of value, 
    when same"
  
  {:clj-kondo/ignore [:unresolved-symbol]}
  [value name test-binding & then-form]
  (assert-args
   (and (vector? test-binding)
        (= 2 (count test-binding)))
   "test-binding is vector of 2, like in a when-let binding")
  (let [[r tst] test-binding
        name' name]
    `(let [value# ~value
           ~name' value# 
           r# ~tst]
       (if r#
         (let [~r r#]
           (do ~@then-form))
         value#))))


(defmacro ->>as-when-let
  "The then-form is applied when the when-let-like 
   test-binding is truth. The last parameter is 
   available as name throughout both test-binding 
   and then-form. The last parameter is passed along 
   as is when the test isn't truth.

   Both name and test-binding support destructuring.
   The then-form is a implicit do.

   The ->>as-when-let is meant to be used in ->> expression, 
   where the threaded value is broken out and used freely. 
   Like e.g:

   (->> {:a 2}
     (->>as-when-let x [a (:a x)]
       (assoc x :b (* 3 a))))
    
   ..which would result in {:a 2 :b 6}
    
   The name of the test-form will shadow the name of value, 
   when same.
   
   Like ->as-when-let but with the treaded value as last, 
   so fitting in a ->> threading. Technically the threaded 
   value will piggyback as last on then-form."
  {:clj-kondo/ignore [:unresolved-symbol]}
  [name test-binding & then-form]
  (assert-args
   (and (vector? test-binding)
        (= 2 (count test-binding)))
   "name-and-binding is vector of: name of first arg, and a when-let like binding")
  (let [x' name 
        [r tst] test-binding
        then (butlast then-form)
        x (last then-form)]
    `(let [x# ~x
           ~x' x#
           r# ~tst]
       (if r#
         (let [~r r#]
           (do ~@then))
         x#))))


(defmacro ->as
  "Bind value to name, possibly destructured, and available in form. 

   Usable in breaking out passed value in -> threading expression"
  {:clj-kondo/ignore [:unresolved-symbol]}
  [value name & form]
  `(let [~name ~value]
     ~@form))

(defmacro ->>as
  "Bind value of last expression in form to name, possibly 
   destructured, and available in form. 

   Usable in breaking out passed value in ->> threading expression. 
   Technically threaded value will be added to form."
  {:clj-kondo/ignore [:unresolved-symbol]}
  [name & form]
  (let [x (last form)
        body (butlast form)]
    `(let [~name ~x]
       ~@body)))





(defmacro ->when-let
  "The then-form is applied when the when-let-like 
   test-binding is truth. The value is applied as 
   first argument to both the test and the last 
   expression in the then-form. The value is passed 
   along as is when the test is not truth.

   The then-form is a implicit do where test binding 
   is available, but the first parameter to ->when-let 
   is provided as first parameter to the last expression.
  
   The test-binding can be destructured. 

   Both test of test-binding, and the last expression in 
   then-form is put in a list, if they are not already lists.

   Usuable as when-let in -> threading expression, where the
   threaded value is passed as first parameter. Like e.g.
  
  (-> {:a 1}
    (->when-let [a (get :a)] ;; or just [a :a]
       (assoc :b (* 3 a))))
  
  ..which would be: {:a 1 :b 3}"  
  
  {:clj-kondo/ignore [:unresolved-symbol :invalid-arity]}
  [value test-binding & then-form]
  (assert-args
   (and (vector? test-binding)
        (= 2 (count test-binding)))
   "test-binding is vector of 2, like in a when-let binding")

  (let [[r test] test-binding
        [tst test-args] (if (seq? test)
                          [(first test) (next test)]
                          [test])
        
        side-effects (butlast then-form)
        expression (last then-form)
        [then-fn then-args] (if (seq? expression)
                              [(first expression) (next expression)]
                              [expression])]
    `(let [value# ~value
           r# (~tst value# ~@test-args)]
       (if r#
         (let [~r r#]
           (do ~@side-effects
               (~then-fn value# ~@then-args)))
         value#))))





(defmacro ->>when-let
  "Like ->when-let but with the last parameter as 
   threaded value, to fit use in ->> expressions. 
   The last parameter is provided as first in the test 
   binding.
  
   The then-form is applied when the when-let-like 
   test-binding is truth. The last parameter is applied as 
   first argument to the test and as last parameter 
   for the last expression in the then-form. The value 
   is passed along as is when the test is not truth.

   The then-form is a implicit do where test binding 
   is available, but the last parameter to ->>when-let 
   is provided as last parameter to the last expression.
  
   The test-binding can be destructured. 

   Both test of test-binding, and the last expression in 
   then-form is put in a list, if they are not already lists.

   Usuable as when-let in ->> threading expression, where 
   the threaded value is passed as last parameter. Like e.g.
  
   (->> {:a 1}
     (->>when-let [a (get :a)] ;; or just [a :a]
       (assoc :b (* 3 a))))
  
   ..which would be: {:a 1 :b 3}

   Technically the threaded value will piggyback as last on 
   then-form."
  {:clj-kondo/ignore [:unresolved-symbol :invalid-arity]}
  [bindings & then-form]
  (assert-args
   (and (vector? bindings)
        (= 2 (count bindings)))
   "bindings is vector of 2, like when-let binding")

  (let [[r test] bindings
        [tst test-args] (if (seq? test)
                          [(first test) (next test)]
                          [test])
        value (last then-form)
        all-then (butlast then-form)
        then (last all-then)
        side-effects (butlast all-then) 
        [thn thn-args] (if (seq? then)
                         [(first then) (next then)]
                         [then])]

    `(let [value# ~value
           r# (~tst value# ~@test-args)]
       (if r#
         (let [~r r#]
           (do ~@side-effects
             (~thn ~@thn-args value#)))
         value#))))







