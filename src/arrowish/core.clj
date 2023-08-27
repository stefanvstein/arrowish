(ns arrowish.core)

(defmacro ^{:private true} assert-args
  [& pairs]
  `(do (when-not ~(first pairs)
         (throw (IllegalArgumentException.
                  (str (first ~'&form) " requires " ~(second pairs) " in " ~'*ns* ":" (:line (meta ~'&form))))))
     ~(let [more (nnext pairs)]
        (when more
          (list* `assert-args more)))))

(defmacro ->as-when-let
   "a conditional construct to use within threading, 
    -> expressions that behaves as a when let. 
    The threaded value is also named in the expression,
    as first entry in name-and-binding. 
    The then form is in a do block where the names are 
    available. When the test is falsy, the ->as-when-let  
    expression is its first argument. 

    The ->as-when-let is meant to be used in -> expression
    Like e.g:

    (-> {:a 2}
     (->as-when-let [x a (:a x)]
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
  "Like ->as-when-let but with the treaded item as last, 
   so fitting in a ->> threading

   Technically the threaded value will piggyback as last on then-form."
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
  "Puts a name on the first argument, possibly destructured, as in a let. 
  The form is a do when name is available.  
  Usable in breaking out passed value in -> threading expression"
  {:clj-kondo/ignore [:unresolved-symbol]}
  [x x-name & form]
  `(let [~x-name ~x]
     ~@form))

(defmacro ->>as
  "Puts a name on the last argument, possibly destructured, as in a let. 
  Usable in breaking out passed value in ->> threading expression. Form-and-x
  contains the  content of a do, that can see the named imput. 
  Technically x will be added to form in form-and-x when used in ->>"
  {:clj-kondo/ignore [:unresolved-symbol]}
  [name & form-and-x]
  (let [x (last form-and-x)
        body (butlast form-and-x)]
    `(let [~name ~x]
       ~@body)))





(defmacro ->when-let
  "A conditional where the then-form is applied when the 
  when-let-like test-binding is truth. The value is applied 
  as first argument to both the test and the last expression 
  in the then-form. The value is passed along as is when the 
  test is opposed.

  The then body is a implicit do, but the first parameter 
  to ->when-let is provided as first parameter to both the 
  test and the last expression in then then body.
  
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
  The last parameter is provided as first in the test binding.
  
  Technically the threaded value will piggyback as last on then-form."
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







