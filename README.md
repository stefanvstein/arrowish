# arrowish

Clojure macros when there is an urge for conditional details in threading expressions.  

## Usage

Code can be easier to understand when rearranged in threading macros. Sometimes there is a urge to express conditionals up front, inline with the threading expression, rather than hidden away in stand alone functions. 

Expressions like this are more difficult to read than necessary, with complex inline lambdas and less obvious usage of other constructs. 

```clj
(let [outer {:foreign 3}]
      (-> {:apple 1 
           :banana 2}
         (#(if-let [banana (:banana %)]
              (assoc % :banana (+ 3 banana))
               %))
         (cond-> (:foreign outer)
            (assoc :banana (+ 2 (:foreign outer))))
         ((fn [item]
             (is (= {:apple 1 :banana 5}
                    item))
             item))))
```

Macros like: `->as`, `->as-when-let` and `->when-let` can increase readability by breaking out of the treading and introduce simple conditionality.

```clj
(let [outer {:foreign 3}]
    (-> {:apple 1 
         :banana 2}
      (->when-let [banana :banana] 
         (assoc :banana (+ 3 banana)))
      (->as-when-let fruits [another (:foreign outer)]
         (assoc fruits :banana (+ 2 another)))
      (->as item
         (is (= {:apple 1 :banana 5}
                item))
         item)))
```

where:
- `->when-let` is a when-let-like expression that fits in -> expressions
- `->when-let-as` is a value broken out, when-let-like expressions that first in -> expressions
-  `->as` is a value broken out expression that fits in -> expressions

## `->when-let [value test-binding & then-form]`

A conditional where the `then-form` is applied when the when-let-like `test-binding` is truth. The threaded `value` is applied as first argument to both the `test` and last expression in the `then-form`. The `value` is passed along as is when the `test` is opposed.

In the extract from above: 

```clj
(-> {:apple 1 
     :banana 2}
  (->when-let [banana :banana] 
     (assoc :banana (+ 3 banana))))
```

.. the value of :banana will increase by 3 if there happen to be :banana in the threaded value. Keyword `:banana` is applied to the value in the test. The result is in `banana`, used by name when calculating the new :banana. The assoc is applied with the threaded value as first argument. If there were no :banana, the test failed, the fruity value would be passed along as is, as its original value {:apple 1 :banana 2}`.

This should be contrasted with:

```clj
(-> {:apple 1 
     :banana 2}
   (#(if-let [banana (:banana %)]
        (assoc % :banana (+ 3 banana))
        %)))
```

..where we used an immediately called lambda, and an `if-let`, rather than `when-let`, as we need to provide something when the `test-binding` is not truth.

The test is a function application, so `:cherry`, `(:cherry)` and `(get :cherry)` is the same in code below. The `test-binding` is simply a `let` that support destructuring.
```clj
(-> {:apple 1 
     :banana 2}
    (->when-let [cherry (get :cherry)]
                (println "There is no cherry so this is just weird")
                (#(throw (ex-info (str "Should no occur!")
                                  {:fruit %}))))
    (assoc :cherry {:black 3
                    :red 4})
    (->when-let [{:keys [black red yellow] :as all} :cherry]
   	        (println (str "Removing " (or black 0) " black cherries from " all))
                (update :cherry dissoc :black)))
```
The result is `{:apple 1, :banana 2, :cherry {:red 4}}`

The `->>when-let` behaves the same as `->when-let` but fits in a `->>` expression. The `value` is passed to the test as first parameter, and to the last `then-form` expression as last parameter, just like in a `->>` expression.

```clj
(->> [1 2 3]
     (->>when-let [y (->> (filter even?) 
                          first)]
                  (map #(+ y %)))
     (= '(3 4 5) ))
```

There are no even values in `[1 3]`, test fails, so its passed as is

```clj
(->> [1 3]
     (->>when-let [y (->> (filter even?) 
                           first)]
                  (map #(+ y %))
     (= [1 3])
```

Both snippets above is true.
 
## `->as-when-let [value name test-binding & then-form]`
A conditional where the `then-form` is applied when the when-let-like `test-binding` is truth. The threaded `value` is applied as first argument to both the `test` and last expression in the `then-form`. The value is available as name throughout both `test-binding` and `then-form`. The `value` is passed along as is when the `test` is opposed.

In the extract from above: 

```clj
(let [outer {:foreign 3}]
  (-> {:apple 1 
       :banana 2}
      (->as-when-let fruits [another (:foreign outer)]
         (assoc fruits :banana (+ 2 another)))))
```

.. the `value` of :banana will increase by 2 if there happen to be :foreign in the `outer` value. The `value` is broken out of the threading expression, as `fruits`, and made available for both the test and `then-form` by its `name`.

The expression would equal to the value: {:apple 1 :banana 2}, if there were no :foreign in outer.

Compared to `->when-let`, the `value` is not applied automatically to neither test nor `then-form` in `->as-when-let`. The value is applied manually at will.

This should be contrasted with:

```clj
(let [outer {:foreign 3}]
  (-> {:apple 1 
       :banana 2}
      (cond-> (:foreign outer)
        (assoc :banana (+ 2 (:foreign outer))))))
```

... where `cond->` is cleverly utilized to perform a conditional on the outer value. Here though, the test `(:foreign outer)`, is conducted twice as the value is not directly available in the then-form.

This is a bit silly example as the whole expression would be better as a cond-> expression, but it was extracted from the top, where different conditionals steps are used:

```clj
(let [outer {:foreign 3}]
    (-> {:apple 1 
         :banana 2}
      (->when-let [banana :banana] 
         (assoc :banana (+ 3 banana)))
      (->as-when-let fruits [another (:foreign outer)]
         (assoc fruits :banana (+ 2 another)))))
```


```
Destructuring binds work for both the first value, and the test result, as shown in:

```clj
(-> {:a 1 :b 2}
    (->as-when-let [{:keys [a] :as all}
                    {c :c b :b} (assoc all :c 3)]
      (is (= 1 a))
      (is (= 2 b))
      (is (= 3 c))))
```
..where `a` and `all` is destructured from the value, and `c` and `b` is extracted from the truth of `(assoc all :c 3)`. Where `all` is the `{:a 1 :b 2}` value.

`->>as-when-let` works as `->as-when-let`, except that it names the value of the last parameter and then naturally fits in a `->>` expression. Like:

```clj
(->> {:a 1 :b 2}
     (->>as-when-let {:keys [a] :as all}
                     [{c :c b :b} (assoc all :c 3)]
       (is (= 1 a))
       (is (= 2 b))
       (is (= 3 c))))
```  
The value of the two last expressions is true, as a result of their last is expression, of the then-form.

## `->as [value name & form]`
->as is used to bind a name to the passed value in a -> expression, and make it available in the do form.

In the example from above:
 
```clj
(-> {:apple 1 
     :banana 2}
    (->as item
       (is (= {:apple 1 :banana 2}
              item))
       item))
```

..the threaded value, {:apple 1 :banana 2}, is named item and made available for checks with the is-expression. The last expression, item, is the value getting passed out to the threading expression.

This should be contrasted with:

```clj
(let [outer {:foreign 3}]
      (-> {:apple 1 
           :banana 2}
         ((fn [item]
             (is (= {:apple 1 :banana 2}
                    item))
             item))))
```
where the value is broken out and named using a direct call to a lambda.

The name binding is a let expression, that support destructured name.

```clj
(-> {:apple 1 :banana 2}
    (->as {:keys [apple] :as all}
          (is (= 1 apple))
          all))
```


The ->>as expression behaves like the ->as expression but binds a name to last argument, to fit usage in the ->> threading expression.

```clj
(->> {:apple 1 :banana 2}
     (->>as {:keys [apple] :as all}
            (is (= 1 apple))
            all))
```

## License

Copyright © 2023 Stefan von Stein

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
