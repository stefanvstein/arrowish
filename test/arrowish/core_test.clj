(ns arrowish.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [arrowish.core :refer [->as ->when-let ->as-when-let ->>as ->>when-let ->>as-when-let]]))

;; These threading expression are more difficult than necesary to read
(deftest example
  (testing 
      "Threading expressions are good to make code easier to understand,
       by rearangeing the order of appearence. Sometimes it is tempting
       to use anonymous functions and sometimes it is tempting to embed
       some conditionality. Naturally you do this in stand alone functions,
       but sometimes there is an urge to express details inline.
       Threading expression is there to increase clarity, in first place.

       These threading expression are more difficult than necesary to read"

      (let [thing (atom 0)
            outer {:y 3}]
        (-> {:a 1 :b 2}
            (#(if-let [b (:b %)]
                (assoc % :b (+ 3 b))
                %))
            (cond-> (:y outer)
              (assoc :b (+ 2 (:y outer))))
            ((fn [item]
               (swap! thing inc)
               (is (= {:a 1 :b 5}
                      item)))))))
  
  (testing "These macros are here to increase the readbility"
    (let [thing (atom 0)
          outer {:y 3}]
      (-> {:a 1 :b 2}
          (->when-let [b :b] 
                      (assoc :b (+ 3 b)))
          (->as-when-let item [a (:y outer)]
                         (assoc item :b (+ 2 a)))
          (->as item
                (swap! thing inc)
                (is (= {:a 1 :b 5}
                       item)))))))


(deftest explain->as

  (testing
   "->as is used to bind a name to the passed in a -> expression 

       1 bound to x, which can be used in the succeding implicit do.
       The result of the implicit do form is passed along in the ->

       The -> expression is still just a -> expression.
       The sole inc expression the same as the one above it"
    (-> 1
        (->as x
              (inc x))
        inc
        (->as result
              (is (= 3 result)))))

  (testing
   "the binding can be a destructuring expression,
   and the implicit do can contain statements, obviously
   You are free to easily express advanced expressions directly
   within the -> expression"

    (-> {:a 1 :b 2}
        (->as {:keys [a] :as all}
              (is (= 1 a))
              all)
        (->as x
              (is (= {:a 1 :b 2}
                     x)))))

  (testing
  "The ->>as expression behaves like the ->as expression but binds
   a name to last argument, to fit usage in the ->> threading expression" 

   (->> {:a 1 :b 2}
        (->>as {:keys [a] :as all}
               (is (= 1 a))
               all))))

(deftest explain->as-when-let
  (testing "->as-when-let binds first to a name, here x, but does also form
    a when-let expression. The x will return without evaluating the then form
    if test is falsy. Otherwise, the second bind a will contains the truth value"

    (-> {:a 1 :b 2}
        (->as-when-let x
                       [ a (:a x)]
                       (is (= 1 a))
                       (inc a))
        (->as x
              (is (= 2 x)))))

  (testing "since there is no :c in the value, the exception won't be thrown")
  (-> {:a 1 :b 2}
      (->as-when-let x [c (:c x)]
                     (throw (ex-info "Should not occur. (:c x) should not be true"
                                     {:x x})))

      (->as x
            (is (= {:a 1 :b 2}
                   x))))

  (testing "Destructuring binds work for both the first value, and the test result"
    (-> {:a 1 :b 2}

        (->as-when-let {:keys [a] :as all}
                       [{c :c b :b} (assoc all :c 3)]
                       (is (= 1 a))
                       (is (= 2 b))
                       (is (= 3 c)))))

  (testing "->>as-when-let works as ->as-when-let, names the last parameter
            and then fits in a ->> expression"
    (->> {:a 1 :b 2}
       ;; 
       ;; 
         (->>as-when-let {:keys [a] :as all}
                         [{c :c b :b} (assoc all :c 3)]
                         (is (= 1 a))
                         (is (= 2 b))
                         (is (= 3 c))))))

(deftest explain->when-let
  (testing "->when-let is a conditional -> where the value is passed
            as is unless a when-let like test is truthy, in case the 
            value is passed as first argument to when expression instead.
            When a implicit do expression, with value passed as first to
            the last expression in the do.")
  (-> {:a 1 :b 2}
      (->when-let [a :a]
                  (assoc :c (+ a 2)))
      (->as x (is (= {:a 1 :b 2 :c 3} x))))

  (testing "->when-let will just pass forward when test is falsy. 
            There is no :c")
  (-> {:a 1 :b 2}
      (->when-let [c :c]
                  (->as x (throw (ex-info "Should not occur"
                                          {:x x}))))
      (->as x (is (= {:a 1 :b 2}
                     x))))
  (testing "->>when-let behaves the same but fits in a ->> expression
            the value is passed to the test as first parameter, but passed
            to the last when expression as last parameter, just like in a 
            ->> expression."
    (->> [1 2 3]
         (->>when-let [y (->as x (first (filter even? x)))]
                      (map #(+ y %)))
         (->>as x (is (= '(3 4 5) x)))))
  (testing "There are no even values in [1 3]
            So its passed as is")
  (->> [1 3]
       (->>when-let [y (->as x (first (filter even? x)))]
                    (->>as x (throw (ex-info "Should not occur" 
                                             {:y y :x x}))))
       (->>as x (is (= [1 3] x)))))


(deftest check->as
  (let [once (atom 0)]
    (-> ((fn [] (swap! once inc)
           1))
        (->as x (inc x))
        (->as x (inc x))
        (->as x (is (= 3 x))))

    (is (= 1 @once) "No double evaluation"))
  
  (is (= 1 (->as nil x 1))
      "No shortcutting when nil")
  (let [twice (atom 0)]
    (-> 1 (as-> _
                (swap! twice inc)
                (swap! twice inc)))
    (is (= 2 @twice) "A do block")))


(deftest check->as-when-let
    (let [only-once (atom 0)]
    (is (= ['(8 9) {:four 4 :five 5 :six 6}]
           (-> ((fn []
                  (swap! only-once inc)
                  (inc 1)))
               ;; 
               (->as-when-let item [one 1]
                              (+ one item))
                                        ;nil means passed through
               (->as-when-let passed [none nil]
                              (+ 100 passed))
               (->as-when-let passed [none ((constantly nil))]
                              (+ 100 passed))
               ;; so is for false
               (->as-when-let passed [none false]
                              (+ 100 passed))
               ;; both names can be used in then form, which is a do block
               (->as-when-let three [four ((fn []
                                             (swap! only-once inc) 
                                             (inc three)))]
                              
                              {:four four :five (inc four) :six (* 2 three)})
               ;;Both names can be destructured
               (->as-when-let {four :four :keys [five six] :as all}
                              [[seven & higher] [(inc six) 8 9]]
                              [higher all]))))
    (is (= 2 (deref only-once))))
  (let [only-once (atom 0)]
    (is (= ['(8 9) {:four 4 :five 5 :six 6}]
           (->> ((fn []
                   (swap! only-once inc)
                   (inc 1)))
                ;; 
                (->>as-when-let item [one 1]
                                (+ one item))
                                        ;nil means passed through
                (->>as-when-let passed [none nil]
                                (+ 100 passed))
                (->>as-when-let passed [none ((constantly nil))]
                                (+ 100 passed))
                ;; so is for false
                (->>as-when-let passed [none false]
                                (+ 100 passed))
                ;; both names can be used in then form, which is a do block
                
                (->>as-when-let three [four ((fn []
                                               (swap! only-once inc) 
                                               (inc three)))]
                                
                                {:four four :five (inc four) :six (* 2 three)})
                ;;Both names can be destructured
                (->>as-when-let {four :four :keys [five six] :as all}
                                 [[seven & higher] [(inc six) 8 9]]
                                [higher all]))))
    (is (= 2 (deref only-once)))))

(deftest check->when-let
  (testing "double eval"
    (let [only-once (atom 0)]
      (is (=  8
              (-> ((fn []
                     (swap! only-once inc)
                     (inc 1)))
                  (->when-let [a identity]
                              (+ a))
                  (->when-let [a (identity)]
                              (+ a)))))
      (is (= 1 @only-once))))
  (testing "double eval"
    (let [only-once (atom 0)]
      (is (=  8
              (->> ((fn []
                      (swap! only-once inc)
                      (inc 1)))
                   (->>when-let [a identity]
                                (+ a))
                   (->>when-let [a (identity)]
                                (+ a)))))
      (is (= 1 @only-once))))

  (testing "falsy"
    (is (= {:a 1}
           (-> {:a 1}
               (->when-let [a :b]
                           (->as x (throw (ex-info "Should not happen since falsy"))))))))
  (testing "extra argument"
    (is (= {:a 1 :c 6}
           (-> {:a 1}
               (->when-let [a (:b 3)]
                           (assoc :c (* a 2)))))))
  (testing "destructure"
    (is (= {:foo 3} (-> {:foo 3}
                  (->when-let [{:keys [foo] :as all} (identity)]
                              (->as  x
                                     (is (= 3 foo))
                                     (is (= {:foo 3} all))
                                     (is (= x all))
                                     x)))))))
