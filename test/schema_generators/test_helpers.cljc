(ns schema-generators.test-helpers
  "Provides generators that make valid schemata."
  (:require [schema.core :as s :include-macros true]
            [clojure.test.check.generators :as gen :include-macros true]))

(def ^:private POS_INFINITY #?(:clj Double/POSITIVE_INFINITY, :cljs (.-POSITIVE_INFINITY js/Number)))
(def ^:private NEG_INFINITY #?(:clj Double/NEGATIVE_INFINITY, :cljs (.-NEGATIVE_INFINITY js/Number)))
(def ^:private NAN #?(:clj Double/NaN, :cljs (.-NaN js/Number)))


(def safe-any
  (gen/such-that
   (fn [x]
     (and (not= x POS_INFINITY)
          (not= x NEG_INFINITY)
          (not= x NAN)))
   gen/simple-type-printable
   100))

(def name-gen
  (gen/one-of
   [(gen/not-empty gen/string-ascii)
    gen/keyword]))

(def eq-gen
  (gen/fmap
   s/eq
   safe-any))

(def enum-gen
  (gen/fmap
   s/enum
   (gen/vector safe-any 2 6)))



(def leaf-gen
  (gen/frequency
   [[9 (gen/elements
        [s/Str
         s/Bool
         s/Num
         s/Int
         s/Keyword
         s/Symbol
         ;; s/Regex
         s/Inst
         s/Uuid
         s/Any

         ])]
    [1 eq-gen]
    [1 enum-gen]]))

(def required-key-gen
  (gen/fmap
   s/required-key
   name-gen))

(def optional-key-gen
  (gen/fmap
   s/optional-key
   name-gen))

(def schema-key-gen
  (gen/one-of
   [required-key-gen
    optional-key-gen
    gen/keyword]))

(def branch-gen
  (fn [inner-gen]
    (gen/one-of
     [;; maybe schema
      (gen/fmap
       s/maybe
       inner-gen)

      ;; named schema
      (gen/fmap
       (fn [[s n]]
         (s/named s n))
       (gen/tuple
        inner-gen
        name-gen))

      ;; cond-pre.. doesn't work yet..
      #_(gen/fmap
       (partial apply s/cond-pre)
       (gen/vector-distinct
        inner-gen
        {:min-elements 2 :max-elements 6}))

      ;; constrained... fails as tries are locked at 10
      #_(gen/let [inner inner-gen]
        (s/constrained inner (complement nil?)))

      ;; if
      (gen/let [inner inner-gen]
        (s/if integer?
          s/Int
          s/Str))

      ;; atom? doesn't seem to work..
      #_(gen/let [anyval gen/any]
        (s/atom anyval))

      ;; queue
      (gen/fmap
       s/queue
       inner-gen)

      ;; pair
      (gen/fmap
       (partial apply s/pair)
       (gen/tuple
        inner-gen
        name-gen
        inner-gen
        name-gen))

      ;; set
      (gen/let [inner inner-gen]
        #{inner})

      ;; basic map schema
      (gen/fmap
       (fn [m]
         (let [non-specific-keys (->> m keys (remove s/specific-key?))]
           (if (< (count non-specific-keys) 2)
             m
             (apply dissoc m (rest non-specific-keys)))))
       (gen/map (gen/one-of
                 [schema-key-gen
                  inner-gen]) inner-gen))

      ;; seq schema
      ;; optional currently broken, see https://github.com/plumatic/schema/pull/342
      (gen/fmap
         (fn [[n one one-k optional optional-k all]]
           (into [] (take n [(s/one one one-k)
                             ;; (s/optional optional optional-k)
                             all])))
         (gen/tuple
          (gen/large-integer* {:min 0 :max 2 ;; 3
                               })
          inner-gen
          gen/keyword
          inner-gen
          gen/keyword
          inner-gen))])))

(def schema-gen
  (gen/recursive-gen
   branch-gen
   leaf-gen))
