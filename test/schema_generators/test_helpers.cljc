(ns schema-generators.test-helpers
  (:require [schema.core :as s :include-macros true]
            [clojure.test.check.generators :as gen]))

;; We can test schema generation by generating random schemata,
;; generating them, and checking the result.

(def leaf-gen
  (gen/elements
   [s/Str
    s/Keyword
    ;; s/Any
    s/Inst
    s/Int
    s/Num
    s/Bool
    s/Symbol
    s/Uuid
    ;; s/Regex
    ]))

(def required-key-gen
  (gen/fmap
   s/required-key
   (gen/one-of
    [(gen/not-empty gen/string-ascii)
     gen/keyword])))

(def optional-key-gen
  (gen/fmap
   s/optional-key
   (gen/one-of
    [(gen/not-empty gen/string-ascii)
     gen/keyword])))

(def schema-key-gen
  (gen/one-of
   [required-key-gen
    optional-key-gen
    gen/keyword]))

(def branch-gen
  (fn [inner-gen]
    (gen/one-of
     [;; basic map schema
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
