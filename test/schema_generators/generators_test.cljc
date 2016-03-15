(ns schema-generators.generators-test
  #?(:clj (:use clojure.test))
  (:require
   #?(:cljs [cljs.test :refer-macros [deftest is testing run-tests]])
   [clojure.test.check :refer [quick-check]]
   [clojure.test.check.properties :as properties :include-macros true]
   [clojure.test.check.generators :as check-generators]
   [clojure.test.check.clojure-test #?@(:clj [:refer [defspec]]
                                        :cljs [:refer-macros [defspec]])]
   [schema.core :as s :include-macros true]
   [schema-generators.test-helpers :refer [schema-gen]]
   [schema-generators.generators :as generators]))

(def OGInner
  {(s/required-key "l") [s/Int]
   s/Keyword s/Str})

(def OGInner2
  {:c OGInner
   :d s/Str})

(def OGSchema
  {:a [s/Str]
   :b OGInner2})

(def FinalSchema
  {:a (s/eq ["bob"])
   :b {:c (s/conditional (fn [{:strs [l]}] (and (every? even? l) (seq l))) OGInner)
       :d (s/eq "mary")}})

(deftest sample-test
  (let [res (generators/sample
             20 OGSchema
             {[s/Str] (generators/always ["bob"])
              s/Int ((generators/fmap #(inc (* % 2))) check-generators/int)}
             {[s/Int] (comp (generators/such-that seq)
                            (generators/fmap (partial mapv inc)))
              OGInner2 (generators/merged {:d "mary"})})]
    (is (= (count res) 20))
    (is (s/validate [FinalSchema] res))))

(deftest simple-leaf-generators-smoke-test
  (doseq [leaf-schema [#?@(:clj [double float long int short char byte boolean
                                 Double Float Long Integer Short Character Byte Boolean
                                 doubles floats longs ints shorts chars bytes booleans
                                 String Object]
                           :cljs [js/Object])
                       s/Str s/Bool s/Num s/Int s/Keyword s/Symbol s/Inst
                       s/Any s/Uuid (s/eq "foo") (s/enum :a :b :c)]]
      (testing (str leaf-schema)
        (is (= 10 (count (generators/sample 10 leaf-schema)))))))

(defspec spec-test
  50
  (properties/for-all [x (generators/generator OGSchema)]
                      (not (s/check OGSchema x))))

(defspec random-spec-test
  50
  (properties/for-all
   [rand-schema schema-gen]
   (= nil
      (s/check generators/Schema rand-schema)
      (s/check rand-schema (generators/generate rand-schema)))))
