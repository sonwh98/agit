(ns stigmergy.clgit-test
  (:require [clojure.test :refer :all]
            [stigmergy.clgit :as git]
            [stigmergy.io :as io]
            [clojure.core.async :as a :include-macros true]))


(deftest clgit-tests
  (testing "init"
    (prn  (git/init {:dir "/tmp/cdr"})))

  (testing "hash-object"
    (is (= "e69de29bb2d1d6434b8b29ae775ad8c2e48c5391"
           (git/hash-object "")))
    
    (is (= "323fae03f4606ea9991df8befbb2fca795e648fa"
           (git/hash-object "foobar\n")))
    )
  )

(deftest io-tests
  (testing "mkdir"
    (a/go
      (prn "foobar=" (a/<! (io/mkdir "/tmp/foobar"))))
    )
  )

(comment
  (clojure.test/run-tests)

  )
