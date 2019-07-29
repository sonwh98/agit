(ns stigmergy.clgit-test
  (:require [clojure.test :refer :all]
            [stigmergy.clgit :as git]
            [stigmergy.io :as io]
            [clojure.core.async :as a :include-macros true]))


(deftest clgit-tests
  (testing "init"
    (prn  (git/init))
    (prn  (git/init {:dir "cdr"})))
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
