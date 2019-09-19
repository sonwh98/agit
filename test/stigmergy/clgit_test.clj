(ns stigmergy.clgit-test
  (:require [clojure.test :refer :all]
            [stigmergy.clgit :as git]
            [stigmergy.io :as io]
            [stigmergy.voodoo :as vd]
            [clojure.core.async :as a :include-macros true]))


(deftest clgit-tests
  (testing "init"
    (prn  (git/init {:dir "/tmp/cdr"})))

  (testing "hash-object str content"
    (is (= "e69de29bb2d1d6434b8b29ae775ad8c2e48c5391"
           (git/hash-object "")))
    
    (is (= "323fae03f4606ea9991df8befbb2fca795e648fa"
           (git/hash-object "foobar\n")))
    
    (is (= "d670460b4b4aece5915caf5c68d12f560a9fe3e4"
           (git/hash-object "test content\n"))))

  (testing "hash-object binary content"
    (let [cwd (System/getProperty "user.dir")
          jpg-file (str cwd "/test/lambda.jpg")
          jpg-content (io/suck jpg-file)]
      (is (= "0a881c05c740e89bb192dac0d85877512cdc2d67"
             (git/hash-object jpg-content)))))

  (testing "wrap/unwrap content"
    (let [content "abc"
          git-object (git/wrap "blob" content)]
      (is (= '(98 108 111 98 32 51 0 97 98 99)
             git-object))

      (is (= content
             (vd/seq->str (git/unwrap git-object)))))))

(deftest io-tests
  (testing "mkdir"
    (a/go
      (prn "foobar=" (a/<! (io/mkdir "/tmp/foobar"))))
    )

  (testing "compress/decompress"
    (let [txt  "hello 12345"]
      (is (= txt
             (-> txt io/compress io/decompress vd/seq->str))))
    )
  )

(comment
  (clojure.test/run-tests)

  )
