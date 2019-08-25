(ns stigmergy.clgit
  (:require [stigmergy.io :as io]
            [clojure.java.io :as jio]
            [octet.core :as buf]
            [octet.spec :as spec]
            [octet.util :as outil]
            [stigmergy.voodoo :as vd])
  (:import [java.nio ByteBuffer]
           [org.apache.commons.codec.binary Hex]
           [org.apache.commons.codec.digest DigestUtils])  )

(defn init
  ([{:keys [dir]}]
   (let [git-dir ".git"
         mk-path (fn [file-or-dir]
                   (let [path (str git-dir "/" file-or-dir)]
                     (if dir
                       (str dir "/" path)
                       path)))
         folders ["hooks" "info" "objects/info" "objects/pack" "refs/heads" "refs/tags"]
         folders (map mk-path 
                      folders)]
     (doseq [dir folders]
       (io/mkdir dir))

     (let [config-file (mk-path "config")
           config-data (str "[core]\n"
                            "\trepositoryformatversion = 0\n"
                            "\tfilemode = false\n"
                            "\tbare = false\n"
                            "\tlogallrefupdates = true\n")
           head-file (mk-path "HEAD")
           head-data "ref: refs/heads/master"]
       (io/writeFile config-file config-data)
       (io/writeFile head-file head-data))))
  ([]
   (init {}))
  
  )

(defn hash-object [data]
  (let [size (count data)
        git-str (str "blob " size "\0" data)]
    (-> git-str
        vd/sha1-as-bytes
        vd/bytes->hex-str)))

(defrecord Entry [ctime-sec
                  ctime-nsec
                  mtime-sec
                  mtime-nsec
                  dev
                  ino
                  mode
                  uid
                  gid
                  size
                  sha1
                  name-len
                  name])

(defn padding [n]
  (let [floor (quot (- n 2) 8)
        target (+ (* (inc floor) 8)
                  2)]
    (- target n)))

(defn parse-index [index-file]
  (let [file-bytes (vd/sniff index-file)
        header-size 12
        header (vd/take-between 0 header-size file-bytes)
        num-of-entries  (vd/bytes->int (take-last 4 header))
        entry-pt (atom header-size)]

    (doseq [i (range num-of-entries)
            :let [sha-start (+ @entry-pt (* 4 10))
                  sha-end (+ sha-start 20)
                  sha1 (vd/take-between  sha-start sha-end
                                         file-bytes)
                  name-len-start sha-end
                  name-len-end (+ name-len-start 2)
                  name-len (vd/take-between name-len-start name-len-end file-bytes)
                  name-len (vd/bytes->int name-len)
                  name-start name-len-end
                  name-end (+ name-start name-len)
                  name (vd/take-between name-start name-end file-bytes)
                  name (vd/bytes->str name)]]
      (prn (outil/bytes->hex sha1) name)
      (reset! entry-pt (+ name-end (padding name-len))))))

(comment
  (parse-index "/tmp/test/.git/index")

  
  (let [data (vd/sniff "/tmp/test/.git/index")
        header [:signature [:char 4]
                :version :int32
                :entry-count :int32]
        pt (vd/pointer data header)]

    (pt :entry-count)
    )
  )
