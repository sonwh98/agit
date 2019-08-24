(ns stigmergy.clgit
  (:require [stigmergy.io :as io]
            [clojure.java.io :as jio]
            [octet.core :as buf]
            [octet.spec :as spec]
            [octet.util :as outil])
  (:import [java.nio ByteBuffer]
           [org.apache.commons.codec.binary Hex]
           [org.apache.commons.codec.digest DigestUtils]))

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

(defn bytes->hex-str [bytes]
  (Hex/encodeHexString bytes))

(defn hex-str->bytes [hex-str]
  (Hex/decodeHex hex-str))

(defn sha1-as-bytes [data]
  (DigestUtils/sha1 data))

(defn hash-object [data]
  (let [size (count data)
        git-str (str "blob " size "\0" data)]
    (-> git-str
        sha1-as-bytes
        bytes->hex-str)))

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

(defn read-bytes
  "read num-of-bytes from input-stream and return as a byte-array"
  [input-stream num-of-bytes]
  (let [bytes (byte-array num-of-bytes)]
    (.. input-stream (read bytes))
    bytes))

(defn sniff
  "like slurp but returns raw bytes"
  [file-name]
  (let [paths (rest (clojure.string/split file-name #"/"))
        root-dir (str "/" (first paths))
        path (java.nio.file.Paths/get root-dir (into-array (rest paths)))]
    (java.nio.file.Files/readAllBytes path)))

(defn bytes->int [bytes]
  (let [base 10
        bytes (reverse bytes)]
    (reduce + (map-indexed (fn [index b]
                             (int (* (Math/pow base index) b)))
                           bytes))))

(defn bytes->str [bytes]
  (clojure.string/join "" (map (fn [c]
                                 (char (max 0, c)))
                               bytes)))

(defn take-between [i j coll]
  (let [chunk (drop i coll)
        num (- j i)]
    (take num chunk)))

(defn padding [n]
  (let [floor (quot (- n 2) 8)
        target (+ (* (inc floor) 8)
                  2)]
    (- target n)))

(defn parse-index [index-file]
  (let [file-bytes (sniff index-file)
        header-size 12
        header (take-between 0 header-size file-bytes)
        num-of-entries  (bytes->int (take-last 4 header))
        entry-pt (atom header-size)]

    (doseq [i (range num-of-entries)
            :let [sha-start (+ @entry-pt (* 4 10))
                  sha-end (+ sha-start 20)
                  sha1 (take-between  sha-start sha-end
                                      file-bytes)
                  name-len-start sha-end
                  name-len-end (+ name-len-start 2)
                  name-len (take-between name-len-start name-len-end file-bytes)
                  name-len (bytes->int name-len)
                  name-start name-len-end
                  name-end (+ name-start name-len)
                  name (take-between name-start name-end file-bytes)
                  name (bytes->str name)]
            ]
      (prn (outil/bytes->hex sha1) name)
      (reset! entry-pt (+ name-end (padding name-len)))
      )))

(comment
  (parse-index "/tmp/test2/.git/index")


  (def index (sniff "/tmp/test/.git/index"))

  (def header-size 12)
  (def entry-size 64)

  (def header (take header-size index))
  
  (def entry (take entry-size (drop header-size index)))
  (def sha-start 40)
  (def sha1 (vec (drop-last 4 (drop sha-start entry))))

  (outil/bytes->hex sha1)
  (def foo (drop sha-start entry))

  (def last-4 (vec (take-last 4 (drop sha-start entry))))


  (def index2 (sniff "/tmp/test2/.git/index"))

  (def entries (drop header-size  index2))
  (def entry (take entry-size entries))
  (def file-size-start (* 9 4))
  (def file-size (vec (take 4 (drop file-size-start entry))))
  (def sha-start 40)
  (def sha1-size 20)
  (def sha1 (vec (take sha1-size (drop sha-start entry))))

  (def len-name (drop (+ sha-start sha1-size) entry))
  (outil/bytes->hex sha1)
  (map char (take-between 74 81 index2))
  (take-between (- sha-start 4) (inc sha-start) entry)
  
  (outil/bytes->hex (take-between 52 (+ 52 20) index2))
  (take-between (+ 53 20) (+ 53 21) index2)
  
  

  

  )
