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
    (prn "header=" header)
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

(defn parse [index-file]
  (let [data (vd/sniff index-file)
        index [:signature [:char 4]
               :version :int32
               :entry-count :int32
               :entries :byte*]
        entry [:ctime-sec :int32
               :ctime-nsec :int32
               :mtime-sec :int32
               :mtime-nsec :int32
               :dev :int32
               :ino :int32
               :mode [:byte 4]
               :uid :int32
               :gid :int32
               :size :int32
               :sha1 [:byte 20]
               :flags :byte
               :name-len :byte
               :name :char*]
        index-pt (vd/pointer index data)
        entries (index-pt :entries)
        entry-pt (vd/pointer entry entries)]
    
    (doseq [i (-> :entry-count index-pt vd/bytes->int range)
            :let [name-len (vd/bytes->int (entry-pt :name-len))
                  file-name (take name-len (entry-pt :name))]]
      (prn "ctime=" (vd/bytes->int (entry-pt :ctime-sec)) (vd/bytes->int (entry-pt :ctime-nsec)))
      (prn "mtime=" (vd/bytes->int (entry-pt :mtime-sec)) (vd/bytes->int (entry-pt :mtime-nsec)))
      (prn "dev=" (vd/bytes->int (entry-pt :dev)))
      (prn "ino=" (vd/bytes->int (entry-pt :ino)))
      (prn "mode=" (vd/bytes->oct (entry-pt :mode)))
      (prn "uid=" (vd/bytes->int (entry-pt :uid)))
      (prn "gid=" (vd/bytes->int (entry-pt :gid)))
      (prn "size=" (vd/bytes->int (entry-pt :size)))
      (prn "sha1=" (outil/bytes->hex (entry-pt :sha1)))
      (prn "flags=" (entry-pt :flags))
      (prn "name-len=" name-len)
      (prn "name=" (vd/bytes->str file-name))      
      (entry-pt + :name name-len (padding name-len))
      (prn "------ " i)
      )))

(comment
  (parse-index "/tmp/test/.git/index")
  (parse "/tmp/test/.git/index")

  (def foo '&:name)
  (def a (atom 0))

  )
