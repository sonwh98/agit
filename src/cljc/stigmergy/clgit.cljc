(ns stigmergy.clgit
  (:require [stigmergy.io :as io]
            [clojure.java.io :as jio])
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

(defn read
  "read num-of-bytes from input-stream and return as a byte-array"
  [input-stream num-of-bytes]
  (let [bytes (byte-array num-of-bytes)]
    (.. input-stream (read bytes))
    bytes))

(defn parse-index [index-file]
  (let [in (-> index-file
               jio/file 
               jio/input-stream
               (java.io.DataInputStream.))
        sig (let [sig-array (read in 4)]
              (apply str (map (fn [c]
                                (char c))
                              sig-array)))
        version (.. in (readInt))
        num-of-entries (.. in (readInt))
        extension (byte-array 4)
        ]
    (prn "sig=" sig)
    (prn "version=" version)
    (prn "num=" num-of-entries)
    (.. in (read extension))
    (prn "extension=" (first extension))
    )
  )

(comment
  (parse-index "/tmp/test/.git/index")

  (.. (java.util.Base64/getEncoder)
      (encodeToString ))

  (hash-object "foobar\n")
  
  )
