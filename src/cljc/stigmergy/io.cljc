(ns stigmergy.io
  (:require [taoensso.timbre :as log :include-macros true]
            [clojure.core.async :as a :include-macros true]
            [clojure.java.io :as jio])
  (:import  [java.nio ByteBuffer]))


(defn mkdir
  ([path options cb]
   #?(:clj (let [file (clojure.java.io/file path)]
             (if-not (.. file mkdirs)
               (cb (ex-info (str "cannot create file " path) {:path path
                                                              :options options
                                                              :cb cb})))
             ))
   )
  
  ([path options]
   (let [c (a/chan 2)]
     (mkdir path options (fn [ex]
                           (a/put! c ex)))
     c))
  ([path]
   (mkdir path {})))

(defn mkdirSync
  ([path options]
   #?(:clj (let [file (clojure.java.io/file path)]
             (.. file mkdirs))))

  ([path]
   (mkdirSync path {})))

(defn writeFile
  ([filepath data opts cb]
   #?(:clj (spit filepath data)))

  ([filepath data]
   (writeFile filepath data {} nil)
   )
  )

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

(defn parse-index [index-file]
  (let [in (-> index-file
               jio/file 
               jio/input-stream
               (java.io.DataInputStream.))
        sig (let [sig-array (byte-array 4)]
              (.. in (read sig-array))
              (apply str (map (fn [c]
                                (char c))
                              sig-array)))
        version (.. in (readInt))
        num-of-entries (.. in (readInt))
        sha1-buf (byte-array 20)
        total 137
        remain (- total 4 4 4 20)
        buf (byte-array remain)
        ]

    (prn "sig=" sig)
    (prn "version=" version)
    (prn "num=" num-of-entries)
    (prn "remain=" remain)
    (prn "sha1-buf-count=" (.. in (read sha1-buf)))
    (prn "sha1-buf=" sha1-buf)
    (prn "read-buf-count=" (.. in (read buf)))
    (prn "buf=" buf)
    )
  )

(comment
  (parse-index "/tmp/manny/.git/index")

  (.. (java.util.Base64/getEncoder)
      (encodeToString ))

  (hash-object "foobar\n")
  
  )
