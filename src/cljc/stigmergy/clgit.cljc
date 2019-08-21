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

(defn parse-index [index-file]
  (let [file (-> index-file jio/file)
        file-size (.length file)
        in (-> file
               jio/input-stream)
        bytes (read-bytes in file-size)
        buffer (buf/allocate file-size)
        sig-spec (buf/spec buf/byte buf/byte  buf/byte  buf/byte)
        version-spec (buf/spec buf/byte  buf/byte  buf/byte  buf/byte)
        num-entry-spec (buf/spec buf/int32)
        header-spec (buf/spec sig-spec version-spec num-entry-spec)]
    (doto buffer
      (.put bytes))
    
    (let [header (buf/read buffer header-spec)
          sig (let [sig-bytes (nth header 0)
                    sig-chars (map outil/byte->ascii sig-bytes)]
                (clojure.string/join "" sig-chars))
          version (nth header 1)
          num-of-entries (nth header 2)]
      (assert (= "DIRC" sig) "Not Git index file because signature is not DIRC")
      (prn "sig=" sig)
      
      (prn "header=" header)
      (prn "version=" version)
      (prn "entries=" num-of-entries)
      )
    
    )
  )



(comment
  (parse-index "/tmp/test/.git/index")


  (.. (java.util.Base64/getEncoder)
      (encodeToString ))

  (hash-object "foobar\n")
  (def my-spec1 (buf/spec buf/int32 buf/bool))
  (buf/size my-spec1)
  (def buffer (buf/allocate 24))
  (buf/write! buffer [22 true] my-spec1)
  (buf/read buffer my-spec1)
  (buf/read buffer (buf/int32))

  (defrecord Point [x y])

  (def point-spec (reify
                    spec/ISpecSize
                    (size [_]
                      (prn "_=" _)
                      8)

                    spec/ISpec
                    (read [_ buff pos]
                      (let [[readed xvalue] (spec/read (buf/int32)  buff pos)
                            [readed' yvalue] (spec/read (buf/int32)  buff (+ pos readed))]
                        [(+ readed readed')
                         (Point. xvalue yvalue)]))

                    (write [_ buff pos point]
                      (let [written (spec/write (buf/int32) buff pos (:x point))
                            written' (spec/write (buf/int32) buff (+ pos written) (:y point))]
                        (+ written written')))))

  (def my-point (Point. 1 2))

  (buf/write! buffer my-point point-spec)
  (buf/read* buffer point-spec)
  (buf/size point-spec)

  (-> "/tmp/test/.git/index"
      jio/file
      (.length)
      )

  
  )
