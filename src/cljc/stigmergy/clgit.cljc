(ns stigmergy.clgit
  (:require [stigmergy.io :as io]
            #_[digest])
  (:import  [org.apache.commons.codec.binary Hex]))

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
  (.. (BigInteger. 1 bytes)
      (toString 16)))

(defn hex-str->bytes [hex-str]
  (Hex/decodeHex hex-str)
  )

(defn sha1-bytes [data]
  (let [md (java.security.MessageDigest/getInstance "SHA-1")
        data-bytes (.. data getBytes)]
    (.. md (digest data-bytes))))

(defn sha1-str [data]
  (-> data
      sha1-bytes
      bytes->hex-str))

(defn hash-object [data]
  (let [size (count data)
        s (str "blob " size "\0" data)]
    (sha1-str s)))


(comment
  (let [data "foobar\n"
        sha1-bytes (sha1-bytes data)
        sha-str (sha1-str data)
        sha1-bytes2 (hex-str->byte-array sha-str)]
    (prn sha-str)
    (prn (= sha1-bytes
            sha1-bytes2))
    (prn "sha1" sha1-bytes)
    (prn "sha2" sha1-bytes2)
    
    )

  (org.apache.commons.codec.binary.Hex/decodeHex "00A0BF")
  
  
  (Integer/toString (Integer/parseInt "16" 10) 16)
  (dec->hex 16)
  )
