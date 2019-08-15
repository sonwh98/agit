(ns stigmergy.clgit
  (:require [stigmergy.io :as io]
            #_[digest])
  (:import  [org.apache.commons.codec.binary Hex]
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


(comment
  (let [data "foobar3\n"
        sha1 (sha1-as-bytes data)
        hs (bytes->hex-str sha1)
        sha2 (hex-str->bytes hs)
        ]
    (prn  sha1)
    (prn sha2)
    
    )
  (-> "10"
      hex-str->bytes
      bytes->hex-str
      )

  (let [b (-> "foobar3\n"
              sha1-as-bytes
              )
        h (bytes->hex-str b)
        b2 (hex-str->bytes h)
        ]
    (prn h)
    (prn b)
    (prn b2)
    )

  (-> "9302e9711018ac4d6815617f44ee8cf55a1b6c53"
      hex-str->bytes
      bytes->hex-str)

  (def b1 (-> "9302e9711018ac4d6815617f44ee8cf55a1b6c53"
              hex-str->bytes))

  (def b2 (-> "9302e9711018ac4d6815617f44ee8cf55a1b6c53"
              hex-str->bytes))

  (-> "10" hex-str->bytes)
  
  (org.apache.commons.codec.binary.Hex/decodeHex "00A0BF")
  
  
  (Integer/toString (Integer/parseInt "16" 10) 16)
  (dec->hex 16)
  )
