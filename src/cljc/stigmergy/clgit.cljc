(ns stigmergy.clgit
  (:require [stigmergy.io :as io]
            #_[digest]))

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

(defn sha1-bytes [data]
  (let [md (java.security.MessageDigest/getInstance "SHA-1")
        data-bytes (.. data getBytes)]
    (.. md (digest data-bytes))))

(defn sha1-str [data]
  (let [hash-bytes (sha1-bytes data)
        hash-str (.. (BigInteger. 1 hash-bytes)
                     (toString 16))]
    hash-str))

(defn hash-object [data]
  (let [size (count data)
        s (str "blob " size "\0" data)]
    (sha1-str s)))
