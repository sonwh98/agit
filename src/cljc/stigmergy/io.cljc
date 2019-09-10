(ns stigmergy.io
  (:require [taoensso.timbre :as log :include-macros true]
            [clojure.core.async :as a :include-macros true]
            [clojure.java.io :as jio]))

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

(defn squirt [file seq-of-bytes]
  (with-open [os (jio/output-stream file)]
    (.. os (write (byte-array seq-of-bytes)))))

(defn suck
  "like slurp but returns raw bytes"
  [file-name]
  (let [paths (clojure.string/split file-name #"/")
        root-dir (let [fp (first paths)]
                   (if (= "" fp)
                     "/"
                     fp))
        path (java.nio.file.Paths/get root-dir (into-array (rest paths)))]
    (try
      (java.nio.file.Files/readAllBytes path)
      (catch Exception ex nil))))
