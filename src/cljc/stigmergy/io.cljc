(ns stigmergy.io
  (:require [taoensso.timbre :as log :include-macros true]
            [clojure.core.async :as a :include-macros true]
            [clojure.java.io :as jio]
            [stigmergy.voodoo :as vd])
  (:import [java.nio.file Files LinkOption]
           [java.nio.file.attribute PosixFilePermissions]))

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
  (let [f (java.io.File. file)]
    (.. f getParentFile mkdirs)
    (with-open [os (jio/output-stream file)]
      (.. os (write (byte-array seq-of-bytes))))))

(defn suck
  "like slurp but returns vector of bytes"
  [file-path]
  (try
    (with-open [f-in (jio/input-stream file-path)
                b-out (java.io.ByteArrayOutputStream.)]
      (jio/copy f-in b-out)
      (vec (.. b-out toByteArray)))
    (catch Exception ex nil)))

(defn lstat [file]
  (let [paths (clojure.string/split file #"/")
        path0 (first paths)
        root (cond
               (clojure.string/blank? path0) "/"
               (= 1 (count paths)) "."
               :else path0)
        more-paths (let [rest-paths (rest paths)]
                     (if (empty? rest-paths)
                       paths
                       rest-paths))
        path (java.nio.file.Paths/get root (into-array more-paths))]
    (into {} (Files/readAttributes path "unix:*" (into-array [LinkOption/NOFOLLOW_LINKS])))))

(defn to-seq [a-seq]
  (if (string? a-seq)
    (vd/str->seq a-seq)
    a-seq))

(defn gunzip [zipped-bytes]
  (with-open [zip-ins (-> zipped-bytes
                          byte-array
                          jio/input-stream
                          (java.util.zip.InflaterInputStream.))
              b-out (java.io.ByteArrayOutputStream.)]
    (jio/copy zip-ins b-out)
    (vec (.. b-out toByteArray))))

(defn gunzip-file [file-path]
  (-> file-path suck gunzip))

(defn gzip [a-seq]
  (with-open [baos (java.io.ByteArrayOutputStream.)
              gzos (java.util.zip.GZIPOutputStream. baos)
              ]
    (.. gzos (write (byte-array (to-seq a-seq))))
    (.. gzos close)
    (.. baos toByteArray)))

(defn zip [file-path a-seq]
  (let [entry (java.util.zip.ZipEntry. file-path)]
    (.. entry (setSize (count a-seq)))
    
    (with-open [baos (java.io.ByteArrayOutputStream.)
                zos (java.util.zip.ZipOutputStream. baos)]
      (.. zos (putNextEntry entry))
      (.. zos closeEntry)
      (.. zos close) ;;need to close zos before can call toByteArray. without this zip is corrupted
      (.. baos toByteArray))))

(defn compress [a-seq]
  (let [compressor (java.util.zip.Deflater.)
        a-bytes (vd/seq->byte-array a-seq)
        _ (.. compressor (setInput a-bytes))
        _ (.. compressor finish)
        
        buffer (byte-array (* 2 (count a-bytes)))
        byte-count (.. compressor (deflate buffer))
        bytes-compressed (take byte-count buffer)]
    (vd/seq->byte-array bytes-compressed)))

(defn decompress [compressed-data]
  (let [decompressor (java.util.zip.Inflater.)
        compressed-data (vd/seq->byte-array compressed-data)
        _ (.. decompressor (setInput compressed-data))
        buffer (byte-array 1024)
        baos (java.io.ByteArrayOutputStream.)
        decompressed-bytes (loop [finished? false]
                             (if finished?
                               (do
                                 (.. baos close)
                                 (.. baos toByteArray))
                               (let [byte-count (.. decompressor (inflate buffer))
                                     finished? (zero? (.. decompressor getRemaining))]
                                 (..  baos (write buffer 0 byte-count))
                                 (recur finished?))))]
    (vec decompressed-bytes)))
