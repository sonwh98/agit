(ns stigmergy.clgit
  (:require [stigmergy.io :as io]
            [clojure.java.io :as jio]
            [stigmergy.voodoo :as vd])
  (:import [java.nio.file Files LinkOption]
           [java.nio.file.attribute PosixFilePermissions]))

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
   (init {})))

(defn hash-object [data]
  (let [size (count data)
        git-str (str "blob " size "\0" data)]
    (-> git-str
        vd/sha1
        vd/seq->hex)))

(defn padding [n]
  (let [floor (quot (- n 2) 8)
        target (+ (* (inc floor) 8)
                  2)]
    (- target n)))

(defonce struct-header [:signature [:char 4]
                        :version :int32
                        :entry-count :int32])

(defonce struct-entry [:ctime-sec :int32
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
                       :name :char*])

(defonce struct-index (concat struct-header [:entries :byte*]))

(defn index->map [byte-buffer]
  (let [index-pt (vd/pointer struct-index byte-buffer)
        entry-count (vd/seq->int (index-pt :entry-count))
        entries (index-pt :entries)
        entry-pt (vd/pointer struct-entry entries)
        index-map {:signature (vd/seq->char (index-pt :signature))
                   :version (vd/seq->int (index-pt :version))
                   :entry-count entry-count
                   :entries (for [i (range entry-count)]
                              (let [name-len (entry-pt :name-len)
                                    file-name (take name-len (entry-pt :name))
                                    entry {:ctime-sec (vd/seq->int (entry-pt :ctime-sec))
                                           :ctime-nsec (vd/seq->int (entry-pt :ctime-nsec))
                                           :mtime-sec (vd/seq->int (entry-pt :mtime-sec))
                                           :mtime-nsec (vd/seq->int (entry-pt :mtime-nsec))
                                           :dev (vd/seq->int (entry-pt :dev))
                                           :ino (vd/seq->int (entry-pt :ino))
                                           :mode (vd/seq->oct (entry-pt :mode))
                                           :uid (vd/seq->int (entry-pt :uid))
                                           :gid (vd/seq->int (entry-pt :gid))
                                           :size (vd/seq->int (entry-pt :size))
                                           :sha1 (vd/seq->hex (entry-pt :sha1))
                                           :flags (entry-pt :flags)
                                           :name-len name-len
                                           :name (vd/seq->str file-name)}]
                                (entry-pt + :name name-len (padding name-len))
                                entry))}]
    index-map))

(defn index->seq [index-map]
  (let [int32->seq (fn [value]
                     (-> value vd/int->seq
                         (vd/pad-left (vd/sizeof :int32) 0)
                         vec))
        char->seq (fn [value]
                    (mapv #(byte %) value))
        header (for [[field type] (partition 2 struct-header)
                     :let [value (index-map field)]]
                 (cond
                   (= type :int32) (int32->seq value)
                   (= field :signature) (char->seq value)
                   :else value))
        entries (for [e (:entries index-map)]
                  (for [[field type] (partition 2 struct-entry)
                        :let [value (e field)]]
                    (cond
                      (= type :int32) (int32->seq value)
                      (= field :mode) (vd/oct->seq value)
                      (= field :name) (char->seq value)
                      (= field :sha1) (vd/hex->seq value)
                      :else value)))]
    (flatten (concat  header entries))))

(defn parse-index
  "parse a git index file, e.g. myproject/.git/index"
  [index-file]
  (let [buffer (vd/suck index-file)]
    (index->map buffer)))

(defn ms->sec-nanosec [ctime-ms]
  (let [ctime-sec (mod (quot ctime-ms 1000)
                       Integer/MAX_VALUE)
        ctime-nsec (mod (* (- ctime-ms (* ctime-sec 1000))
                           1000000)
                        Integer/MAX_VALUE)]
    [ctime-sec ctime-nsec]))

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

(defn add [{:keys [git-root file]}]
  (let [project-root (last (clojure.string/split git-root #"/"))
        index (parse-index (str git-root "/.git/index"))
        entries (:entries index)
        file-attributes (lstat (str git-root "/" file))
        ctime (file-attributes "ctime")
        ctime-ms (.. ctime toMillis)
        [ctime-sec ctime-nsec] (ms->sec-nanosec ctime-ms)
        ;;ctime-bytes (-> ctime vd/int->seq (vd/pad-left 8 0))
        mtime (file-attributes "lastModifiedTime")
        mtime-ms (.. mtime toMillis)
        [mtime-sec mtime-nsec] (ms->sec-nanosec mtime-ms)
        file-buffer (vd/suck (str git-root "/" file))
        new-entry {:ctime-sec ctime-sec 
                   :ctime-nsec ctime-nsec
                   :mtime-sec mtime-sec 
                   :mtime-nsec mtime-nsec 

                   :dev (get file-attributes "dev")
                   :ino (get file-attributes "ino")
                   :mode (vd/seq->oct (vd/int->seq (get file-attributes "mode")))
                   
                   :uid (get file-attributes "uid")
                   :gid (get file-attributes "gid")
                   :size (get file-attributes "size")
                   :sha1 (-> file-buffer vd/sha1 vd/seq->hex)
                   :flags 0
                   :name-len (count file)
                   :name file}
        entries (sort-by :name (conj entries new-entry))
        index (assoc index :entries entries :entry-count (count entries))]
    index))

(defn squirt [file seq-of-bytes]
  (with-open [os (jio/output-stream file)]
    (.. os (write (byte-array seq-of-bytes)))))

(comment
  (parse-index "/tmp/test/.git/index")

  (def index (add {:git-root "/tmp/test"
                   :file "src/add.clj"}))
  (def bi (index->seq index))
  (squirt "/tmp/test2/.git/index" bi)
  (def index2 (parse-index "/tmp/test2/.git/index"))
  )
