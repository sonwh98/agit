(ns stigmergy.clgit
  (:require [stigmergy.io :as io]
            [clojure.java.io :as jio]
            [stigmergy.voodoo :as vd])
  (:import [java.nio.file Files LinkOption]
           [java.nio.file.attribute PosixFilePermissions])
  )

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
        vd/sha1-as-bytes
        vd/bytes->hex)))

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

(defonce index (concat struct-header [:entries :byte*]))

(defn index-buffer->map [byte-buffer]
  (let [index [:signature [:char 4]
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
        index-pt (vd/pointer index byte-buffer)
        entry-count (vd/bytes->int (index-pt :entry-count))
        entries (index-pt :entries)
        entry-pt (vd/pointer entry entries)
        index-map {:signature (vd/bytes->char (index-pt :signature))
                   :version (vd/bytes->int (index-pt :version))
                   :entry-count entry-count
                   :entries (for [i (range entry-count)]
                              (let [name-len (entry-pt :name-len)
                                    file-name (take name-len (entry-pt :name))
                                    entry {:ctime-sec (vd/bytes->int (entry-pt :ctime-sec))
                                           :ctime-nsec (vd/bytes->int (entry-pt :ctime-nsec))
                                           :mtime-sec (vd/bytes->int (entry-pt :mtime-sec))
                                           :mtime-nsec (vd/bytes->int (entry-pt :mtime-nsec))
                                           :dev (vd/bytes->int (entry-pt :dev))
                                           :ino (vd/bytes->int (entry-pt :ino))
                                           :mode (vd/bytes->oct (entry-pt :mode))
                                           :uid (vd/bytes->int (entry-pt :uid))
                                           :gid (vd/bytes->int (entry-pt :gid))
                                           :size (vd/bytes->int (entry-pt :size))
                                           :sha1 (vd/bytes->hex (entry-pt :sha1))
                                           :flags (entry-pt :flags)
                                           :name-len name-len
                                           :name (vd/bytes->str file-name)}]
                                (entry-pt + :name name-len (padding name-len))
                                entry))}]
    index-map))

(defn map->index-buffer [index-map]
  (let [header (for [[field type] (partition 2 struct-header)
                     :let [value (index-map field)]]
                 [field (cond
                          (= type :int32) (vd/int->bytes value)
                          :else value)])
        entries (for [e (:entries index-map)]
                  (into {} (for [[field type] (partition 2 struct-entry)
                                 :let [value (e field)]]
                             [field (cond
                                      (= type :int32) (vd/int->bytes value)
                                      :else value)])))]
    (assoc (into {} header)
           :entries entries)))


(defn parse-index
  "parse a git index file, e.g. myproject/.git/index"
  [index-file]
  (let [buffer (vd/suck index-file)]
    (index-buffer->map buffer)))

(defn add [file-name]
  (let [index (parse-index "/tmp/test/.git/index")
        entries (:entries index)
        paths (clojure.string/split file-name #"/")
        root-dir (let [fp (first paths)]
                   (if (= "" fp)
                     "/"
                     fp))  
        path (java.nio.file.Paths/get root-dir (into-array (rest paths)))
        file-attributes (Files/readAttributes path "unix:*" (into-array [LinkOption/NOFOLLOW_LINKS]))
        file-buffer (vd/suck file-name)
        ctime (..(get file-attributes "ctime")
                 toMillis)
        ctime-bytes (vd/int->bytes ctime)
        _ (prn "ctime-bytes=" (seq ctime-bytes) " count=" (count ctime-bytes))
        ctime-sec (vd/bytes->int (take 4 ctime-bytes))
        ctime-nsec (vd/bytes->int (take-last 4 ctime-bytes))
        last-modified (get file-attributes "lastModifiedTime")
        new-entry {:ctime-sec ctime-sec
                   :ctime-nsec ctime-nsec
                   :mtime-sec (.. last-modified toMillis)
                   :mtime-nsec (.. last-modified toMillis)

                   :dev (get file-attributes "dev")
                   :ino (get file-attributes "ino")
                   :mode (vd/bytes->oct (vd/int->bytes (get file-attributes "mode")))
                   
                   :uid (get file-attributes "uid")
                   :gid (get file-attributes "gid")
                   :size (get file-attributes "size")
                   :sha1 (-> file-buffer vd/sha1-as-bytes vd/bytes->hex)
                   :flags 0
                   :name-len (count file-name)
                   :name file-name}
        entries (sort-by :name (conj entries new-entry))
        index (assoc index :entries entries :entry-count (count entries))]
    index))

(comment
  (parse-index "/tmp/test/.git/index")

  (def index (add "/tmp/test/src/add.clj"))
  (map->index-buffer index)
  
  (def m (:entries index))

  (doseq [entry (:entries index)]
    (doseq [[k v] entry]
      (prn k )
      )
    )
  
  (def a (let [paths (clojure.string/split "/tmp/test/src/add.clj" #"/")
               root-dir (let [fp (first paths)]
                          (if (= "" fp)
                            "/"
                            fp))
               _ (prn "root" root-dir)
               path (java.nio.file.Paths/get root-dir (into-array (rest paths)))]
           (Files/readAttributes path "unix:*" (into-array [LinkOption/NOFOLLOW_LINKS]))))
  
  )
