(ns stigmergy.clgit
  (:require [stigmergy.io :as io]
            [clojure.java.io :as jio]
            [stigmergy.voodoo :as vd])
  (:import [java.nio.file Files LinkOption])
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
        file-attributes (Files/readAttributes path  java.nio.file.attribute.BasicFileAttributes
                                              (into-array [LinkOption/NOFOLLOW_LINKS]))
        last-modified (.. file-attributes lastModifiedTime toMillis)
        last-access (.. file-attributes lastAccessTime toMillis)
        file-buffer (vd/suck file-name)
        new-entry {:ino 6291480
                   :uid (Files/getAttribute path "unix:uid"
                                            (into-array [LinkOption/NOFOLLOW_LINKS]))
                   :gid (Files/getAttribute path "unix:gid"
                                            (into-array [LinkOption/NOFOLLOW_LINKS]))
                   :name file-name
                   :ctime-sec last-modified
                   :ctime-nsec last-access
                   :mode "100644"
                   :size (count file-buffer)
                   
                   :sha1 (-> file-buffer vd/sha1-as-bytes vd/bytes->hex)
                   :flags 0
                   :mtime-sec last-modified
                   :mtime-nsec last-modified
                   :name-len (count file-name)
                   :dev 2050
                   }
        entries (sort-by :name (conj entries new-entry))
        index (assoc index :entries entries :entry-count (count entries))]
    index
    )
  )

(comment
  (parse-index "/tmp/test/.git/index")

  (add "/tmp/test/src/add.clj")
  )
