(ns stigmergy.clgit
  (:require [stigmergy.io :as io]
            [clojure.java.io :as jio]
            [stigmergy.voodoo :as vd]))

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
                              (let [name-len (vd/bytes->int (entry-pt :name-len))
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

(comment
  (parse-index "/tmp/test/.git/index")
  (defn foo [a-map]
    (reduce merge (for [[k v] a-map]
                    (into {} (for [[k' v'] v]
                               [[k k'] v'])))))

  (defn foo1 [m]
    (into {}
          (for [[k v] m [vk vv] v]
            [[k vk] vv])))
  
  (foo1 '{a {p 1, q 2}
          b {m 3, n 4}})

  (def digits [1 2 3])
  (for [x1 digits x2 digits] [x1 x2])
  
  
  
  (= [:a :b :c] (list :a :b :c) (vec '(:a :b :c)) (vector :a :b :c))
  (conj {:a 1} {:b 2} [:c 3])
  (last [1 2 3 4 5])
  (drop-last  (take-last 2 [1 2 3 4]))

  ((fn foo [coll]
     (first (last  (map-indexed (fn [i c]
                                  [i c])
                                coll)))
     ) '(1 2 3 3 1))

  (reduce (fn [a b]
            (conj a b)
            )
          '()
          '(1 2 3))gggg
  )
