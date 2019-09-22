(ns stigmergy.clgit
  (:require [stigmergy.io :as io]
            [stigmergy.voodoo :as vd]
            [stigmergy.tily :as util]))

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

(defn git-object-header
  [obj-type a-seq]
  (let [size (count a-seq)]
    (vd/str->seq (str obj-type " " size "\0"))))

(defn hash-object
  "git hash-object does not hash the raw bytes but adds a header before sha1 hashing.
   https://stackoverflow.com/questions/552659/how-to-assign-a-git-sha1s-to-a-file-without-git/552725#552725"
  ([obj-type a-seq]
   (let [header (git-object-header obj-type a-seq)
         seq-to-hash (concat header (io/to-seq a-seq))
         sha1-hex-str (-> seq-to-hash
                          vd/sha1
                          vd/seq->hex)]
     sha1-hex-str))
  ([a-seq]
   (hash-object "blob" a-seq)))

(defn wrap [object-type a-seq]
  (let [header (git-object-header object-type a-seq)]
    (concat header (io/to-seq a-seq))))

(defn get-object-type-and-size [a-seq]
  (let [space 32
        null 0
        s (.indexOf a-seq space)
        n (.indexOf a-seq null)
        obj-type (vd/seq->str (take s a-seq))
        size (->> a-seq
                  (util/take-between (inc s) n )
                  vd/seq->str
                  Integer/parseInt)]
    [obj-type size]))

(defn unwrap [a-seq]
  (let [[obj-type size] (get-object-type-and-size a-seq)
        content (take-last size a-seq)]
    (assert (= (count content)
               size))
    content))

(defn write-blob [project-root content]
  (let [size (count content)
        sha1-hex-str (hash-object "blob" content)
        two (vd/seq->str (take 2 sha1-hex-str))
        other (vd/seq->str (drop 2 sha1-hex-str))
        file-path (util/format "%s/.git/objects/%s/%s" project-root two other)
        compressed-content (->> content io/to-seq (wrap "blob") io/compress)]
    (io/squirt file-path compressed-content)
    file-path))

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

(defn index-seq->map
  "convert git index as sequence of bytes into a map with keys
  :signature :version :entry-count :entries"
  [seq-of-bytes]
  (let [index-pt (vd/pointer struct-index seq-of-bytes)
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
                                           :name (vd/seq->str file-name) 
                                           }]
                                (entry-pt + :name name-len (padding name-len))
                                entry))}]
    index-map))

(defn index-map->seq
  "convert git index in a structured map into a sequence of bytes by stripping out the keys
  from index-map and flatten the values into a sequence of bytes"
  [index-map]
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
                      (= field :mode) (-> value vd/oct->seq (vd/pad-left (vd/sizeof :int32) 0))
                      (= field :name) (let [name-len (:name-len e)
                                            padding-count (padding name-len)
                                            pad (repeat padding-count 0)]
                                        (concat (char->seq value)
                                                pad))
                      (= field :sha1) (vd/hex->seq value)
                      :else value)))]
    (flatten (concat header entries))))

(defn parse-git-index
  "parse a git index file, e.g. myproject/.git/index, into a map. Format of git index file is at
  https://github.com/git/git/blob/master/Documentation/technical/index-format.txt"
  [index-file]
  (let [buffer (io/suck index-file)]
    (when buffer
      (index-seq->map buffer))))

(defn ms->sec-nanosec [ctime-ms]
  (let [ctime-sec (mod (quot ctime-ms 1000)
                       Integer/MAX_VALUE)
        ctime-nsec (mod (* (- ctime-ms (* ctime-sec 1000))
                           1000000)
                        Integer/MAX_VALUE)]
    [ctime-sec ctime-nsec]))

(defn add [project-root & files]
  (let [git-dir (str project-root "/.git")
        index (let [index (parse-git-index (str git-dir "/index"))]
                (if index
                  index
                  {:signature '(\D \I \R \C)
                   :version 2
                   :entry-count 0
                   :entries []}))
        entries (:entries index)
        new-entries (for [file files]
                      (let [file-attributes (io/lstat (str project-root "/" file))
                            ctime (file-attributes "ctime")
                            ctime-ms (.. ctime toMillis)
                            [ctime-sec ctime-nsec] (ms->sec-nanosec ctime-ms)
                            mtime (file-attributes "lastModifiedTime")
                            mtime-ms (.. mtime toMillis)
                            [mtime-sec mtime-nsec] (ms->sec-nanosec mtime-ms)
                            file-path (str project-root "/" file)
                            file-content (io/suck file-path)
                            sha1-as-bytes (-> file-content vd/sha1 )
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
                                       :sha1 (vd/seq->hex sha1-as-bytes)
                                       :flags 0
                                       :name-len (count file)
                                       :name file}]
                        (write-blob project-root file-content)
                        new-entry))
        entries (sort-by :name (concat entries new-entries))
        index (assoc index :entries entries :entry-count (count entries))]
    (->> index index-map->seq
         (io/squirt (str project-root "/.git/index")))
    index))

(defn index-of
  "find index of a value v in a-seq starting from s"
  ([a-seq v s]
   (let [found (filter (fn [[index value]]
                         (and (>= index s)
                              (= value v)))
                       (util/with-index a-seq))
         found-index (ffirst found)]
     found-index))
  ([a-seq v]
   "find index of a value v in a-seq starting from 0"
   (index-of a-seq v 0)))

(defn parse-tree-object
  "takes tree-seq raw bytes of tree object and returns a vector of maps containing entries with keys :mode :file :sha1"
  [tree-seq]
  (loop [entries (unwrap tree-seq)
         results []]
    (if (pos? (count entries))
      (let [space 32
            null 0
            mode-end (index-of entries space)
            file-end (index-of entries null mode-end)
            sha1-end (+ (inc file-end) 20)
            tree-entry {:mode (vd/seq->str (take mode-end entries))
                        :file (vd/seq->str (util/take-between (inc mode-end) file-end entries))
                        :sha1 (vd/seq->hex (util/take-between (inc file-end) sha1-end entries))}]
        (recur (drop sha1-end entries) (conj results tree-entry)))
      results)))

(comment
  (def project-root "/home/sto/tmp/test")
  (def index (parse-git-index (str project-root "/.git/index")))

  (def index (add project-root
                  "mul.clj"))

  (->>
   ;;(str project-root "/.git/objects/23/289bbde2cf96efd692f68e6510f9d8309538c4") ;;mul.clj
   ;;(str project-root "/.git/objects/3e/d6dc80c6424e488326c987d1d648dae033b1a5") ;;add.clj
   ;;(str project-root "/.git/objects/5f/1f8c3b0602b9d6b24d0aa2de483ca3ec04293a") ;;commit
   ;;(str project-root "/.git/objects/e4/bacba3da20fe448dccfa5c8a92fcb9c3a05e89") ;;tree
   ;;(str project-root "/.git/objects/61/8855e49e7bf8dbdbb2b1275d37399db2a7ed62")
   
   ;;(str project-root "/.git/objects/1c/ea9c4904eac4b98ceed306528d4affc88e0fcc") ;; tree object
   ;;(str project-root "/.git/objects/ac/e1184aaa4831125dd8ac321ff58356345b5270");; commit object
   (str project-root "/.git/objects/a9/8fd2c2855c3d2b12876854a773d30f2c39199b")
   io/suck

   io/decompress
   ;;unwrap
   vd/seq->str
   )

  (let [tree-seq (->> (str project-root "/.git/objects/a9/8fd2c2855c3d2b12876854a773d30f2c39199b")
                      io/suck
                      io/decompress
                      vec)
        [obj-type size] (get-object-type-and-size tree-seq)
        ;;content (vd/seq->str (take 22 tree-seq))
        entries2 (take-last size tree-seq)]
    (prn "size=" size)
    (prn "tree-seq=" (vd/seq->str tree-seq))
    (prn "entries2=" entries2)
    (loop [entries entries2
           results []]
      (prn "entries=" entries)
      (prn "results= " results)
      (if (pos? (count entries))
        (let [space 32
              null 0
              mode-end (index-of entries space)
              _ (prn "mode-end" mode-end)
              file-end (index-of entries null mode-end)
              _ (prn "file-end="file-end)
              sha1-end (+ (inc file-end) 20)
              _ (prn "sha1-end=" sha1-end)
              tree-entry {:mode (vd/seq->str (take mode-end entries))
                          :file (vd/seq->str (util/take-between (inc mode-end) file-end entries))
                          :sha1 (vd/seq->hex (util/take-between (inc file-end) sha1-end entries))}]
          (recur (drop sha1-end entries) (conj results tree-entry)))
        results)
      )
    
    ;;(vd/seq->str (util/take-between 0 24 tree-seq))
    ;;(vd/seq->str (util/take-between 0 size tree-seq))
    ;;[obj-type size]
    ;;(->> tree-seq (take 20) vd/seq->str)
    ;;(vd/seq->str file-meta)

    )
  
  
  (write-blob project-root "add\n")
  [{:mode (vd/seq->oct (49 48 48 54 52 52)),
    :file (32 97 100 100 46 99 108 106),
    :sha1 (0 62 -42 -36 -128 -58 66 78 72 -125 38 -55 -121 -47 -42 72 -38 -32 51 -79)}
   {:mode (56 -60 49 48 48 54 52 52),
    :file (32 115 117 98 46 99 108 106),
    :sha1 (0 120 92 94 16 42 -100 -74 13 102 -80 75 36 -82 112 -74 -16 -55 55 118)}]

  (parse-tree-object (->> (str project-root "/.git/objects/61/8855e49e7bf8dbdbb2b1275d37399db2a7ed62")
                          io/suck
                          io/decompress
                          ))
  )
