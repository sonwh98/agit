(ns stigmergy.agit
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

(defn wrap
  "git objects are wrapped in a header"
  [object-type a-seq]
  (let [header (git-object-header object-type a-seq)]
    (concat header (io/to-seq a-seq))))

(defn get-object-type-and-size [a-seq]
  (let [space 32
        null 0
        s (util/index-of a-seq space)
        n (util/index-of a-seq null)
        obj-type (vd/seq->str (take s a-seq))
        size (->> a-seq
                  (util/take-between (inc s) n )
                  vd/seq->str
                  Integer/parseInt)]
    [obj-type size]))

(defn unwrap
  "unwrap git object returning the raw content as seq of bytes"
  [a-seq]
  (let [[obj-type size] (get-object-type-and-size a-seq)
        content (take-last size a-seq)]
    (assert (= (count content)
               size))
    content))

(defn write-blob
  "write blob to .git/objects return the sha1 hash of blob"
  [project-root content]
  (let [size (count content)
        sha1-hex-str (hash-object "blob" content)
        two (vd/seq->str (take 2 sha1-hex-str))
        other (vd/seq->str (drop 2 sha1-hex-str))
        file-path (util/format "%s/.git/objects/%s/%s" project-root two other)
        compressed-content (->> content io/to-seq (wrap "blob") io/compress)]
    (io/squirt file-path compressed-content)
    sha1-hex-str))

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
        index-map {:signature (vd/seq->char-seq (index-pt :signature))
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
                            new-entry {:ctime-sec ctime-sec 
                                       :ctime-nsec ctime-nsec
                                       :mtime-sec mtime-sec 
                                       :mtime-nsec mtime-nsec 

                                       :dev (get file-attributes "dev")
                                       :ino (get file-attributes "ino")
                                       :mode (-> (file-attributes "mode")
                                                 vd/int->seq
                                                 vd/seq->oct)
                                       :uid (get file-attributes "uid")
                                       :gid (get file-attributes "gid")
                                       :size (get file-attributes "size")
                                       :sha1 (write-blob project-root file-content)
                                       :flags 0
                                       :name-len (count file)
                                       :name file}]
                        new-entry))
        entries (sort-by :name (concat entries new-entries))
        index (assoc index :entries entries :entry-count (count entries))]
    (->> index index-map->seq
         (io/squirt (str project-root "/.git/index")))
    index))

(defn cat-file [project-root sha1]
  (let [two (vd/seq->str (take 2 sha1))
        other (vd/seq->str (drop 2 sha1))
        file-path (util/format "%s/.git/objects/%s/%s" project-root two other)]
    (-> file-path
        io/suck
        io/decompress)))

(defn parse-tree-object [project-root sha1]
  (let [tree-seq (cat-file project-root sha1)]
    (loop [entries (unwrap tree-seq)
           results []]
      (if (pos? (count entries))
        (let [space 32
              null 0
              mode-end (util/index-of entries space)
              file-end (util/index-of entries null mode-end)
              sha1-end (+ (inc file-end) 20)
              tree-entry {:mode (vd/seq->str (take mode-end entries))
                          :path (vd/seq->str (util/take-between (inc mode-end) file-end entries))
                          :sha1 (vd/seq->hex (util/take-between (inc file-end) sha1-end entries))}]
          (recur (drop sha1-end entries) (conj results tree-entry)))
        results))))

(defn parse-commit-object [project-root sha1]
  (let [commit-content (unwrap (cat-file project-root sha1))
        commit (-> #_(clojure.string/join "" (map char commit-content))
                   (vd/char-seq->str (vd/seq->char-seq commit-content))
                   (clojure.string/split #"\n"))
        msg (last commit)]
    (prn commit)
    (into {"message" (vd/char-seq->str msg)}
          (for [line (drop-last 1 commit)
                :let [i (util/index-of (seq line) \space)]
                :when (and (->  line clojure.string/blank? not)
                           (-> i nil? not))]
            (let [k (vd/char-seq->str (take i line))
                  v (vd/char-seq->str (drop (inc i) line))]
              [k v])))))

(defn parse-blob-object [project-root sha1]
  (let [blob-content (unwrap (cat-file project-root sha1))]
    blob-content
    )
  )

(comment
  (def project-root "/home/sto/tmp/test")
  (init {:dir project-root})
  
  (def index (parse-git-index (str project-root "/.git/index")))

  (def index (add project-root
                  "src/mul.clj"))

  (write-blob project-root "test content\n")
  
  (cat-file project-root "55e3e7f64afee31012c8c00c56cdd97d95b5e31c")
  
  (parse-commit-object project-root "6602853e69285bfbed6d80480289278600c02a92")

  (parse-tree-object project-root "55e3e7f64afee31012c8c00c56cdd97d95b5e31c")
  (parse-tree-object project-root "618855e49e7bf8dbdbb2b1275d37399db2a7ed62")

  (vd/seq->char-seq (cat-file project-root "618855e49e7bf8dbdbb2b1275d37399db2a7ed62"))


  (parse-commit-object project-root "51c728e8d4c98187f8dcce40803982d44378d838")
  (parse-tree-object project-root "127ad2ef366f53e843334fb97cd4c29c6386d8f0")
  (parse-tree-object project-root "4bf3de4f6cee771dd26e4fc2f622d81acaec30fe")
  (parse-commit-object project-root "75ea033015e19b68f449fad44d52384f7b582b09")
  (parse-tree-object project-root "d3595c5162b9f12b3acfd103ec2c35ddd9e2172a")

  (parse-commit-object project-root "781ead446c9c0f4d789b78278e43936fba70c4a9")
  (parse-tree-object project-root "d082d83094e4496c05344c3c4a3a259744df3ba4")
  (parse-tree-object project-root "c7b2b064d38d017406637ceb61cb1fbec0b81c92")
  (-> (parse-blob-object project-root "1269488f7fb1f4b56a8c0e5eb48cecbfadfa9219")
      vd/seq->str
      )

  (parse-commit-object project-root "8776260b4af43343308fd020dcac15eb8d8becbd")
  (parse-tree-object project-root "e386f932851a893109d25351034204ac3123e8ba")

  (seq "tree d082d83094e4496c05344c3c4a3a259744df3ba4")
  (util/index-of (seq "tree d082d83094e4496c05344c3c4a3a259744df3ba4") \space)
  (cat-file project-root "8776260b4af43343308fd020dcac15eb8d8becbd")
  )