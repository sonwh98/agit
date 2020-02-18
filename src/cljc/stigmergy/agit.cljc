(ns stigmergy.agit
  (:require [clojure.pprint :refer :all]
            [stigmergy.io :as io]
            [stigmergy.node :as n]
            [stigmergy.tily :as util]
            [stigmergy.voodoo :as vd]))

(def project-root "/home/sto/tmp/agit")

(defn init
  ([{:keys [project-root]}]
   (let [git-dir ".git"
         mk-path (fn [file-or-dir]
                   (let [path (str git-dir "/" file-or-dir)]
                     (if project-root
                       (str project-root "/" path)
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

(defn delete-directory-recursive [^java.io.File file]
  (when (.isDirectory file)
    (doseq [file-in-dir (.listFiles file)]
      (delete-directory-recursive file-in-dir)))
  (clojure.java.io/delete-file file))

(defn delete [project-root]
  (delete-directory-recursive (clojure.java.io/file (str project-root "/.git"))))

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
  "convert git index from a structured map into a sequence of bytes by stripping out the keys
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

;;TODO should take index-map, files and return index-map
(defn remove-entry-duplicates [project-root files]
  (let [git-dir (str project-root "/.git")
        index (let [index (parse-git-index (str git-dir "/index"))]
                (if index
                  index
                  {:signature '(\D \I \R \C)
                   :version 2
                   :entry-count 0
                   :entries []}))
        entries (:entries index)
        entries (vec (remove (fn [{:keys [name] :as entry}]
                               (util/some-in? name files))
                             entries))]
    (assoc index :entries entries :entry-count (count entries))))

(defn rm [project-root & files]
  (let [index (remove-entry-duplicates project-root files)
        entries (:entries index)
        entry-count (:entry-count index)
        index (assoc index :entries entries)
        index-seq (let [index-seq (-> index index-map->seq)
                        sha1-bits 160
                        num-bytes (/ sha1-bits 8)
                        empty-sha1 (repeat num-bytes 0)]
                    (if (zero? entry-count)
                      (concat index-seq empty-sha1)
                      index-seq))]
    (->> index-seq
         (io/squirt (str project-root "/.git/index")))
    index))

(defn add [project-root & files]
  (let [index (remove-entry-duplicates project-root files)
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
    ;;(prn "cat-file " file-path)
    (-> file-path
        io/suck
        io/decompress)))

(defn cat-file-str [project-root sha1]
    (-> (cat-file project-root sha1)
      vd/seq->char-seq
      vd/char-seq->str))

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

(defn person-str->person-timestamp [person]
  (let [fields (clojure.string/split person #" ")
        c (count fields)
        person (clojure.string/join " " (take (- c 2) fields))
        timestamp (take-last 2 fields)
        timestamp {:sec (-> timestamp first Long/parseLong)
                   :timezone (->> timestamp second (drop-last 2) vd/char-seq->str Integer/parseInt)}]
    [person timestamp]))

(defn commit-seq->map [seq-of-bytes]
  (let [commit (-> seq-of-bytes
                   vd/seq->char-seq 
                   vd/char-seq->str 
                   (clojure.string/split #"\n"))
        kv-pairs (for [line (drop-last 1 commit)
                       :let [i (util/index-of (seq line) \space)]
                       :when (and (->  line clojure.string/blank? not)
                                  (-> i nil? not))]
                   (let [k (-> (take i line)
                               vd/char-seq->str
                               keyword)
                         v (vd/char-seq->str (drop (inc i) line))]
                     (cond
                       (= k :author) (let [[person timestamp] (person-str->person-timestamp v)]
                                       [:author {:person person
                                                 :timestamp timestamp}])
                       (= k :committer) (let [[person timestamp] (person-str->person-timestamp v)]
                                          [:committer {:person person
                                                       :timestamp timestamp}])
                       :else [k v])))]
    (into {:message (-> commit last vd/char-seq->str)}
          kv-pairs)))

(defn commit-map->seq [cm]
  (let [ks [:tree :parent :author :commiter :message]
        commit-lines (for [k ks
                           :let [v (k cm)]
                           :when (-> v nil? not)]
                       (cond
                         (or  (= k :author)
                              (= k :commiter)) (let [{:keys [person timestamp]} v
                                                      {:keys [sec timezone]} timestamp]
                                                  (str (name k) " " person " " sec " " timezone))
                         (= k :message) (str "\n" v)
                         :else (str (name k) " " v)))
        commit-lines-as-str (clojure.string/join "\n" commit-lines)]
    commit-lines-as-str))

(defn parse-blob-object [project-root sha1]
  (let [blob-content (unwrap (cat-file project-root sha1))]
    blob-content
    )
  )

(defn ls [project-root]
  (let [files-n-dirs (-> (str project-root "/.git/objects")
                         clojure.java.io/file
                         file-seq)
        files (filter #(.isFile %) files-n-dirs)
        path-type-sha1 (map #(let [path (.getPath %)
                                   i (util/index-of path ".git")
                                   git-path (-> (drop i path)
                                                vd/seq->str)
                                   sha1 (-> (take-last 42 git-path)
                                            vd/seq->str
                                            (clojure.string/replace #"/" ""))
                                   t (vd/seq->str (cat-file project-root sha1))]
                               [git-path t  sha1])
                            files)]
    path-type-sha1))

(defn log [project-root]
  (let [ls-commits (filter #(re-find #"commit" (second %))
                           (ls project-root))]
    (->> ls-commits
         (map (fn [[path commit-type sha1]]
                (-> (cat-file project-root sha1)
                    unwrap
                    commit-seq->map
                    (assoc :sha1 sha1))))
         (sort-by (fn [commit-map]
                    (-> commit-map :author :timestamp :sec)))
         reverse)))

(defn dir? [tree-entry]
  (= (:mode tree-entry) "40000"))

(defn get-files [project-root tree-sha1 parents]
  (let [tree-entries (parse-tree-object project-root tree-sha1)]
    (for [tree-entry tree-entries]
      (if (dir? tree-entry)
        (get-files project-root (:sha1 tree-entry) (conj parents (:path tree-entry)))
        (assoc-in tree-entry [:parents] parents)))))

(defn status [project-root]
  (let [commit-files (atom [])
        nested-files (let [commits (log project-root)]
                       (for [c commits]
                         (get-files project-root (:tree c) [])))
        _ (prn "nested-files=" nested-files)
        _ (clojure.walk/postwalk (fn [v]
                                   (when (and (map? v)
                                              (not (some (fn [file]
                                                           (= (:sha1 v)
                                                              (:sha1 file)))
                                                         @commit-files)))
                                     (swap! commit-files conj v))
                                   v)
                                 nested-files)
        _ (prn "commit-files=" @commit-files)
        index (parse-git-index (str project-root "/.git/index"))
        index-entries (:entries index)
        result (for [{:keys [name sha1] :as index-entry} index-entries]
                 (cond
                   (some #(= sha1 (:sha1 %)) @commit-files) (do
                                                              (prn "index-entry=" index-entry)
                                                              [:no-change index-entry])
                   (some (fn [c]
                           (let [name-paths (clojure.string/split name #"/")
                                 file-name (last name-paths)
                                 parents (drop-last name-paths)
                                 path (:path c)]
                             (= file-name path)))
                         @commit-files) [:modified index-entry]
                   (not (some #(= % index-entry) @commit-files)) [:new index-entry]
                   :else [:unknown index-entry]))
        _ (prn "result1=" result)
        result (group-by first result)
        result {:modified (map second (:modified result))
                :new (map second (:new result))
                :no-change (map second (:no-change result))
                }]
    result))

(defn mk-tree [parent path]

  )

(defn index-entry->tree-entry [index-entry]
  (select-keys index-entry [:mode :name :sha1]))

(defn mkdir [nodes]
  (reduce n/join-node nodes))

(defn tree-hash [project-root]
  (let [status (status project-root)
        new-entries (map index-entry->tree-entry  (:new status))
        modified-entries (map index-entry->tree-entry (:modified status))
        
        unchanged-entries (map index-entry->tree-entry (:no-change status))
        commits (log project-root)
        head-commit (first commits)
        head-tree (:tree head-commit)
        new-entries (map (fn [e]
                           (let [path (first (clojure.string/split (:name e) #"/"))
                                 e (clojure.set/rename-keys e {:name :path})
                                 e (select-keys e [:mode :path :sha1])
                                 e (assoc e :path path)]
                             e))
                          new-entries)
        files (concat new-entries modified-entries unchanged-entries)
        tree-seq (flatten (map (fn [{:keys [mode path sha1]}]
                                 (let [mode-path (vd/str->seq (str mode " " path))
                                       sha1-binary (vd/hex->seq sha1)]
                                   (concat mode-path [0] sha1-binary)))
                               files))
        sha1-hex-str (hash-object "tree" tree-seq)]
    sha1-hex-str))

(defn write-tree [project-root]
  (let [status (status project-root)
        new-entries (map index-entry->tree-entry  (:new status))
        modified-entries (map index-entry->tree-entry (:modified status))
        
        unchanged-entries (map index-entry->tree-entry (:no-change status))
        commits (log project-root)
        head-commit (first commits)
        head-tree (:tree head-commit)
        new-entries (map (fn [e]
                           (let [path (first (clojure.string/split (:name e) #"/"))
                                 e (clojure.set/rename-keys e {:name :path})
                                 e (select-keys e [:mode :path :sha1])
                                 e (assoc e :path path)]
                             e))
                          new-entries)
        files (concat new-entries modified-entries unchanged-entries)
        _ (prn "files=" files)
        _ (prn "new-entries=" new-entries)
        ;; files (map (fn [f]
        ;;              (-> f
        ;;                  n/->path
        ;;                  n/->node
        ;;                  )) files)
        ;; _ (prn "files2=" files)
        tree-seq (flatten (map (fn [{:keys [mode path sha1]}]
                                 (let [mode-path (vd/str->seq (str mode " " path))
                                       sha1-binary (vd/hex->seq sha1)]
                                   (concat mode-path [0] sha1-binary)))
                               files))
        sha1-hex-str (hash-object "tree" tree-seq)
        two (vd/seq->str (take 2 sha1-hex-str))
        other (vd/seq->str (drop 2 sha1-hex-str))
        file-path (util/format "%s/.git/objects/%s/%s" project-root two other)
        tree (->> tree-seq (wrap "tree") io/compress)
        ]
    (io/squirt file-path tree)
    sha1-hex-str
    ;;head-tree
    
    ;;unchanged-entries
    ;;(mkdir files)
    )
  )


(defn commit-map [{:keys [project-root message]}]
  (let [;;status (status project-root)
        head-commit (first (log project-root))
        head-sha1 (:sha1 head-commit)
        timestamp (.. (java.util.Date.) getTime)
        sec (quot  timestamp 1000)
        author {:person "sto <son.c.to@gmail.com>"
                :timestamp {:sec sec :timezone "-0500"}} ;;hardcoded timezone
        committer author
        cm {:message message
            :tree (tree-hash project-root) ;;TODO suspect write-tree is wrong
            :author author
            :commiter committer}
        cm (if head-sha1
             (assoc cm :parent head-sha1)
             cm)]
    cm))

(defn commit [{:keys [project-root] :as params}]
  (let [commit-map (commit-map params)
        commit-seq (commit-map->seq commit-map)
        sha1-hex-str (hash-object "commit" commit-seq)
        two (vd/seq->str (take 2 sha1-hex-str))
        other (vd/seq->str (drop 2 sha1-hex-str))
        file-path (util/format "%s/.git/objects/%s/%s" project-root two other)
        commit (->> commit-seq (wrap "commit") io/compress)
        master-ref-path (str project-root "/.git/refs/heads/master")]
    (io/squirt file-path commit)
    (spit master-ref-path (str sha1-hex-str "\n"))
    sha1-hex-str))

(comment

  (init {:project-root project-root})
  (delete project-root)
  
  (def index (add project-root
                  "project.clj"))
  (commit {:project-root project-root
           :message "1"})
  
  (def index (add project-root
                  "io.cljc"
                  #_"parse_git_index.c"))
  (commit {:project-root project-root
           :message "2"})

  (commit-map {:project-root project-root
               :message "3"})
  
  (def index (add project-root
                  "io.cljc"))



  (def index2 (parse-git-index (str project-root "/.git/index")))

    
  (commit-map {:project-root project-root
               :message "foobar"})
    
  (def cm (log project-root))
  
  (def st (status project-root))

  (-> (parse-blob-object project-root "519408a5747c66ff45b792fa69f0811401e2dfa5")
      vd/seq->char-seq
      vd/char-seq->str
      )
  (map n/->path ["/src/foo.bar"])
  (parse-tree-object project-root "f45ddc24e41ed7b5086659b811b9694215b14506")

  (hash-object "tree" (flatten (map (fn [{:keys [mode path sha1]}]
                                        (let [mode-path (vd/str->seq (str mode " " path))
                                              sha1-binary (vd/hex->seq sha1)]
                                          (concat mode-path [0] sha1-binary)))
                                    [{:mode "100644"
                                      :path "parse_git_index.c"
                                      :sha1 "306a9ea8cb563ba61de6d4f6462f4f3b70e52ef0"}
                                     {:mode "100644"
                                      :path "project.clj"
                                      :sha1 "e00a70c4aeaa5c8d039946f606c6c001f8cc5ca4"}]
                                    )))
  
  (-> (cat-file project-root "306a9ea8cb563ba61de6d4f6462f4f3b70e52ef0")
      vd/seq->char-seq
      vd/char-seq->str
      )

  
  (def gobj (ls project-root))

    (write-tree project-root)
  (let [root-tree (parse-tree-object project-root "b84b2a329705f3fcaa99e38557ed4e1e18dd179e")
        tree (flatten (map (fn [{:keys [mode path sha1]}]
                             (let [mode-path (vd/str->seq (str mode " " path))
                                   sha1-binary (vd/hex->seq sha1)]
                               (concat mode-path [0] sha1-binary)))
                           root-tree))]
    #_(hash-object "tree" tree)
    tree
    )

  (->> (cat-file project-root "519408a5747c66ff45b792fa69f0811401e2dfa5")
       #_(take-last 3807)
       get-object-type-and-size
      #_vd/seq->char-seq
      #_vd/char-seq->str)

  (->> (cat-file project-root "519408a5747c66ff45b792fa69f0811401e2dfa5")
       count
       #_(take-last 1004)
       #_vd/seq->char-seq
       )

  (spit "/tmp/foo.txt" (cat-file-str project-root "306a9ea8cb563ba61de6d4f6462f4f3b70e52ef0"))
    
    
  (parse-tree-object project-root "59b793192c0653e86f7b7d4532b598450f1a4444")
  (commit-map {:project-root project-root :message "testing"})
  
  
  (let [f1 (concat (vd/str->seq (str "100644" " " "baz.txt"))
                   [0]
                   (vd/hex->seq "76018072e09c5d31c8c6e3113b8aa0fe625195ca"))
        f1-2 (concat (vd/str->seq (str "100644" " " "foo.txt"))
                     [0]
                     (vd/hex->seq "448d72f5402480b2edb332446aafccfd579eb94d"))
        f2 (concat (vd/str->seq (str "100644" " " "hi.txt"))
                   [0]
                   (vd/hex->seq "45b983be36b73c0788dc9cbcb76cbb80fc7bb057"))
        f1+f2 (concat f1 f1-2 f2)]
    (hash-object "tree" f1+f2)
    )

  (-> {:project-root project-root :message "foobar"}
      commit-map
      commit-map->seq
      )

  (write-blob project-root "hello")

  (use '[clojure.java.shell :only [sh]])

  (sh "/usr/bin/git cat-file -p" "519408a5747c66ff45b792fa69f0811401e2dfa5" :dir "/home/sto/tmp/agit")

  (defn cmp-content [sha1]
    (let [cat (util/format "cd /home/sto/tmp/agit; git cat-file -p %s" sha1)]
      (prn  (sh "sh" "-c" cat ))
      ))
  
  (sh "sh" "-c" "cd /home/sto/tmp/agit; git cat-file -p 519408a5747c66ff45b792fa69f0811401e2dfa5")

  (cmp-content "519408a5747c66ff45b792fa69f0811401e2dfa5")
  
  )


