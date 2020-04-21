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
        _ (clojure.walk/postwalk (fn [v]
                                   (when (and (map? v)
                                              (not (some (fn [file]
                                                           (= (:sha1 v)
                                                              (:sha1 file)))
                                                         @commit-files)))
                                     (swap! commit-files conj v))
                                   v)
                                 nested-files)
        index (parse-git-index (str project-root "/.git/index"))
        index-entries (:entries index)
        result (for [{:keys [name sha1] :as index-entry} index-entries]
                 (cond
                   (some #(= sha1 (:sha1 %)) @commit-files) [:no-change index-entry]
                   (some (fn [c]
                           (let [name-paths (clojure.string/split name #"/")
                                 file-name (last name-paths)
                                 parents (drop-last name-paths)
                                 path (:path c)]
                             (= file-name path)))
                         @commit-files) [:modified index-entry]
                   (not (some #(= % index-entry) @commit-files)) [:new index-entry]
                   :else [:unknown index-entry]))
        result (group-by first result)
        result {:modified (map second (:modified result))
                :new (map second (:new result))
                :no-change (map second (:no-change result))}]
    result))

(defn index-entry->tree-entry [index-entry]
  (let [tree-entry (select-keys index-entry [:mode :name :sha1])
        path (first (clojure.string/split (:name tree-entry) #"/"))
        tree-entry (-> tree-entry
                       (assoc :path path)
                       (dissoc :name))]
    tree-entry))

(defn build-dir [paths]
  (if (= 2 (count paths))
    (let [[parent child] paths]
      [parent [child]])
    (let [parent (first paths)]
      [parent (build-dir (rest paths))])))

(defn get-tree-entries-in-snapshot [project-root]
  (let [{:keys [new modified no-change]} (status project-root)
        index-entries (concat new modified no-change)
        paths (map (fn [index-entry]
                     (clojure.string/split (:name index-entry) #"/" ))
                   index-entries)
        dirs (map #(build-dir %) paths)
        _ (prn "dirs=" dirs)
        tree-entries (map index-entry->tree-entry  index-entries)]
    tree-entries))

(defn tree-snapshot [project-root]
  (let [tree-entries (get-tree-entries-in-snapshot project-root)
        tree-seq (flatten (map (fn [{:keys [mode path sha1]}]
                                 (let [mode-path (vd/str->seq (str mode " " path))
                                       sha1-binary (vd/hex->seq sha1)]
                                   (concat mode-path [0] sha1-binary)))
                               tree-entries))
        sha1-hex-str (hash-object "tree" tree-seq)]
    [sha1-hex-str tree-seq]))

(defn write-compressed-tree-snapshot [project-root]
  (let [[sha1-hex-str tree-seq] (tree-snapshot project-root)
        two (vd/seq->str (take 2 sha1-hex-str))
        other (vd/seq->str (drop 2 sha1-hex-str))
        file-path (util/format "%s/.git/objects/%s/%s" project-root two other)
        compressed-tree (->> tree-seq (wrap "tree") io/compress)]
    (io/squirt file-path compressed-tree)
    sha1-hex-str))

(defn commit-map [{:keys [project-root message]}]
  (let [head-commit (first (log project-root))
        head-sha1 (:sha1 head-commit)
        timestamp (.. (java.util.Date.) getTime)
        sec (quot  timestamp 1000)
        author {:person "sto <son.c.to@gmail.com>"
                :timestamp {:sec sec :timezone "-0500"}} ;;hardcoded timezone
        committer author
        cm {:message message
            :author author
            :commiter committer}
        cm (if head-sha1
             (assoc cm :parent head-sha1)
             cm)]
    cm))

(defn commit [{:keys [project-root] :as params}]
  (let [commit-map (commit-map params)
        commit-map (assoc commit-map :tree (write-compressed-tree-snapshot project-root))
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
                  "parse_git_index.c"))

  (def index (add project-root
                  "src/cljc/stigmergy/agit.cljc"))
  
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

  (write-compressed-tree-snapshot project-root)
  
  (-> (parse-blob-object project-root "519408a5747c66ff45b792fa69f0811401e2dfa5")
      vd/seq->char-seq
      vd/char-seq->str
      )
  (map n/->path ["/src/foo.bar"])
  (parse-tree-object project-root "a672dea289281cfda525e228eb5750cbc932b6cf")
  (parse-tree-object project-root "8bb8177731e14225fbcd14bb0ec99d7fb9392e46")
  (parse-tree-object project-root "f7adef825635430158f301630b0845869f61dd44")
  (parse-tree-object project-root "d3a17365deab73f0409e25868bf9643f44f85dab")
  (parse-blob-object project-root "9ea856d4823304e8f743fcab5920e708692ef6ce")
  
  ;;;
  (parse-tree-object project-root "592de38f0fa477264639cb6aa0c699942877f3c6")
  (parse-tree-object project-root "1abeaa9c97b958ff98ffd6eb734ccef7428a65c3")
  
  
  
  (hash-object "tree" (flatten (map (fn [{:keys [mode path sha1]}]
                                        (let [mode-path (vd/str->seq (str mode " " path))
                                              sha1-binary (vd/hex->seq sha1)]
                                          (concat mode-path [0] sha1-binary)))
                                    [{:mode "100644"
                                      :path "io.cljc"
                                      :sha1 "9404fd7a4dd2e63bf6acc2bf2fa8867b5c0e3b55"}
                                     {:mode "100644"
                                      :path "project.clj"
                                      :sha1 "519408a5747c66ff45b792fa69f0811401e2dfa5"}]
                                    )))


  (hash-object "tree" (flatten (map (fn [{:keys [mode path sha1]}]
                                        (let [mode-path (vd/str->seq (str mode " " path))
                                              sha1-binary (vd/hex->seq sha1)]
                                          (concat mode-path [0] sha1-binary)))
                                    [{:mode "100644"
                                      :path "io.cljc"
                                      :sha1 "9404fd7a4dd2e63bf6acc2bf2fa8867b5c0e3b55"}]
                                    )))
  
  
  (-> (cat-file project-root #_"16140255816d1b73fb118cab0a660829b3f02cd0" "ec80309431d4146761ea787f1e275461df7d1a39")
      vd/seq->char-seq
      vd/char-seq->str
      )

  
  (def gobj (ls project-root))

  (write-tree project-root)
  (let [root-tree (parse-tree-object project-root "ec80309431d4146761ea787f1e275461df7d1a39" #_"16140255816d1b73fb118cab0a660829b3f02cd0")
        tree (flatten (map (fn [{:keys [mode path sha1]}]
                             (let [mode-path (vd/str->seq (str mode " " path))
                                   sha1-binary (vd/hex->seq sha1)]
                               (concat mode-path [0] sha1-binary)))
                           root-tree))]
    (hash-object "tree" tree)
    )

  (get-tree-entries-in-snapshot project-root)
  (let [dirs [["src" ["clj" ["add.clj"]]]
              ["src" ["cljc" ["stigmergy" ["agit.cljc"]]]]]]
    ;;build dir tree
    (reduce (fn [acc dir]
              (cond
                (empty? acc) dir

                :else (let [[parent child] acc]
                        
                        ))
              )
            []
            dirs)
    )

  (defn build-tree [dir1 dir2]
    (let [[parent1 children1] dir1
          [parent2 children2] dir2]
      (cond
        (= parent1 parent2) [parent1 (build-tree children1 children2)]
        (nil? dir2) dir1
        :else [(build-tree dir1 nil) (build-tree dir2 nil)]
        )
      ))
  
  (build-tree ["src" ["clj" ["add.clj"]]]
              ["src" ["cljc" ["stigmergy" ["agit.cljc"]]]]

              )

  (defn build-forest [dirs]
    (reduce (fn [acc dir]
            (let [[parent children] dir
                  found (first (filter (fn [[p c]]
                                         (= p parent))
                                       acc))]
              (cond
                found (let [[p c] found]
                        [p (build-tree c children)])
                :else (let [[p c] (build-tree dir nil)]
                        (conj [acc] (build-tree dir nil))))))
          []
          dirs))

  (build-forest [["src" ["clj" ["add.clj"]]]
                 ["src" ["cljc" ["stigmergy" ["agit.cljc"]]]]
                 ["test" ["lambda.jpg"]]
                 ])
  
  

  (reduce (fn [acc dir]
            (let [[parent children] dir
                  found (first (filter (fn [[p c]]
                                         (= p parent))
                                       acc))]
              (cond
                found (let [[p c] found]
                        [p (build-tree c children)])
                :else (let [[p c] (build-tree dir nil)]
                        (conj [acc] (build-tree dir nil))))))
          []
          [["src" ["clj" ["add.clj"]]]
           ["src" ["cljc" ["stigmergy" ["agit.cljc"]]]]
           ["test" ["lambda.jpg"]]
           ]
          )


  [["src" [["clj" ["add.clj"]] ["cljc" ["stigmergy" ["agit.cljc"]]]]]
   ["test" ["lambda.jpg"]]]
  
  (build-dir ["src" "cljc" "stigmergy" "agit.cljc"])
  

  )


