(ns stigmergy.node
  (:require [stigmergy.tily :as tily]
            [clojure.string :as str]
            [taoensso.timbre :as log :include-macros true]))

(defn file? [f-or-d ]
  (tily/some-in? :file/name (keys f-or-d)))

(defn dir? [f-or-d]
  (-> f-or-d file? not))

(defn root? [node]
  (-> node :parent nil?))

(defn str->path [f]
  (let [path (clojure.string/split f #"/")]
    (if (-> path first str/blank?)
      (-> path rest vec)
      path)))

(defn get-name [node]
  (let [name (cond
               (dir? node) (-> node (dissoc :parent :file/name) keys first)
               (file? node) (:file/name node)
               :else "invalid node")]
    name))

(defn throw-exception [msg]
  #?(:cljs (throw (js/Error. msg)))
  #?(:clj (throw (Exception. msg)))
  )

(defn get-children [node]
  (if (dir? node)
    (-> node  vals first)
    (throw-exception "only directory can have children")))

(defn get-parent-path [full-path paths]
  (->> full-path (drop-last (count paths)) vec))

(defn get-node-path [node] 
  (let [parent-path-str (str "/" (str/join "/" (:parent node)))]
    (if (file?  node)
      (let [file-name (:file/name node)]
        (str parent-path-str "/" file-name))
      (let [dir-name (-> node (dissoc :parent) keys first)]
        (if (= "/" parent-path-str)
          (str "/" dir-name)
          (str parent-path-str "/" dir-name)))))
  )

(defn ->node-helper [full-path paths node]
  (cond
    (empty? paths) node
    (= 2 (count paths)) (let [[path file] paths
                              file-parent-path (-> full-path drop-last vec)
                              n (assoc node path [{:file/name file
                                                   :parent file-parent-path}])
                              parent-path (get-parent-path full-path paths)]
                          (if (empty? parent-path)
                            n
                            (assoc n :parent parent-path)))
    :else (let [p (first paths)
                n (assoc node
                         p
                         [(->node-helper full-path (rest paths) {})])
                parent-path (get-parent-path full-path paths)]

            (if (empty? parent-path)
              n
              (let [n2 (assoc n :parent parent-path)]
                n2)))))

(defn path->node [paths]
  (->node-helper paths paths {}))

(clojure.set/intersection #{1 2} #{2 3})

(defn index-of [nodes k]
  (->> (tily/with-index nodes)
       (filter (fn [[i node]]
                 (contains? node k)))
       ffirst))

(defn join-node [a b]
  (cond
    (and (sequential? a) (map? b)) (let [k (-> b (dissoc :parent :file/name) ffirst)
                                         i (index-of a k)]
                                     (if i
                                       (let [c (a i)
                                             d (join-node c b)
                                             e (-> (tily/drop-nth a i)
                                                   (tily/insert-at i d))]
                                         e)
                                       (let [r (into a [b])]
                                         r)))
    (and (map? a) (map? b)) (let [a-keys (keys a)
                                  b-keys (keys b)
                                  common-keys (clojure.set/intersection (set a-keys) (set b-keys))
                                  common-keys (disj common-keys :parent :file/name)]
                              (if (empty? common-keys)
                                [a b]
                                (let [ab (merge a b)
                                      ab2 (into {} (for [ck common-keys
                                                         :let [av (a ck)
                                                               bv (b ck)]
                                                         :when (not= av bv)]
                                                     (cond
                                                       (and (map? av) (map? bv)) [ck (join-node av bv)]
                                                       (and (sequential? av)
                                                            (sequential? bv)) (let [av-bv (into av bv)
                                                                                    joined (reduce join-node
                                                                                                   av-bv)]
                                                                                (if (sequential? joined)
                                                                                  [ck  joined]
                                                                                  [ck [joined]]))
                                                       :else [ck (conj [av] bv)])))]
                                  (merge ab ab2))))
    :else (log/warn "unable to join nodes " a b)))

(comment
  (def files ["/scramblies/resources/public/index.html" "/scramblies/src/clj/scramblies/core.clj" "/scramblies/src/clj/scramblies/server.clj" "/scramblies/src/clj/user.clj" "/scramblies/src/cljs/scramblies/core.cljs" "/scramblies/test/scramblies/tests.clj" "/scramblies/README.md" "/scramblies/project.clj"])

  (-> "scramblies/src/cljs/scramblies/core.cljs"
      str->path path->node)

  (def nodes (mapv (fn [n]
                     (-> n str->path path->node)) files))
  (reduce join-node nodes)
  )
