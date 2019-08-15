(ns stigmergy.io
  (:require [taoensso.timbre :as log :include-macros true]
            [clojure.core.async :as a :include-macros true]))

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


