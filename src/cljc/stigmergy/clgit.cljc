(ns stigmergy.clgit)

(defn init
  ([{:keys [dir]}]
   (let [git-dir ".git"
         folders ["hooks" "info" "objects/info" "objects/pack" "refs/heads" "refs/tags"]
         folders (map (fn [a-dir]
                        (let [path (str git-dir "/" a-dir)]
                          (if dir
                            (str dir "/" path)
                            path)))
                      folders)]
     folders))
  ([]
   (init {}))
  
  )
