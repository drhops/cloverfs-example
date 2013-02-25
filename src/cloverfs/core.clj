(ns cloverfs.core
  (:use [clojure.string :only [join split]]))

(defn create-directory [] {:children {}})
(defn create-file [] {:contents ""})

(defn name-from-path [path]
  (last (split path #"\/")))

(defn find-parent [path node]
  (let [[next more] (split path #"\/" 2)]
    (if more
      (recur more (get-in node [:children (keyword next)]))
      node)))

(defn find-node [path node]
  (let [parent (find-parent path node)]
    (get-in parent [:children (keyword (name-from-path path))])))

(defn path-keys [path]
  (map keyword (flatten (map (fn [x] ["children", x]) (split path #"\/")))))

(defn add-node [root path node]
  (assoc-in root (path-keys path) node))

(defn update-file-contents [root path contents]
  (assoc-in root (concat (path-keys path) [:contents]) contents))

(def commands {
  :mkdir
  (fn [root args]
    (let [path args,
          root (add-node root path (create-directory))]
      (println "added directory:" path)
      root))
  :touch
  (fn [root args]
    (let [path args,
          root (add-node root path (create-file))]
      (println "added file:" path)
      root))
  :cat
  (fn [root args]
    (let [path args,
          file (find-node path root)]
      (if (not file) (throw (Exception. "file doesn't exist")))
      (println (:contents file))
      root))
  :append
  (fn [root args]
    (let [[path appendage] (split args #"\s" 2),
          file (find-node path root)]
      (update-file-contents root path (str (:contents file) appendage))))
  :cp
  (fn [root args]
    (let [[srcpath dstpath] (split args #"\s" 2),
          srcfile (find-node srcpath root)]
      (update-file-contents root dstpath (:contents srcfile))))
  })

(defn continue? [line]
  (not (= "" line)))

(defn execute-line [root line]
  (let [[first rest] (split line #"\s" 2)]
    (try
      (((keyword first) commands) root rest)
      (catch Exception e
        (println "error: " (.getMessage e))
        root)
      )))

(defn shell [root]
   (println "$: ")
   (let [line (read-line)]
     (if (continue? line)
       (recur (execute-line root line))
       root)))

(defn main "main function" []
  (println "Running interactively..")
  (shell (create-directory)))
