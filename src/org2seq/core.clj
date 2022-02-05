(ns org2seq.core
  (:gen-class)
  (:require
   [clojure.java.io :as io]))

(defn find-org-files [dir]
  "Recursively lists all of the `.org` files in a given directory"
  (->> (file-seq (io/file dir))
       (filter #(re-matches #".*\.org$" (.getName %)))))

(defn get-file-metadata [regex file]
  "Uses the supplied regex to extract metadata from a given file"
  (with-open [rdr (io/reader file)]
    (->> (line-seq rdr)
         (keep #(re-matches regex %))
         (map second)
         (first))))

(defn get-file-id [file]
  "Extracts the `org-roam` UUID from a given file"
  (get-file-metadata #"(?i)^:ID:\s+([0-9a-z\-]*)" file))

(defn get-file-title [file]
  "Extracts the `#+TITLE` from a given file"
  (get-file-metadata #"(?i)^#\+TITLE:\s+(.+?)\s*$" file))

(defn build-org-id-map [dir]
  (->> (find-org-files dir)
       (map (juxt get-file-id get-file-title))
       (into {})))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
