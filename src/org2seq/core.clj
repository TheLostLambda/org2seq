(ns org2seq.core
  (:gen-class)
  (:require
   [babashka.fs :as fs]
   [clojure.java.io :as io]
   [clojure.string :as str]))

;; Some Global Parameters --------------------------------------------------------------------------

(def old-asset-dir ".orgimg/")
(def new-asset-dir "assets/")

;; Utility Functions -------------------------------------------------------------------------------

(defn uuid
  "Generate a new UUID string"
  []
  (.toString (java.util.UUID/randomUUID)))

(defn link-unescape
  "Take the contents of an org link and unescape escaped characters"
  [link]
  (str/replace link #"\\(\\|\[|\])" second))

;; File Handling -----------------------------------------------------------------------------------

(defn valid-directory?
  "Ensures that `dir` exists and is really a directory"
  [dir]
  (let [f (fs/file dir)]
    (and (fs/exists? f) (fs/directory? f))))

(defn find-org-files
  "Recursively lists all of the `.org` files in a given directory"
  [dir]
  (->> (file-seq (fs/file dir))
       (filter #(= "org" (fs/extension %)))))

(defn replace-in-file
  "Runs `str/replace` with the supplied regex and replacement on a given file"
  [file regex replacement]
  (spit file (str/replace (slurp file) regex replacement)))

(defn copy-asset
  "Uses the parent file path to establish a local assets directory, copying the linked file there"
  [parent-file link]
  (let [parent-dir (-> parent-file fs/file fs/canonicalize fs/parent)
        src-file (-> link link-unescape fs/file fs/expand-home)
        src-file (if (fs/relative? src-file) (fs/file parent-dir src-file) src-file)
        dest-dir (fs/file parent-dir new-asset-dir)
        file-name (fs/file-name src-file)
        uniq-name (str (fs/strip-ext file-name) " (" (uuid) ")." (fs/extension file-name))
        dest-file (fs/file dest-dir uniq-name)]
    (fs/create-dirs dest-dir)
    (fs/copy (fs/path src-file) (fs/path dest-file))
    [uniq-name file-name]))

;; Metadata Extraction -----------------------------------------------------------------------------

(defn get-file-metadata
  "Uses the supplied regex to extract metadata from a given file"
  [regex file]
  (with-open [rdr (io/reader file)]
    (->> (line-seq rdr)
         (pmap #(re-matches regex %))
         (remove nil?)
         (pmap second)
         (first))))

(defn get-file-id
  "Extracts the `org-roam` UUID from a given file"
  [file]
  (get-file-metadata #"(?i)^:ID:\s+([0-9a-z\-]*)" file))

(defn get-file-title
  "Extracts the `#+TITLE` from a given file"
  [file]
  (get-file-metadata #"(?i)^#\+TITLE:\s+(.+?)\s*$" file))

(defn build-org-id-map
  "Constructs a mapping of file IDs and titles"
  [files]
  (->> (pmap (juxt get-file-id get-file-title) files)
       (into {})))

;; Link Processing ---------------------------------------------------------------------------------

(defn fix-id-link
  "Transform an ID link into a title link"
  [id-map dest name]
  (str "[[" (get id-map dest) "]" (if (some? dest) (str "[" name "]") "") "]"))

(defn fix-pdf-link
  "Move PDFs into `assets/` and link them with a name detected by Logseq"
  [file dest]
  (let [[copied-name original-name] (copy-asset file dest)]
    (str "[[" (fs/file new-asset-dir copied-name) "][" original-name "]]")))

(defn fix-img-link
  "Move PDFs into `assets/` and link them with a name detected by Logseq"
  [file dest]
  (let [[copied-name _] (copy-asset file dest)]
    (str "[[" (fs/file new-asset-dir copied-name) "]]")))

(defn fix-link
  "Differentiates different link types and applies type-specific fixes"
  [id-map file [s type dest name]]
  (cond (= type "id") (fix-id-link id-map dest name)
        (or (= type "http") (= type "https")) s
        (= (fs/extension dest) "pdf") (fix-pdf-link file dest)
        (str/starts-with? dest old-asset-dir) (fix-img-link file dest)
        :else (do (println "Unknown Link Type:" s) s)))

(defn fix-all-links
  "Applies fixes to all of the links in a file"
  [id-map file]
  (let [link-re #"(?s)\[\[(?:(\S+?):)?(.+?)\](?:\[(.+?)\])?\]"
        fixer (partial fix-link id-map file)]
    (replace-in-file file link-re fixer)))

;; Miscellaneous Fixes -----------------------------------------------------------------------------

(defn remove-roam-ref
  "Converts `#+ROAM_REF` tags into regular links"
  [file]
  (replace-in-file file #"(?i)#\+ROAM_REF: " ""))

;; Main Program Logic ------------------------------------------------------------------------------

(defn -main [dir]
  (print "Building UUID mapping...")
  (flush)
  (if (valid-directory? dir)
    (let [files (find-org-files dir)
          id-map (build-org-id-map files)]
      (println "done")
      (print "Fixing file links...")
      (flush)
      (doall (pmap (partial fix-all-links id-map) files))
      (println "done")
      (print "Removing #+ROAM_REFs...")
      (flush)
      (doall (pmap remove-roam-ref files))
      (println "done"))
    (let [path (fs/canonicalize (fs/file dir))]
      (printf "\nSorry, but %s is not a valid directory!\n" dir)
      (printf "Check that `%s` exists\n" path)))
  (shutdown-agents))
