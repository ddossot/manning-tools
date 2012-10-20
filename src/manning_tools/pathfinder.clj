(ns manning-tools.pathfinder)

(require '[clojure.xml :as xml]
         '[clojure.zip :as zip]
         '[clojure.string :as string]
         '[clojure.java.io :as io])

(def ^:const source-code-file-pattern #".*\.(clj|java|rb|erl|txt|xml|xsl|html)")

(defn walk [dirpath pattern]
  (doall (filter #(re-matches pattern (.getName %))
                 (file-seq (io/file dirpath)))))

(defn add-snippet-match-to-set [id-set line]
  (if-let [matches (re-find #"<start\s+id=\"([^\"]+)\"" line)]
    (conj id-set (second matches))
     id-set))

(defn fetch-snippet-ids [code-file-path]
  (with-open [file-reader (io/reader code-file-path)]
    (reduce add-snippet-match-to-set (hash-set) (line-seq file-reader))))

(defn add-snippets-to-code-map [code-map code-file-path]
  (let [snippet-ids (fetch-snippet-ids code-file-path)]
    (if (empty? snippet-ids)
      code-map
      (reduce #(assoc %1 %2 code-file-path) code-map snippet-ids))))

(defn walk-source-code [code-directory-path]
  (reduce #(add-snippets-to-code-map %1 (.getPath %2)) {} (walk code-directory-path source-code-file-pattern)))

(defn add-codelink-to-set [linkend-set element]
  (if (= :codelink (:tag element))
    (conj linkend-set element)
     linkend-set))

(defn fetch-codelinks [docbook-file-path]
  (reduce add-codelink-to-set (hash-set) (xml-seq (xml/parse docbook-file-path))))

(defn relativize-path [docbook-file-path source-code-file-path]
  (let [docbook-dir-path (.getParent (io/file docbook-file-path))]
    (.substring (string/replace source-code-file-path docbook-dir-path "") 1)))

(defn -main
  [& args]
  (let [docbook-file-path (first args)
        codelinks (fetch-codelinks docbook-file-path)
        code-map  (walk-source-code (second args))]
    (doseq [codelink codelinks]
      (let [codelink-attrs   (:attrs codelink)
            codelink-linkend (:linkend codelink-attrs)]
        (if (nil? (:file codelink-attrs))
          (println "Missing file attribute for:" codelink-linkend "is:" (relativize-path docbook-file-path (code-map codelink-linkend)))
          ; TODO also output linkends mismatches
          )))))

