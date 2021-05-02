(ns busfactor.core
  (:require [clojure.edn :as edn]
            [clojure.data.json :as json]
            [clj-http.client :as http]))

(def personal-access-token (slurp "personal_access_token.txt"))
(def graphql-endpoint "https://api.github.com/graphql")

(defn graphql-query [query variables]
  (let [query-str (json/write-str {:query query
                                   :variables variables})
        headers {:authorization (str "bearer " personal-access-token)}]
    (update (http/post graphql-endpoint {:headers headers
                                         :body query-str})
            :body json/read-str)))

;; fragments

(def fragment-get-file "fragment GetAllFile on TreeEntry {path, type }")

(let [wrap "on Tree {entries {...GetAllFile%s}}"
      recur-wrap ", object{... %s}"]
  (defn ->fragment-get-files-recursive
    ([depth]
     (str "fragment GetAllFilesRecursive "
          (->fragment-get-files-recursive depth (format wrap ""))))
    ([depth s]
     (if (pos? depth)
       (->fragment-get-files-recursive (dec depth)
                                       (->> s
                                            (format recur-wrap)
                                            (format wrap)))
       s))))

(def fragments (str fragment-get-file ", " (->fragment-get-files-recursive 3)))

;; queries

(def commits-by-issues-by-repo (slurp "resources/queries/commits-by-issues-by-repo.graphql"))

;; manual testing

(defn get-issues-by-repo [owner name]
  (graphql-query
   (str commits-by-issues-by-repo
        ", "
        fragments)
   {:owner owner
    :name name}))

(def r (get-issues-by-repo "futurice" "pepperoni-app-kit"))

(pprint (:body r))
