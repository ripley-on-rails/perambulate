(ns busfactor.core
  (:require [clojure.edn :as edn]
            [clojure.data.json :as json]
            [clj-http.client :as http]))

(def personal-access-token (slurp "personal_access_token.txt"))
(def graphql-endpoint "https://api.github.com/graphql")

(defn graphql-query [query]
  (let [query-str (format "{ \"query\": \" %s \" }"
                          (clojure.string/escape query
                                                 {\" "\\\""}))
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

;; manual testing

(defn get-issues-by-repo [owner name]
  (graphql-query
   (format
    (str "query{repository(owner: \"%s\", name: \"%s\") {issues(first:1) {totalCount, nodes {title, closed, state, url, labels(first:10) {nodes {color, name} }, timelineItems(first:100) {nodes {... on ClosedEvent {closer {__typename, ... on Commit {id, message, tree {...GetAllFilesRecursive } }, ... on PullRequest{number, title, files(first:100) {totalCount, nodes {path} } } }}} } }}}}, "
         fragments)
    owner name)))

(def r (get-issues-by-repo "futurice" "pepperoni-app-kit"))

(pprint (:body r))
