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
            :body #(json/read-json % true))))

;; fragments

(def fragment-get-file "fragment CommitTreeEntryToFile on TreeEntry {path, type}")

(let [wrap "on Tree {entries {...CommitTreeEntryToFile%s}}"
      recur-wrap ", object{... %s}"]
  (defn ->fragment-get-files-recursive
    ([depth]
     (str "fragment CommitTreeToFiles "
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

;; parsing

(defn tree->files [tree]
  (reduce (fn [files entry]
            (case (:type entry)
              "tree" (concat files (tree->files (:object entry)))
              "blob" (conj files (:path entry))))
          [] (:entries tree)))

(defn- parse-commit [commit]
  (-> (assoc commit :files (tree->files (:tree commit)))
      (select-keys[:id :message :files])))

(defn- ->commits-and-pull-requests [timelineItems]
  (->> timelineItems
       (map :closer)
       (remove nil?)
       (reduce (fn [result item]
                 (let [type (:__typename item)
                       [key resolver] (case type
                                        "Commit" [:commits parse-commit]
                                        "PullRequest" [:pull-requests identity])]
                   (update result key #(conj % (resolver item)))))
               {:commits []
                :pull-requests []}))  )

(defn- ->issue [issue]
  (-> issue
      (#(merge % (->commits-and-pull-requests (get-in % [:timelineItems :nodes]))))
      (dissoc :timelineItems))  )

(defn response->repo [response]
  (-> response
      (get-in [:body :data :repository])
      (update :issues (fn [issues] (mapv ->issue (:nodes issues))))))

;; manual testing

(defn get-repo-with-issues-and-commits [owner name]
  (-> (graphql-query
       (str commits-by-issues-by-repo
            ", "
            fragments)
       {:owner owner
        :name name})
      response->repo))

(def r (get-repo-with-issues-and-commits "futurice" "pepperoni-app-kit"))

(pprint r)
