(ns perambulate.core
  (:require [clojure.data.json :as json]
            [clj-http.client :as http]))

(def personal-access-token (slurp "personal_access_token.txt"))
(def graphql-endpoint "https://api.github.com/graphql")

;; x
(defn graphql-query [endpoint query variables personal-access-token]
  (let [query-str (json/write-str {:query query
                                   :variables variables})
        headers {:authorization (str "bearer " personal-access-token)}]
    (update (http/post endpoint {:headers headers
                                 :body query-str})
            :body #(json/read-json % true))))

;; fragments

(defn- ->fragment-get-file [custom-fragment-name]
  (format "fragment CommitTreeEntryToFile on TreeEntry {path, type%s}"
          (if custom-fragment-name
            (str "..." custom-fragment-name)
            "")))

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

(defn make-fragments
  ([depth] (make-fragments [depth nil]))
  ([depth custom-tree-entry-fragment-name]
   (str (->fragment-get-file custom-tree-entry-fragment-name)
        ", "
        (->fragment-get-files-recursive depth))))

;; queries

(def commits-by-issues-by-repo (slurp "resources/queries/commits-by-issues-by-repo.graphql"))

;; parsing

(defn tree->files
  ([tree] (tree->files tree identity))
  ([tree f] (reduce (fn [files entry]
                      (case (:type entry)
                        "tree" (concat files (tree->files (:object entry) f))
                        "blob" (conj files (f entry))
                        _ files ;; ignore tags
                        ))
                    [] (:entries tree))))

(defn- parse-commit [commit]
  (-> (assoc commit :files (tree->files (:tree commit) :path))
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

(defn- response->repo [response]
  (if-let [errors (get-in response [:body :errors])]
    (throw (Exception. (str "Error Response: " errors)))
    (-> response
        (get-in [:body :data :repository])
        (update :issues (fn [issues] (mapv ->issue (:nodes issues)))))))

;; manual testing

(defn get-repo-with-issues-and-commits [owner name]
  (-> (graphql-query
       graphql-endpoint
       (str commits-by-issues-by-repo
            ", "
            (str "fragment CustomTreeEntryData on TreeEntry {mode}, "
                 (make-fragments 5 "CustomTreeEntryData")))
       {:owner owner
        :name name}
       personal-access-token)
      response->repo))

(def r (get-repo-with-issues-and-commits "futurice" "pepperoni-app-kit"))

(pprint r)
