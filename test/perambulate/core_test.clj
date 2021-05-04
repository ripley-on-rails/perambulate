(ns perambulate.core-test
  (:require [clojure.test :refer :all]
            [perambulate.core :refer :all]))

(def slurp-edn (comp read-string slurp))

(deftest tree->files-test
  (testing "Tree Parsing"
    (let [raw-data (slurp-edn "test/resources/commit-tree.edn")
          expected (slurp-edn "test/resources/expected-files.edn")]
      (is (= (tree->files raw-data :path) expected)))))
