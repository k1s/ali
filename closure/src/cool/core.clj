(ns cool.core
  (:gen-class)
  (:use [cool.load :as load]
        [cool.db :as db]
        [clojure.set :as set]))

(defn -main [& args])

(defn flow []
  (let [db (db/db-spec (ztoken))
        processes (db/get-processes db)]
    (doseq [p processes]
      (load/handle-process (set/rename-keys p {:process_id   :process
                                               :loading_unit :location})))))
(flow)
