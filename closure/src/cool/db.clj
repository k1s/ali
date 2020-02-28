(ns closure.db
  (:require [clojure.java.jdbc :as sql]))

(defn db-spec [password] {:dbtype     "postgresql"
                          :dbname     "prod_bumblebee_db"
                          :host       "localhost"
                          :password   password
                          :port       6432
                          :ssl        true
                          :sslmode    "require"
                          :sslfactory "org.postgresql.ssl.NonValidatingFactory"})

(defn get-processes [db]
  (sql/with-db-connection
    [con db]
    (sql/query con ["SELECT set_user('app_owner');"])
    (sql/query con ["SELECT site, loading_unit, process_id, processing_state, last_error
                 FROM bee_data.event e
                 WHERE e.type = 'de.zalando.logistics.laas.hecate.sortpack-loadingunit_process_status_changed'
                 AND e.processing_state = 'PROCESSING(/processes/status/in-progress)'
                 AND last_modified <= now() - INTERVAL '30 minutes'
                 ORDER BY site, last_modified;"])))


