(ns closure.load
  (:gen-class)
  (:use [slingshot.slingshot :only [throw+ try+]]
        [clojure.java.shell :only [sh]])
  (:require [clojure.string :as str]
            [clj-http.client :as client]
            [clojure.tools.logging :as log]
            [cheshire.core :refer :all]))

(defn ztoken []
  (str/trim (:out (sh "ztoken"))))

(def tok
  "tok.py -a sideline-load -r eu-central-1 -s uid,hecate.service.read,hecate.service.write,sideline-load.process.read,sideline-load.process.write")

(def token
  (:out (apply sh (str/split tok #" "))))

(def tok-bb
  "tok.py -a bumblebee-prod -r eu-central-1 -s uid,item.item.read,location.location.read,logistics-article.read")

(def token-bb
  (:out (apply sh (str/split tok-bb #" "))))

(def tenant {"X-Tenant-ID" "810d1d00-4312-43e5-bd31-d8373fdd24c7"})

(defn add-tenant [headers]
  (merge headers tenant))

(defn tasks-url [{:keys [site process]}]
  (str "https://hecate-live.laas.zalan.do/api/sites/" site
       "/processes/load-location/" process "/tasks"))

(defn process-url [site]
  (str "https://sideline-load-prod.logistics.zalan.do/internal/sites/" site "/processes/"))

(defn items-url [site]
  (str "https://item.wpi.zalan.do/api/sites/" site "/items"))

(defn process-continue-url [{:keys [site process]}]
  (str (process-url site) process "/continue"))

(defn tasks-current-url [env]
  (str (tasks-url env) "?state=current"))

(defn tasks-id-url [env id]
  (str (tasks-url env) "/" id))

(defn body-json [response]
  (parse-string (:body response)))

(defn log-throwable [f & args]
  (try+ (apply f args)
        (catch Object _
          (log/error (:throwable &throw-context)))))

(defn get-request
  ([url token] (log-throwable client/get url {:oauth-token token}))
  ([url token params headers] (log-throwable client/get url {:oauth-token  token
                                                             :query-params params
                                                             :headers      headers})))

(defn patch-request [url headers body]
  (log-throwable client/patch url {:oauth-token token
                                   :body        body
                                   :headers     headers}))

(defn get-items-count [{:keys [site location]}]
  (let [url (items-url site)
        params {"limit"    42
                "location" (str "/sites/" site "/locations/" location)}]
    (count (body-json (get-request url token-bb params tenant)))))

(defn continue-process-request [url headers]
  (try+ (client/put url {:oauth-token token
                         :headers     headers})
        true
        (catch [:status 500] _
          (log/info "Unsuccessful continue for" url)
          false)
        (catch Object _
          (log/error (:throwable &throw-context)))))

(defn continue-process [env]
  (let [url (process-continue-url env)]
    (log/debug "Continue for" url)
    (continue-process-request url tenant)))

(defn cancel-task [env task etag]
  (let [url (tasks-id-url env task)
        headers {"If-Match"     etag,
                 "Content-Type" "application/json"}
        body (generate-string {"task_status" "/tasks/status/cancelled"})
        request (patch-request url (add-tenant headers) body)]
    (:body request)))

(defn task-by-process [env]
  (as-> env v
        (tasks-current-url v)
        (get-request v token)
        (body-json v)
        (get v "tasks")
        (first v)
        (str/split v #"/\\*")
        (last v)))

(defn etag-ql [env task]
  (let [url (tasks-id-url env task)
        response (get-request url token)
        headers (:headers response)
        body (body-json response)]
    {:etag (get headers "ETag")
     :ql   (get-in body ["task" "meta" "quality_label"])}))

(defn task-info [env]
  (let [task-id (task-by-process env)]
    (merge {:task task-id} (etag-ql env task-id))))

(defn cancel-process [env info]
  (let [task-id (:task info)
        etag (:etag info)]
    (cancel-task env task-id etag)))

(defn handle-process
  "env is just a process info like
      {:process \"11e4c8f0-d8b4-11e9-a26a-b7bbf39b7ef8\"
       :location \"00040552760485074890\"
       :site     \"lodz\"}"
  [env]
  (if (continue-process env)
    (log/info "Successful continue for" env)
    (let [info (task-info env)]
      (if (> (get-items-count env) 0)
        (log/error "Not empty location, try again or do it manually" (merge env info))
        (do
          (prn "TASK INFO: " info)
          (log/info "Empty location. Cancel and continue for" env)
          (cancel-process env info)
          (continue-process env))))))







