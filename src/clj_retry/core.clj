(ns clj-retry.core
  (:require [clojure.spec.alpha :as s]))

(s/def ::handler fn?)
(s/def ::max-retries nat-int?)
(s/def ::delay nat-int?)
(s/def ::plan (s/keys :req-un [::handler]
                      :opt-un [::max-retries ::delay]))

(def ^:private default {:handler (constantly nil)
                        :max-retries 1
                        :delay 0})


(def ^:dynamic *handler* nil)
(def ^:dynamic *delay* 1)
(def ^:dynamic *max-retries* 1)
(def ^:dynamic *retries* 0)



(s/valid? ::plan default)


(defprotocol IHandler
  (failed? [ex])
  (info [ex]))




(extend-protocol IHandler
  clojure.lang.ExceptionInfo
  (failed? [ex] true)
  (info [ex] (ex-data ex))
  Exception
  (failed? [ex] true)
  (info [ex] (.getMessage ex))
  Object
  (failed? [_] false)
  (info [_] nil)
  nil
  (failed? [_] false)
  (info [_] nil))


(defmacro try* [body]
  `(try
      ~body
    (catch Exception e# e#)))


(defn- will-conj [coll v]
  (Thread/sleep *delay*)
  (conj coll v))


(defn- execute [acc g]
  (let [r (try* (g))]
    (if (and (not= *retries* *max-retries*) (failed? r))
      (do
        (set! *retries* (inc *retries*))
        (try* (*handler* (info r)))
        (will-conj (conj) r))
      (reduced r))))


(defn- execute-repeatedly [fns]
  (let [val (reduce execute (conj) fns)]
    (if (coll? val)
      (first val)
      val)))



(defmacro do* [command {:keys [handler max-retries delay]
                         :or   {max-retries 0 delay 0} :as plan}]
  `(binding [*delay* ~delay
             *handler* ~handler
             *max-retries* ~max-retries
             *retries* (+)]
     (execute-repeatedly
       (repeatedly
         (inc *max-retries*)
         (fn [] ~command)))))