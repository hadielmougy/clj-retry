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

(defrecord Plan [handler types max-retries delay])


(defprotocol IPlan
  (plan [spec]))

(defprotocol IHandler
  (failed? [result]))


(extend-protocol IPlan
  Plan
  (plan [spec] spec)
  clojure.lang.PersistentHashMap
  (plan [spec] (map->Plan spec))
  nil
  (plan [spec] (plan default)))



(extend-protocol IHandler
  clojure.lang.ExceptionInfo
  (failed? [result] true)
  Object
  (failed? [result] false)
  nil
  (failed? [result] false))




(defn with-plan [& {:keys [handler max-retries delay]
                    :or   {max-retries 0 delay 0} :as args}]
  {:pre [(s/valid? ::plan args)]
   :post [#(instance? Plan %)]}
  (plan args))

(defn silently []
  (plan nil))



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
      (binding [*retries* (inc *retries*)]
        (try* (*handler* r))
        (will-conj (conj) r))
      (reduced r))))


(defn- execute-repeatedly [fns]
  (let [val (reduce execute (conj) fns)]
    (if (coll? val)
      (first val)
      val)))



(defmacro safe [command plan]
  `(let [p# (plan ~plan)]
     (binding [*delay* (:delay p#)
               *handler* (:handler p#)
               *max-retries* (:max-retries p#)
               *retries* (+)]
       (execute-repeatedly
         (repeatedly
           (inc *max-retries*)
           (fn [] ~command))))))