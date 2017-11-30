(ns clj-retry.core
  (:require [clojure.spec.alpha :as s]))

(s/def ::type (s/or :val keyword? :val string?))
(s/def ::types (s/coll-of ::type))
(s/def ::handler fn?)
(s/def ::max-retries pos-int?)
(s/def ::delay nat-int?)
(s/def ::plan (s/keys :req-un [::handler ::types]
                      :opt-un [::max-retries ::delay]))

(def ^:private default {:handler (constantly nil)
                        :types []
                        :max-retries 0
                        :delay 0})



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
  (failed? [result] true))



(defn with-plan [& {:keys [handler types max-retries delay]
                    :or   {max-retries 1 delay 0} :as args}]
  {:pre [(s/valid? ::plan args)]
   :post [#(instance? Plan %)]}
  (plan args))


(defn silently []
  (plan nil))



(defmacro try* [body]
  `(try
      ~body
    (catch Exception e# nil)))


(defn- will-conj [coll elm]
  (Thread/sleep 1000)
  (conj coll elm))


(defn- execute [acc g]
  (let [r (try* (g))]
    (if (failed? r)
      (do
        (will-conj acc r))
      (reduced r))))


(defn- execute-repeatedly [fns]
  (reduce execute [] fns))



(defmacro safe [command plan]
  `(let [fns# (repeatedly
                (:max-retries (plan ~plan))
                (fn [] ~command))]
     (execute-repeatedly fns#)))