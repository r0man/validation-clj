(ns validation.predicates
  (:use [clojure.contrib.def :only (defvar-)]))

(defvar- *email-regex*
  #"(?i)[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?"
  "The regualar expression used to validate email addresses.")

(defn email?
  "Returns true if the email address is valid, otherwise false."
  [address] (and address (re-matches *email-regex* address)))

(defn latitude?
  "Returns true if the number is in the range of the latitude axis."
  [number] (and (>= number -90.0) (<= number 90.0)))

(defn longitude?
  "Returns true if the number is in the range of the longitude axis."
  [number] (and (>= number -180.0) (<= number 180.0)))

