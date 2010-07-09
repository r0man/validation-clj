(ns validation.core
  (:use [clojure.contrib.def :only (defvar)]))

(defvar *email-error*
  "Invalid email address."
  "The error message used if an email address is invalid.")

(defvar *email-regex*
  #"(?i)[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?"
  "The regualar expression used to validate email addresses.")

(defn valid-email? [address]
  (and address (re-matches *email-regex* address)))

;; (defn validate-email [record attribute & options]
;;   (if-not (re-matches *email-regex* (attribute record))
;;     *email-error*))

;; (defn validate-presence-of
;;   "Validates that the specified attribute is not blank."
;;   [record attribute & options]
;;   (let [options (apply hash-map options)]
;;     )
;;   )

;; (validate-email "roman.scherer@burningswell")

;; (defvalidation user
;;   (validate-presence-of :name)
;;   (validate-email :email)
;;   )

