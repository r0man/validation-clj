(ns validation.core
  (:use [clojure.contrib.def :only (defvar)]
        [clojure.contrib.string :only (blank?)]
        validation.errors))

;; (defprotocol Validation
;;   (validate [record] "Validate the record.")
;;   (valid?   [record] "Returns true if the record is valid, otherwise false."))

(defvar *email-error*
  "Invalid email address."
  "The error message used if an email address is invalid.")

(defvar *email-regex*
  #"(?i)[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?"
  "The regualar expression used to validate email addresses.")

(defn email?
  "Returns true if the email address is valid, otherwise false."
  [address] (and address (re-matches *email-regex* address)))

(defn validate-email
  "Returns a validation fn that checks if the specified attribute is
  an email address."
  [attribute & options]
  (let [options (apply hash-map options)
        message (or (:message options) "must be an email.")]
    (fn [record]
      (let [value (attribute record)]
        (cond
         (and (:allow-blank options) (blank? value)) record
         (email? value) record
         :else (add-error-message-on record attribute message))))))

(defn validate-presence-of
  "Returns a validation fn that checks if the specified attribute is
  not blank."
  [attribute & options]
  (let [options (apply hash-map options)
        message (or (:message options) "can't be blank.")]
    (fn [record]
      (if (blank? (attribute record))
        (add-error-message-on record attribute message)
        record))))

;; (defn validate-length-of [record attribute & options]
;;   (let [options (apply hash-map options)]
;;     (if (email? (attribute record))
;;       record
;;       (add-error-message-on
;;        record attribute
;;        (or (:message options) "must be an email address.")))))

;; (defn validate-numericality-of
;;   "Validates whether the value of the specified attribute is numeric."
;;   [record attribute & options]
;;   )

;; (defn validate-user [user]
;;   (-> user
;;       (validate-presence-of :name)
;;       (validate-presence-of :email)
;;       (validate-email :email)))

