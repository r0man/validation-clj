(ns validation.core
  (:use [clojure.contrib.def :only (defvar)]
        [clojure.contrib.string :only (blank?)]
        [clojure.contrib.seq :only (includes?)]
        validation.errors))

;; (defprotocol Validation
;;   (validate [record] "Validate the record.")
;;   (valid?   [record] "Returns true if the record is valid,
;;   otherwise false."))

(defn- extract-message
  "Extract the message from the options, or return the default."
  [options & [default]] (or (:message options) default))

(defvar *email-error*
  "Invalid email address."
  "The error message used if an email address is invalid.")

(defvar *email-regex*
  #"(?i)[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?"
  "The regualar expression used to validate email addresses.")

(defn email?
  "Returns true if the email address is valid, otherwise false."
  [address] (and address (re-matches *email-regex* address)))

(defn validate-acceptance-of
  "Returns a validation function that checks if the attribute was
  accepted, e.g. the terms of service."
  [attribute & options]
  (let [options (apply hash-map options)
        message (extract-message options "must be accepted.")]
    (fn [record]
      (if (= (attribute record) "1")
        record
        (add-error-message-on record attribute message)))))

(defn validate-confirmation-of
  "Returns a validation function that checks if the first attribute is
  equal to the second attribute."
  [attribute-1 attribute-2 & options]
  (let [options (apply hash-map options)
        message (extract-message options "doesn’t match confirmation.")]
    (fn [record]      
      (cond
       (= (attribute-1 record) (attribute-2 record)) record
       :else (add-error-message-on record attribute-1 message)))))

(defn validate-email
  "Returns a validation fn that checks if the specified attribute is
  an email address."
  [attribute & options]
  (let [options (apply hash-map options)
        message (extract-message options "must be an email.")]
    (fn [record]
      (let [value (attribute record)]
        (cond
         (and (:allow-blank options) (blank? value)) record
         (email? value) record
         :else (add-error-message-on record attribute message))))))

(defn validate-exclusion-of
  "Returns a validation fn that checks if the specified attribute is
  not included in the sequence of values."
  [attribute values & options]
  (let [options (apply hash-map options)
        message (extract-message options "is reserved.")]
    (fn [record]
      (let [value (attribute record)]
        (cond
         (and (:allow-blank options) (blank? value)) record
         (includes? values value) (add-error-message-on record attribute message)
         :else record)))))

(defn validate-format-of
  "Returns a validation fn that checks if the specified attribute
  matches the pattern."
  [attribute pattern & options]
  (let [options (apply hash-map options)
        message (extract-message options "is invalid.")]
    (fn [record]
      (let [value (attribute record)]
        (cond
         (and (:allow-blank options) (blank? value)) record
         (and value (re-matches pattern value)) record
         :else (add-error-message-on record attribute message))))))

(defn validate-inclusion-of
  "Returns a validation fn that checks if the specified attribute is
  included in the sequence of values."
  [attribute values & options]
  (let [options (apply hash-map options)
        message (extract-message options "is not included in the list.")]
    (fn [record]
      (let [value (attribute record)]
        (cond
         (and (:allow-blank options) (blank? value)) record
         (includes? values value) record
         :else (add-error-message-on record attribute message))))))

(defn validate-presence-of
  "Returns a validation fn that checks if the specified attribute is
  not blank."
  [attribute & options]
  (let [options (apply hash-map options)
        message (extract-message options "can't be blank.")]
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

