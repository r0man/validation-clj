(ns validation.core
  (:use [clojure.contrib.def :only (defvar)]
        [clojure.contrib.string :only (blank?)]))

(defvar *email-error*
  "Invalid email address."
  "The error message used if an email address is invalid.")

(defvar *email-regex*
  #"(?i)[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?"
  "The regualar expression used to validate email addresses.")

(defn error-messages
  "Returns all error messages of the record."
  [record] (:errors (meta record)))

(defn error-messages-on
  "Returns all error messages of the record for the attribute."
  [record attribute] (attribute (error-messages record)))

(defn error-message-on
  "Returns the first error message of the record for the attribute."
  [record attribute] (first (error-messages-on record attribute)))

(defn add-error-message-on
  "Add the error message on the record's attribute."
  [record attribute message]  
  (with-meta record
    (assoc-in (meta record) [:errors attribute]
              (seq (set (conj (error-messages-on record attribute) message))))))

(defn email?
  "Returns true if the email address is valid, otherwise false."
  [address] (and address (re-matches *email-regex* address)))

(defn validate-email [record attribute & options]
  (let [options (apply hash-map options)]
    (if (email? (attribute record))
      record
      (add-error-message-on
       record attribute
       (or (:message options) "must be an email address.")))))

(defn validate-presence-of
  "Validates that the specified attribute is not blank."
  [record attribute & options]
  (let [options (apply hash-map options)]
    (if (blank? (attribute record))
      (add-error-message-on
       record attribute
       (or (:message options) "can't be blank."))
      record)))


;; (conj nil 1)
;; (meta (add-error-message-on {:email "bob"} :email "is not a valid email address."))
;; (assoc-in )
;; (clojure.zip/map-zip)

;; zipmap

;; (validate-email "roman.scherer@burningswell")

;; (defvalidation user
;;   (validate-presence-of :name)
;;   (validate-email :email)
;;   )

