(ns validation.core
  (:import (java.util Formatter))
  (:use [clojure.contrib.def :only (defvar)]
        [clojure.contrib.string :only (blank? replace-re)]
        [clojure.contrib.seq :only (includes?)]
        validation.errors))

(defvar *email-error*
  "Invalid email address."
  "The error message used if an email address is invalid.")

(defvar *email-regex*
  #"(?i)[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?"
  "The regualar expression used to validate email addresses.")

(defn email?
  "Returns true if the email address is valid, otherwise false."
  [address] (and address (re-matches *email-regex* address)))

(defn- confirmation-attribute
  "Returns the keyword attribute used for confirmation."
  [attribute] (keyword (replace-re #"^\:+" "" (str attribute "-confirmation"))))

(defn- extract-value
  "Extract the value of the record's attribute."
  [record attribute]
  (if (vector? attribute)
    (get-in record attribute)
    (get record attribute)))

(defmacro defvalidation [fn-name fn-doc arguments predicate-fn error-fn]
  (let [[record# attribute# _ options#] arguments]
    `(defn ~fn-name ~fn-doc [~record# ~attribute# & ~options#]
       (let [~options# (apply hash-map ~options#)
             ~'value (extract-value ~record# ~attribute#)]
         (if ~predicate-fn
           ~'record
           (add-error-message-on ~'record ~'attribute ~error-fn))))))

(defvalidation validate-acceptance
  "Validates that the record's attribute is accepted."
  [record attribute & options]
  (and value (= value "1"))
  "must be accepted.")

(defvalidation validate-confirmation
  "Validates that the record's attribute is the same as the
  confirmation attribute."
  [record attribute & options]
  (= value ((confirmation-attribute attribute) record))
  "doesn’t match confirmation.")

(defvalidation validate-email 
  "Validates that the record's attribute is a valid email address."
  [record attribute & options]
  (email? value)
  "is not a valid email address.")

(defvalidation validate-exact-length 
  "Validates that the record's attribute is exactly length characters
  long."
  [record attribute & options]
  (= (count value) (:is options))
  (format "has the wrong length (should be %d characters)." (:is options)))

(defvalidation validate-exclusion
  "Validates that the record's attribute is not included in the
  sequence of values."
  [record attribute & options]
  (not (includes? (:in options) value))
  "is reserved.")

(defvalidation validate-format
  "Validates that the record's attribute matches the pattern."
  [record attribute & options]
  (and value (re-matches (:with options) value))
  "is invalid.")

(defvalidation validate-inclusion
  "Validates that the record's attribute is not included in the
  sequence of values."
  [record attribute & options]
  (includes? (:in options) value)
  "is not a valid option.")

(defvalidation validate-max-length 
  "Validates that the record's attribute is not longer than maximum
  number of characters."
  [record attribute & options]
  (< (count value) (:maximum options))
  (format "is too long (maximum is %d characters)." (:maximum options)))

(defvalidation validate-min-length 
  "Validates that the record's attribute is at least minimum number of
characters."
  [record attribute & options]
  (>= (count value) (:minimum options))
  (format "is too short (minimum is %d characters)." (:minimum options)))

(defvalidation validate-presence
  "Validates that the record's attribute is not blank."
  [record attribute & options]
  (not (blank? value))
  "can't be blank.")
