(ns validation.core
  (:refer-clojure :exclude [replace])
  (:require [clojure.string :refer [blank? join replace capitalize]]
            [geo.core :refer [latitude? longitude? point point? point-x point-y point]]
            [slingshot.slingshot :refer [throw+]])
  (:import geo.core.IPoint))

(def ^:dynamic *email-regex*
  #"(?i)[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?")

(defn email?
  "Returns true if the email address is valid, otherwise false."
  [address] (boolean (and address (re-matches *email-regex* (str address)))))

(defn error-messages
  "Returns all error messages of the record."
  [record] (:errors (meta record)))

(defn error-messages-on
  "Returns all error messages of the record for the attribute."
  [record attribute]
  (if (vector? attribute)
    (get-in (error-messages record) attribute)
    (get (error-messages record) attribute)))

(defn error-message-on
  "Returns the first error message of the record for the attribute."
  [record attribute] (first (error-messages-on record attribute)))

(defn add-error-message-on
  "Add the error message for attribute to the record's list of error
  messages."
  [record attribute message]
  (if-not (contains? (set (error-messages-on record attribute)) message)
    (with-meta record
      (assoc-in (meta record) (flatten [:errors attribute])
                (conj (or (error-messages-on record attribute) []) message)))
    record))

(defn format-error-message [attribute error-message]
  (if (and attribute error-message)
    (str (capitalize (replace (str attribute) #"^:+" "")) " " error-message)))

(defn format-error-messages
  "Returns an array of formatted error messages."
  [errors]
  (if errors
    (flatten
     (map (fn [[attribute error-messages]]
            (map #(format-error-message attribute %) error-messages))
          errors))
    []))

(defn exception-message [record]
  (str "Validation error: " record "\n"
       (join "\n" (map #(str " -  " %) (format-error-messages (error-messages record))))))

(defn valid?
  "Returns true if the record is valid, otherwise false."
  [record]
  (let [errors (error-messages record)]
    (or (nil? errors) (empty? errors))))

(defn validate
  "Validates the record by applying the validation-fn. The function
  raises a error if the record is invalid. If the record
  is valid the function returns the record."
  [record validation-fn]
  (let [record (validation-fn record)]
    (if-not (valid? record)
      (throw+ {:type ::error :record record :errors (:errors (meta record))})
      record)))

(defn confirmation-keyword
  "Returns the keyword attribute used for confirmation."
  [attribute] (keyword (replace (str attribute "-confirmation") #"^\:+" "")))

(defn extract-value
  "Extract the validation attributes from record. Keywords are read
  with get, vectors with get-in and fn by applying the fn to the
  record."
  [record read-fn]
  (cond
   (keyword? read-fn) (get record read-fn)
   (vector? read-fn) (get-in record read-fn)
   (fn? read-fn) (read-fn record)))

(defmacro defvalidator [fn-name fn-doc args predicate-fn error-fn]
  `(defn ~fn-name ~fn-doc [~'attribute ~@args & {:as ~'options}]
     (fn [~'record]
       (let [~'value (extract-value ~'record ~'attribute)]
         (if (and (or (nil? (:if ~'options)) ((:if ~'options) ~'record))
                  (or (nil? (:unless ~'options)) (not ((:unless ~'options) ~'record))))
           (if ~predicate-fn
             ~'record
             (add-error-message-on ~'record ~'attribute ~error-fn))
           ~'record)))))

(defmacro defvalidate [name & validations]
  (let [name# name validations# validations]
    `(do
       (defn ~(symbol (str "valid-" name# "?")) [~'record]
         (valid? ((comp ~@validations#) ~'record)))
       (defn ~(symbol (str "validate-" name#)) [~'record]
         ((comp ~@validations#) ~'record))
       (defn ~(symbol (str "validate-" name# "!")) [~'record]
         (validate ~'record (comp ~@validations#))))))

(defvalidator acceptance-of
  "Validates that the record's attribute is accepted."
  []
  (and value (= value "1"))
  "must be accepted.")

(defvalidator confirmation-of
  "Validates that the record's attribute is the same as the
  confirmation attribute."
  []
  (= value ((confirmation-keyword attribute) record))
  "doesn't match confirmation.")

(defvalidator is-email
  "Validates that the record's attribute is a valid email address."
  []
  (email? value)
  "is not a valid email address.")

(defvalidator exact-length-of
  "Validates that the record's attribute is exactly length characters
  long."
  [length]
  (= (count (str value)) length)
  (format "has the wrong length (should be %d characters)." length))

(defvalidator exclusion-of
  "Validates that the record's attribute is not included in the
  sequence of values."
  [exlusions]
  (not (contains? (set exlusions) value))
  "is reserved.")

(defvalidator format-of
  "Validates that the record's attribute matches the pattern."
  [pattern]
  (and value (re-matches pattern (str value)))
  "is invalid.")

(defvalidator inclusion-of
  "Validates that the record's attribute is not included in the
  sequence of values."
  [inlusions]
  (contains? (set inlusions) value)
  "is not a valid option.")

(defvalidator latitude
  "Validates that the record's attribute is between -90.0 and 90."
  []
  (latitude? value)
  "must be between -90.0 and 90.0.")

(defvalidator longitude
  "Validates that the record's attribute is between -90.0 and 90."
  []
  (longitude? value)
  "must be between -180.0 and 180.0.")

(defn location
  "Validates that the record's attribute is a valid location."
  [attribute & {:as opts}]
  (fn [record]
    (reduce
     (fn [record validation-fn]
       (if-let [errors (validation-fn (get record attribute))]
         (add-error-message-on record attribute errors)
         record))
     record
     [#(if (not (point? %1))
         "is not a location")
      #(if (and (point? %1) (not (longitude? (point-x %1))))
         "longitude must be between -180.0 and 180.0.")
      #(if (and (point? %1) (not (latitude? (point-y %1))))
         "latitude must be between -90.0 and 90.0.")])))

(defvalidator max-length-of
  "Validates that the record's attribute is not longer than maximum
  number of characters."
  [maximum]
  (<= (count (str value)) maximum)
  (format "is too long (maximum is %d characters)." maximum))

(defvalidator min-length-of
  "Validates that the record's attribute is at least minimum number of
characters."
  [minimum]
  (>= (count (str value)) minimum)
  (format "is too short (minimum is %d characters)." minimum))

(defvalidator presence-of
  "Validates that the record's attribute is not blank."
  [] (not (blank? (str value)))
  "can't be blank.")
