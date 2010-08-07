(ns validation.core
  (:use [clojure.contrib.string :only (blank? join replace-re)]
        [clojure.contrib.seq :only (includes?)]
        [clojure.contrib.error-kit :only (deferror throw-msg raise)]
        validation.errors
        validation.predicates))

(deferror *validation-error* [] [record]
  {:msg (exception-message record)
   :unhandled (throw-msg IllegalArgumentException)})

(defn validate
  "Validates the record by applying the validation-fn. The function
  raises a *validation-error* if the record is invalid. If the record
  is valid the function returns the record."
  [record validation-fn]
  (let [record (validation-fn record)]
    (if-not (valid? record)
      (raise *validation-error* record)
      record)))

(defn- confirmation-keyword
  "Returns the keyword attribute used for confirmation."
  [attribute] (keyword (replace-re #"^\:+" "" (str attribute "-confirmation"))))

(defn extract-value
  "Extract the value of the record's attribute."
  [record attribute]
  (if (vector? attribute)
    (get-in record attribute)
    (get record attribute)))

(defmacro defvalidation [fn-name fn-doc arguments predicate-fn error-fn & validation-options]  
  `(defn ~fn-name ~fn-doc [~'record ~'attribute ~@arguments & ~'options]
     (let [~'options (apply hash-map ~'options)
           ~'value (extract-value ~'record ~'attribute)]
       (if ~predicate-fn
         ~'record
         (add-error-message-on ~'record ~'attribute ~error-fn)))))

(defvalidation validate-acceptance
  "Validates that the record's attribute is accepted."
  []
  (and value (= value "1"))
  "must be accepted.")

(defvalidation validate-confirmation
  "Validates that the record's attribute is the same as the
  confirmation attribute."
  []
  (= value ((confirmation-keyword attribute) record))
  "doesn't match confirmation.")

(defvalidation validate-email 
  "Validates that the record's attribute is a valid email address."
  []
  (email? value)
  "is not a valid email address.")

(defvalidation validate-exact-length 
  "Validates that the record's attribute is exactly length characters
  long."
  [length]
  (= (count value) length)
  (format "has the wrong length (should be %d characters)." length))

(defvalidation validate-exclusion
  "Validates that the record's attribute is not included in the
  sequence of values."
  [exlusions]
  (not (includes? exlusions value))
  "is reserved.")

(defvalidation validate-format
  "Validates that the record's attribute matches the pattern."
  [pattern]
  (and value (re-matches pattern value))
  "is invalid.")

(defvalidation validate-inclusion
  "Validates that the record's attribute is not included in the
  sequence of values."
  [inlusions]
  (includes? inlusions value)
  "is not a valid option.")

(defvalidation validate-latitude
  "Validates that the record's attribute is between -90.0 and 90."
  []
  (latitude? value)
  "must be between -90.0 and 90.0.")

(defvalidation validate-longitude
  "Validates that the record's attribute is between -90.0 and 90."
  []
  (longitude? value)
  "must be between -180.0 and 180.0.")

(defn validate-location
  "Validates that the record's attribute is a valid location."
  [record attribute & options]
  (let [options (apply hash-map options)
        latitude (or (:latitude options) :latitude)
        longitude (or (:longitude options) :longitude)
        options (flatten (seq options))]
    (apply
     validate-longitude
     (apply validate-latitude record [attribute latitude] options)
     [attribute longitude] options)))

(defvalidation validate-max-length 
  "Validates that the record's attribute is not longer than maximum
  number of characters."
  [maximum]
  (<= (count value) maximum)
  (format "is too long (maximum is %d characters)." maximum))

(defvalidation validate-min-length 
  "Validates that the record's attribute is at least minimum number of
characters."
  [minimum]
  (>= (count value) minimum)
  (format "is too short (minimum is %d characters)." minimum))

(defvalidation validate-presence
  "Validates that the record's attribute is not blank."
  []
  (if (isa? (class value) String)
    (not (blank? value))
    (not (nil? value)))
  "can't be blank.")
