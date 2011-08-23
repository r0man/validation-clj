(ns validation.core
  (:refer-clojure :exclude (replace))
  (:use [clojure.string :only (blank? join replace)]
        [clojure.contrib.error-kit :only (deferror throw-msg raise)]
        validation.errors
        validation.predicates))

(deferror validation-error [] [record]
  {:msg (exception-message record)
   :unhandled (throw-msg IllegalArgumentException)})

(defn validate
  "Validates the record by applying the validation-fn. The function
  raises a validation-error if the record is invalid. If the record
  is valid the function returns the record."
  [record validation-fn]
  (let [record (validation-fn record)]
    (if-not (valid? record)
      (raise validation-error record)
      record)))

(defn- confirmation-keyword
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
  `(defn ~fn-name ~fn-doc [~'attribute ~@args]
     (fn [~'record]
       (let [~'value (extract-value ~'record ~'attribute)]
         (if ~predicate-fn
           ~'record
           (add-error-message-on ~'record ~'attribute ~error-fn))))))

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
  (= (count value) length)
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
  (and value (re-matches pattern value))
  "is invalid.")

(defvalidator inclusion-of
  "Validates that the record's attribute is not included in the
  sequence of values."
  [inlusions]
  (contains? (set inlusions) value)
  "is not a valid option.")

(defvalidator is-latitude
  "Validates that the record's attribute is between -90.0 and 90."
  []
  (latitude? value)
  "must be between -90.0 and 90.0.")

(defvalidator is-longitude
  "Validates that the record's attribute is between -90.0 and 90."
  []
  (longitude? value)
  "must be between -180.0 and 180.0.")

(defn is-location
  "Validates that the record's attribute is a valid location."
  [attribute & {:keys [latitude longitude]}]
  (let [latitude (or latitude :latitude) longitude (or longitude :longitude)]
    (fn [record]
      (let [record (or record {})]
        (if-let [errors (-> (or (extract-value record attribute) {})
                            ((is-latitude latitude))
                            ((is-longitude longitude))
                            meta :errors)]
          (with-meta record
            (assoc-in (meta record) [:errors attribute] errors))
          record)))))

(defvalidator max-length-of
  "Validates that the record's attribute is not longer than maximum
  number of characters."
  [maximum]
  (<= (count value) maximum)
  (format "is too long (maximum is %d characters)." maximum))

(defvalidator min-length-of
  "Validates that the record's attribute is at least minimum number of
characters."
  [minimum]
  (>= (count value) minimum)
  (format "is too short (minimum is %d characters)." minimum))

(defvalidator presence-of
  "Validates that the record's attribute is not blank."
  []
  (if (isa? (class value) String)
    (not (blank? value))
    (not (nil? value)))
  "can't be blank.")
