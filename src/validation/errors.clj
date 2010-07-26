(ns validation.errors
  (:use [clojure.contrib.seq :only (includes?)]
        [clojure.contrib.string :only (capitalize join replace-re)]))

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
  "Add the error message for attribute to the record's list of error
  messages."
  [record attribute message]
  (if-not (includes? (error-messages-on record attribute) message)
    (with-meta record
      (assoc-in (meta record) [:errors attribute]
                (conj (or (error-messages-on record attribute) []) message)))
    record))

(defn- format-error-message [attribute error-message]
  (if (and attribute error-message)
    (str (capitalize (replace-re #"^:+" "" (str attribute))) " " error-message)))

(defn format-error-messages
  "Returns an array of formatted error messages."
  [errors]
  (if errors
    (flatten
     (map (fn [[attribute error-messages]]
            (map #(format-error-message attribute %) error-messages))
          errors))
    []))

(defn valid?
  "Returns tru if the record is valid, otherwise false."
  [record]  
  (let [errors (error-messages record)]
    (or (nil? errors) (empty? errors))))
