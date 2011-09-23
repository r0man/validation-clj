(ns validation.errors
  (:refer-clojure :exclude (replace))
  (:use [clojure.string :only (capitalize join replace)]))

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
