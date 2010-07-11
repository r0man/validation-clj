(ns validation.errors)

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

(defn valid?
  "Returns tru if the record is valid, otherwise false."
  [record]  
  (let [errors (error-messages record)]
    (or (nil? errors) (empty? errors))))
