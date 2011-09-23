(ns validation.predicates)

(def ^:dynamic *email-regex*
  #"(?i)[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?")

(defn email?
  "Returns true if the email address is valid, otherwise false."
  [address] (and address (re-matches *email-regex* address)))

(defn latitude?
  "Returns true if number is betweeen -90.0 and 90.0, otherwise
  false."
  [number]
  (and (number? number)
       (>= number -90.0)
       (<= number 90.0)))

(defn longitude?
  "Returns true if number is between -180.0 and 180.0, otherwise
  false."
  [number]
  (and (number? number)
       (>= number -180.0)
       (<= number 180.0)))

(defn location?
  "Returns true if location has valid latitude and longitude
  coordinates, otherwise false."
  [location & {:keys [latitude longitude]}]
  (and (latitude? ((or latitude :latitude) location))
       (longitude? ((or longitude :longitude) location))))

