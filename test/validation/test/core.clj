(ns validation.test.core
  (:use clojure.test validation.core))

(deftest test-add-error-message-on
  (let [record (add-error-message-on {:email "bob"} :email "is not a valid email address.")]
    (is (= (error-messages-on record :email) ["is not a valid email address."]))))

(deftest test-error-messages
  (let [messages {:name ["can't be blank"]}
        record (with-meta {:name ""} {:errors messages})]
    (is (= (error-messages record) messages))))

(deftest test-error-messages-on
  (let [record (with-meta {:name ""} {:errors {:name ["can't be blank"]}})]
    (is (= (error-messages-on record :name) ["can't be blank"]))))

(deftest test-error-message-on
  (let [record (with-meta {:name ""} {:errors {:name ["can't be blank"]}})]
    (is (= (error-message-on record :name) "can't be blank"))))

(deftest test-valid-email?
  (are [address]
    (is (valid-email? address))
    "info@domain.com"
    "first.last@domain.com")
  (are [address]
    (is (not (valid-email? address)))
    nil
    ""
    "root"
    "@localhost"
    "root@localhost"
    "domain.com"
    "@domain.com"))

(deftest test-validate-presence-of  
  (let [result (validate-presence-of {:name "Bob"} :name)]
    (is (nil? (meta result))))
  (let [result (validate-presence-of {:name ""} :name)]
    (is (= (error-messages-on result :name) ["can't be blank."]))))

