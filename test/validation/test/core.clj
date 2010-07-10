(ns validation.test.core
  (:use clojure.test validation.core))

(def *user*
     {:name "Roman Scherer"
      :email "roman.scherer@burningswell.com"
      :password "secret"
      :password-confirmation "secret"})

(deftest test-add-error-message-on
  (let [message "is not a valid email address."]
    (let [user (add-error-message-on *user* :email message)]
      (is (= (error-messages-on user :email) [message])))
    (is (= (-> *user*
               (add-error-message-on :email message)
               (add-error-message-on :email message))
           (add-error-message-on *user* :email message)))))

(deftest test-error-messages
  (is (= (error-messages *user*) nil))
  (let [messages {:name ["can't be blank"]}
        record (with-meta {:name ""} {:errors messages})]
    (is (= (error-messages record) messages))))

(deftest test-error-messages-on
  (is (= (error-messages-on *user* :email) nil))
  (let [record (with-meta {:name ""} {:errors {:name ["can't be blank"]}})]
    (is (= (error-messages-on record :name) ["can't be blank"]))))

(deftest test-error-message-on
  (is (= (error-message-on *user* :email) nil))
  (let [record (with-meta {:name ""} {:errors {:name ["can't be blank"]}})]
    (is (= (error-message-on record :name) "can't be blank"))))

(deftest test-email?
  (are [address]
    (is (email? address))
    "info@domain.com"
    "first.last@domain.com")
  (are [address]
    (is (not (email? address)))
    nil
    ""
    "root"
    "@localhost"
    "root@localhost"
    "domain.com"
    "@domain.com"))

(deftest test-validate-email
  (let [validator (validate-email :email) errors ["must be an email."]]
    (is empty? (error-messages-on (validator *user*) :email))
    (is (= (error-messages-on (validator (assoc *user* :email nil)) :email) errors))
    (is (= (error-messages-on (validator (assoc *user* :email "")) :email) errors))
    (is (= (error-messages-on (validator (assoc *user* :email "root")) :email) errors))))

(deftest test-validate-presence-of
  (let [validator (validate-presence-of :name) errors ["can't be blank."]]
    (is empty? (error-messages-on (validator *user*) :name))
    (is (= (error-messages-on (validator (assoc *user* :name nil)) :name) errors))
    (is (= (error-messages-on (validator (assoc *user* :name "")) :name) errors))))

