(ns validation.test.errors
  (:use clojure.test validation.errors))

(def *user*
     {:name "Bob"
      :email "bob@example.com"
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

(deftest test-valid?
  (is (valid? {}))
  (is (valid? (with-meta {} {:errors {}})))
  (is (not (valid? (with-meta {} {:errors {:name ["can't be blank."]}})))))
