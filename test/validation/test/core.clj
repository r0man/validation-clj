(ns validation.test.core
  (:use clojure.test validation.core validation.errors))

(def *user*
     {:name "Roman Scherer"
      :email "roman.scherer@burningswell.com"
      :password "secret"
      :password-confirmation "secret"})

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

