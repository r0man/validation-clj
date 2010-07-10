(ns validation.test.core
  (:use clojure.test validation.core validation.errors))

(def *user*
     {:name "Roman Scherer"
      :email "roman.scherer@burningswell.com"
      :age 30
      :gender "m"
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
    (are [value]
      (is empty? (error-messages-on (validator (assoc *user* :email value)) :email))
      (:email *user*))
    (are [value]
      (is (= (error-messages-on (validator (assoc *user* :email value)) :email) errors))
      nil "" "root")))

(deftest test-validate-email-with-allow-blank
  (let [validator (validate-email :email :allow-blank true) errors ["must be an email."]]
    (are [value]
      (is empty? (error-messages-on (validator (assoc *user* :email value)) :email))
      (:email *user*) nil "")
    (are [value]
      (is (= (error-messages-on (validator (assoc *user* :email value)) :email) errors))
      "root")))

(deftest test-validate-inclusion-of-with-vector
  (let [validator (validate-inclusion-of :gender ["m" "f"]) errors ["is not included in the list."]]
    (are [value]
      (is empty? (error-messages-on (validator (assoc *user* :gender value)) :gender))
      "m" "f")
    (are [value]
      (is (= (error-messages-on (validator (assoc *user* :gender value)) :gender) errors))
      nil "" "x")))

(deftest test-validate-inclusion-of-with-range
  (let [validator (validate-inclusion-of :age (range 0 120)) errors ["is not included in the list."]]
    (are [value]
      (is empty? (error-messages-on (validator (assoc *user* :gender value)) :gender))
      0 1 119 120)
    (are [value]
      (is (= (error-messages-on (validator (assoc *user* :age value)) :age) errors))
      nil "" -1 121)))

(deftest test-validate-inclusion-of-with-allow-blank
  (let [validator (validate-inclusion-of :gender ["m" "f"] :allow-blank true) errors ["is not included in the list."]]
    (are [value]
      (is empty? (error-messages-on (validator (assoc *user* :gender value)) :gender))
      nil "" "m" "f")
    (are [value]
      (is (= (error-messages-on (validator (assoc *user* :gender value)) :gender) errors))
      "x")))

(deftest test-validate-presence-of
  (let [validator (validate-presence-of :name) errors ["can't be blank."]]
    (is empty? (error-messages-on (validator *user*) :name))
    (are [value]
      (is (= (error-messages-on (validator (assoc *user* :name value)) :name) errors))
      nil "")))

