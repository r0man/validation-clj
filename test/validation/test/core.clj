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
    (is empty? (error-messages-on (validator *user*) :email))
    (is (= (error-messages-on (validator (assoc *user* :email nil)) :email) errors))
    (is (= (error-messages-on (validator (assoc *user* :email "")) :email) errors))
    (is (= (error-messages-on (validator (assoc *user* :email "root")) :email) errors)))
  (let [validator (validate-email :email :allow-blank true) errors ["must be an email."]]
    (is empty? (error-messages-on (validator *user*) :email))
    (is (empty? (error-messages-on (validator (assoc *user* :email nil)) :email)))
    (is (empty? (error-messages-on (validator (assoc *user* :email "")) :email)))
    (is (= (error-messages-on (validator (assoc *user* :email "root")) :email) errors))))

(deftest test-validate-inclusion-of
  (let [validator (validate-inclusion-of :gender :in ["m" "f"]) errors ["is not included in the list."]]
    (is empty? (error-messages-on (validator *user*) :gender))
    (is (= (error-messages-on (validator (assoc *user* :gender nil)) :gender) errors))
    (is (= (error-messages-on (validator (assoc *user* :gender "")) :gender) errors))
    (is (= (error-messages-on (validator (assoc *user* :gender "x")) :gender) errors)))
  (let [validator (validate-inclusion-of :gender :in ["m" "f"] :allow-blank true) errors ["is not included in the list."]]
    (is empty? (error-messages-on (validator *user*) :gender))
    (is (empty? (error-messages-on (validator (assoc *user* :gender nil)) :gender)))
    (is (empty? (error-messages-on (validator (assoc *user* :gender "")) :gender)))
    (is (= (error-messages-on (validator (assoc *user* :gender "x")) :gender) errors)))
  (let [validator (validate-inclusion-of :age :in (range 0 120)) errors ["is not included in the list."]]
    (is empty? (error-messages-on (validator *user*) :age))
    (is (= (error-messages-on (validator (assoc *user* :age nil)) :age) errors))
    (is (= (error-messages-on (validator (assoc *user* :age "")) :age) errors))
    (is (= (error-messages-on (validator (assoc *user* :age -1)) :age) errors))
    (is (= (error-messages-on (validator (assoc *user* :age 121)) :age) errors))))

(deftest test-validate-presence-of
  (let [validator (validate-presence-of :name) errors ["can't be blank."]]
    (is empty? (error-messages-on (validator *user*) :name))
    (is (= (error-messages-on (validator (assoc *user* :name nil)) :name) errors))
    (is (= (error-messages-on (validator (assoc *user* :name "")) :name) errors))))

