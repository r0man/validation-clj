(ns validation.test.predicates
  (:use clojure.test validation.predicates validation.test))

(refer-private 'validation.predicates)

(deftest test-email?
  (testing "valid email address"
    (are [address]
      (is (email? address))
      "info@domain.com" "first.last@domain.com"))
  (testing "invalid email address"
    (are [address]
      (is (not (email? address)))
      nil "" "root" "@localhost" "root@localhost" "domain.com" "@domain.com")))

