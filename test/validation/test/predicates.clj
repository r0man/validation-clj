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

(deftest test-latitude?
  (testing "valid latitude coordinates"
    (are [number]
      (is (latitude? number))
      -90 -90.0 0 90 90.0))
  (testing "invalid latitude coordinates"
    (are [number]
      (is (not (latitude? number)))
      -90.1 91 90.1 91)))

(deftest test-longitude?
  (testing "valid longitude coordinates"
    (are [number]
      (is (longitude? number))
      -180 -180.0 0 180 180.0))
  (testing "invalid longitude coordinates"
    (are [number]
      (is (not (longitude? number)))
      -180.1 181 180.1 181)))
