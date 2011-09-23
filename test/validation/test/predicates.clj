(ns validation.test.predicates
  (:use clojure.test
        validation.predicates))

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
      nil "" -90.1 91 90.1 91)))

(deftest test-longitude?
  (testing "valid longitude coordinates"
    (are [number]
      (is (longitude? number))
      -180 -180.0 0 180 180.0))
  (testing "invalid longitude coordinates"
    (are [number]
      (is (not (longitude? number)))
      nil "" -180.1 181 180.1 181)))

(deftest test-location?
  (testing "valid locations"
    (are [location]
      (is (location? location))
      {:latitude 0 :longitude 0}
      {:latitude 0 :longitude -180}
      {:latitude 0 :longitude 180}
      {:latitude -90 :longitude 0}
      {:latitude 90 :longitude 0}))
  (testing "invalid locations"
    (are [location]
      (is (not (location? location)))
      {:latitude 0 :longitude -180.1}
      {:latitude 0 :longitude 180.1}
      {:latitude -90.1 :longitude 0}
      {:latitude 90.1 :longitude 0})))
