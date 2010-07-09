(ns validation.test.core
  (:use clojure.test validation.core))

(deftest test-valid-email?
  (are [address]
    (is (valid-email? address))
    "info@domain.com"
    "first.last@domain.com")
  (are [address]
    (is (not (valid-email? address)))
    nil
    ""
    "root@localhost"))
