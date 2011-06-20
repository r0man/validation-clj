(ns validation.test.core
  (:use [clojure.contrib.error-kit :only (handle with-handler)]
        clojure.test validation.core validation.errors validation.test))

(refer-private 'validation.core)

(defn validate-user [user]
  (-> user
      (validate-presence :nick)
      (validate-min-length :nick 2)
      (validate-max-length :nick 16)
      (validate-presence :email)
      (validate-email :email)
      (validate-presence :password)
      (validate-confirmation :password)))

(def *valid-user*
     {:nick "bob"
      :email "bob@example.com"
      :password "secret"
      :password-confirmation "secret"})

(deftest test-extract-value
  (is (= "bob" (extract-value {:nick "bob"} :nick)))
  (is (= "bob" (extract-value {:user {:nick "bob"}} [:user :nick])))
  (is (= "bob" (extract-value {:nick "bob"} (fn [record] (:nick record))))))

(deftest test-validate-acceptance
  (testing "accepted attribute"
    (are [value]
      (is (valid? (validate-acceptance {:tos value} :tos)))
      "1"))
  (testing "not accepted attribute"
    (are [value]
      (let [record (validate-acceptance {:tos value} :tos)]
        (is (not (valid? record)))
        (is (= (error-messages-on record :tos) ["must be accepted."])))
      nil "" "0")))

(deftest test-validate-confirmation
  (testing "valid attribute"
    (is (valid? (validate-confirmation {:password "secret" :password-confirmation "secret"} :password))))
  (testing "invalid attribute"
    (are [value]
      (let [record (validate-confirmation {:password "secret" :password-confirmation value} :password)]
        (is (not (valid? record)))
        (is (= (error-messages-on record :password) ["doesn't match confirmation."])))
      nil "" "invalid")))

(deftest test-validate-email
  (testing "valid email address"
    (are [value]
      (is (valid? (validate-email {:email value} :email)))
      "info@domain.com" "first.last@sub.domain.com"))
  (testing "invalid email address"
    (are [value]
      (let [record (validate-email {:email value} :email)]
        (is (not (valid? record)))
        (is (= (error-messages-on record :email) ["is not a valid email address."])))
      nil "" "root" "root@localhost")))

(deftest test-validate-exact-length
  (testing "valid attribute"
    (are [value]
      (is (valid? (validate-exact-length {:country value} :country 2)))
      "de" "es"))
  (testing "invalid attribute"
    (are [value]
      (let [record (validate-exact-length {:country value} :country 2)]
        (is (not (valid? record)))
        (is (= (error-messages-on record :country) ["has the wrong length (should be 2 characters)."])))
      nil "" "deu" "esp")))

(deftest test-confirmation-keyword
  (is (= (confirmation-keyword "password") :password-confirmation))
  (is (= (confirmation-keyword :password) :password-confirmation)))

(deftest test-validate-exclusion
  (testing "valid attribute"
    (are [value]
      (is (valid? (validate-exclusion {:nick value} :nick ["admin" "root"])))
      nil "" "test"))
  (testing "invalid attribute"
    (are [value]
      (let [record (validate-exclusion {:nick value} :nick ["admin" "root"])]
        (is (not (valid? record)))
        (is (= (error-messages-on record :nick) ["is reserved."])))
      "admin" "root")))

(deftest test-validate-format
  (testing "valid attribute"
    (are [value]
      (is (valid? (validate-format {:nick value} :nick #"(?i)[a-z0-9]+")))
      "nick" "n1ck" ))
  (testing "invalid attribute"
    (are [value]
      (let [record (validate-format {:nick value} :nick #"(?i)[a-z0-9]+")]
        (is (not (valid? record)))
        (is (= (error-messages-on record :nick) ["is invalid."])))
      nil "" "!" "\"" "§" "$" "%" "&" "/" "(" ")" "=" "?" "`" "´")))

(deftest test-validate-inclusion
  (testing "valid attribute"
    (are [value]
      (is (valid? (validate-inclusion {:gender value} :gender ["m" "f"])))
      "m" "f"))
  (testing "invalid attribute"
    (are [value]
      (let [record (validate-inclusion {:gender value} :gender ["m" "f"])]
        (is (not (valid? record)))
        (is (= (error-messages-on record :gender) ["is not a valid option."])))
      nil "" "x")))

(deftest test-validate-max-length
  (testing "valid attribute"
    (are [value]
      (is (valid? (validate-max-length {:nick value} :nick 5)))
      nil "" "1" "12" "123" "1234" "1234"))
  (testing "invalid attribute"
    (are [value]
      (let [record (validate-max-length {:nick value} :nick 5)]
        (is (not (valid? record)))
        (is (= (error-messages-on record :nick) ["is too long (maximum is 5 characters)."])))
      "123456")))

(deftest test-validate-min-length
  (testing "valid attribute"
    (are [value]
      (is (valid? (validate-min-length {:nick value} :nick 2)))
      "12" "123" "1234" "1234"))
  (testing "invalid attribute"
    (are [value]
      (let [record (validate-min-length {:nick value} :nick 2)]
        (is (not (valid? record)))
        (is (= (error-messages-on record :nick) ["is too short (minimum is 2 characters)."])))
      nil "" "1")))

(deftest test-validate-presence
  (testing "valid attribute"
    (are [value]
      (is (valid? (validate-presence {:name value} :name)))
      "x" "root" {:first "First" :last "Last"}))
  (testing "invalid attribute"
    (are [value]
      (let [record (validate-presence {:nick value} :nick)]
        (is (not (valid? record)))
        (is (= (error-messages-on record :nick) ["can't be blank."])))
      nil "")))

(deftest test-validate
  (is (= (validate *valid-user* validate-user) *valid-user*))
  (let [invalid-user (assoc *valid-user* :nick "" :email "bob")]
    (is (thrown? IllegalArgumentException (validate invalid-user validate-user)))
    (try
      (validate invalid-user validate-user)
      (catch IllegalArgumentException e
        (is (= (.getMessage e)
               (exception-message (validate-user invalid-user))))))
    (with-handler
      (validate invalid-user validate-user)
      (handle *validation-error* [record]
              (is (not (valid? record)))
              (is (= (error-messages record)
                     (error-messages (validate-user invalid-user))))))))

(deftest test-validate-latitude
  (testing "valid latitude"
    (are [latitude]
      (is (valid? (validate-latitude {:latitude latitude} :latitude)))
      -90.0 90 0 90 90.0))
  (testing "invalid latitude"
    (are [latitude]
      (let [record (validate-latitude {:latitude latitude} :latitude)]
        (is (not (valid? record)))
        (is (= (error-messages-on record :latitude)
               ["must be between -90.0 and 90.0."])))
      nil "" -90.1 91 90.1 91)))

(deftest test-validate-longitude
  (testing "valid longitude"
    (are [longitude]
      (is (valid? (validate-longitude {:longitude longitude} :longitude)))
      -180.0 180 0 180 180.0))
  (testing "invalid longitude"
    (are [longitude]
      (let [record (validate-longitude {:longitude longitude} :longitude)]
        (is (not (valid? record)))
        (is (= (error-messages-on record :longitude)
               ["must be between -180.0 and 180.0."])))
      nil "" -180.1 181 180.1 181)))

(deftest test-validate-location
  (testing "valid locations"
    (are [location]
      (is (valid? (validate-location {:location location} :location)))
      {:latitude 0 :longitude 0}
      {:latitude 0 :longitude -180}
      {:latitude 0 :longitude 180}
      {:latitude -90 :longitude 0}
      {:latitude 90 :longitude 0}))
  (testing "invalid locations"
    (are [location messages]
      (let [record (validate-location {:location location} :location)]
        (is (not (valid? record)))
        (is (= (error-messages-on record :location) messages)))
      {:latitude 0 :location 180.1}
      {:longitude ["must be between -180.0 and 180.0."]}
      {:latitude 90.1 :longitude 0}
      {:latitude ["must be between -90.0 and 90.0."]})))
