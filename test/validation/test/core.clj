(ns validation.test.core
  (:use [slingshot.slingshot :only [try+]]
        clojure.test
        validation.core
        validation.errors))

(defn new-user? [user]
  (nil? (:id user)))

(defvalidate user
  (presence-of :nick)
  (min-length-of :nick 2)
  (max-length-of :nick 16)
  (presence-of :email)
  (is-email :email)
  (presence-of :crypted-password :unless new-user?)
  (presence-of :password :if new-user?)
  (confirmation-of :password :if new-user?))

(def saved-user
  {:id 1
   :nick "bob"
   :email "bob@example.com"
   :crypted-password "xxxx"})

(deftest test-extract-value
  (is (= "bob" (extract-value {:nick "bob"} :nick)))
  (is (= "bob" (extract-value {:user {:nick "bob"}} [:user :nick])))
  (is (= "bob" (extract-value {:nick "bob"} (fn [record] (:nick record))))))

(deftest test-acceptance-of
  (testing "accepted attribute"
    (are [value]
      (is (valid? ((acceptance-of :tos) {:tos value} )))
      "1"))
  (testing "not accepted attribute"
    (are [value]
      (let [record ((acceptance-of :tos) {:tos value})]
        (is (not (valid? record)))
        (is (= (error-messages-on record :tos) ["must be accepted."])))
      nil "" "0")))

(deftest test-confirmation-of
  (testing "valid attribute"
    (is (valid? ((confirmation-of :password) {:password "secret" :password-confirmation "secret"}))))
  (testing "invalid attribute"
    (are [value]
      (let [record ((confirmation-of :password) {:password "secret" :password-confirmation value})]
        (is (not (valid? record)))
        (is (= (error-messages-on record :password) ["doesn't match confirmation."])))
      nil "" "invalid")))

(deftest test-is-email
  (testing "valid email address"
    (are [value]
      (is (valid? ((is-email :email) {:email value})))
      "info@domain.com" "first.last@sub.domain.com"))
  (testing "invalid email address"
    (are [value]
      (let [record ((is-email :email) {:email value})]
        (is (not (valid? record)))
        (is (= (error-messages-on record :email) ["is not a valid email address."])))
      nil "" "root" "root@localhost")))

(deftest test-exact-length-of
  (testing "valid attribute"
    (are [value]
      (is (valid? ((exact-length-of :country 2) {:country value})))
      "de" "es"))
  (testing "invalid attribute"
    (are [value]
      (let [record ((exact-length-of :country 2) {:country value})]
        (is (not (valid? record)))
        (is (= (error-messages-on record :country) ["has the wrong length (should be 2 characters)."])))
      nil "" "deu" "esp")))

(deftest test-confirmation-keyword
  (is (= (confirmation-keyword "password") :password-confirmation))
  (is (= (confirmation-keyword :password) :password-confirmation)))

(deftest test-exclusion-of
  (testing "valid attribute"
    (are [value]
      (is (valid? ((exclusion-of :nick ["admin" "root"]) {:nick value})))
      nil "" "test"))
  (testing "invalid attribute"
    (are [value]
      (let [record ((exclusion-of :nick ["admin" "root"]) {:nick value})]
        (is (not (valid? record)))
        (is (= (error-messages-on record :nick) ["is reserved."])))
      "admin" "root")))

(deftest test-format-of
  (testing "valid attribute"
    (are [value]
      (is (valid? ((format-of :nick #"(?i)[a-z0-9]+") {:nick value})))
      "nick" "n1ck" ))
  (testing "invalid attribute"
    (are [value]
      (let [record ((format-of :nick #"(?i)[a-z0-9]+") {:nick value} )]
        (is (not (valid? record)))
        (is (= (error-messages-on record :nick) ["is invalid."])))
      nil "" "!" "\"" "§" "$" "%" "&" "/" "(" ")" "=" "?" "`" "´")))

(deftest test-inclusion-of
  (testing "valid attribute"
    (are [value]
      (is (valid? ((inclusion-of :gender ["m" "f"]) {:gender value} )))
      "m" "f"))
  (testing "invalid attribute"
    (are [value]
      (let [record ((inclusion-of :gender ["m" "f"]) {:gender value})]
        (is (not (valid? record)))
        (is (= (error-messages-on record :gender) ["is not a valid option."])))
      nil "" "x")))

(deftest test-max-length-of
  (testing "valid attribute"
    (are [value]
      (is (valid? ((max-length-of :nick 5) {:nick value})))
      nil "" "1" "12" "123" "1234" "1234"))
  (testing "invalid attribute"
    (are [value]
      (let [record ((max-length-of :nick 5) {:nick value})]
        (is (not (valid? record)))
        (is (= (error-messages-on record :nick) ["is too long (maximum is 5 characters)."])))
      "123456")))

(deftest test-min-length-of
  (testing "valid attribute"
    (are [value]
      (is (valid? ((min-length-of :nick 2) {:nick value})))
      "12" "123" "1234" "1234"))
  (testing "invalid attribute"
    (are [value]
      (let [record ((min-length-of :nick 2) {:nick value})]
        (is (not (valid? record)))
        (is (= (error-messages-on record :nick) ["is too short (minimum is 2 characters)."])))
      nil "" "1")))

(deftest test-presence-of
  (testing "valid attribute"
    (are [value]
      (is (valid? ((presence-of :name) {:name value})))
      "x" "root" {:first "First" :last "Last"}))
  (testing "invalid attribute"
    (are [value]
      (let [record ((presence-of :nick) {:nick value})]
        (is (not (valid? record)))
        (is (= (error-messages-on record :nick) ["can't be blank."])))
      nil "")))

(deftest test-is-latitude
  (testing "valid latitude"
    (are [latitude]
      (is (valid? ((is-latitude :latitude) {:latitude latitude})))
      -90.0 90 0 90 90.0))
  (testing "invalid latitude"
    (are [latitude]
      (let [record ((is-latitude :latitude) {:latitude latitude})]
        (is (not (valid? record)))
        (is (= (error-messages-on record :latitude)
               ["must be between -90.0 and 90.0."])))
      nil "" -90.1 91 90.1 91)))

(deftest test-is-longitude
  (testing "valid longitude"
    (are [longitude]
      (is (valid? ((is-longitude :longitude) {:longitude longitude})))
      -180.0 180 0 180 180.0))
  (testing "invalid longitude"
    (are [longitude]
      (let [record ((is-longitude :longitude) {:longitude longitude})]
        (is (not (valid? record)))
        (is (= (error-messages-on record :longitude)
               ["must be between -180.0 and 180.0."])))
      nil "" -180.1 181 180.1 181)))

(deftest test-is-location
  (testing "valid locations"
    (are [location]
      (is (valid? ((is-location :location) {:location location})))
      {:latitude 0 :longitude 0}
      {:latitude 0 :longitude -180}
      {:latitude 0 :longitude 180}
      {:latitude -90 :longitude 0}
      {:latitude 90 :longitude 0}))
  (testing "invalid locations"
    (are [location messages]
      (let [record ((is-location :location) {:location location})]
        (is (not (valid? record)))
        (is (= (error-messages-on record :location) messages)))
      {}
      {:latitude ["must be between -90.0 and 90.0."]
       :longitude ["must be between -180.0 and 180.0."]}
      {:latitude 0 :longitude 180.1}
      {:longitude ["must be between -180.0 and 180.0."]}
      {:latitude 90.1 :longitude 0}
      {:latitude ["must be between -90.0 and 90.0."]})
    (is (= (error-messages ((is-location :location) nil))
           (error-messages ((is-location :location) {}))))))

(deftest test-validate-user
  (is (= (validate saved-user validate-user) saved-user))
  (let [invalid-user (assoc saved-user :nick "" :email "bob")]
    (is (thrown? slingshot.Stone (validate invalid-user validate-user)))
    (try
      (validate invalid-user validate-user)
      (catch slingshot.Stone e
        (is (re-matches #"Object thrown by throw\+.*" (.getMessage e)))))
    (try+
     (validate invalid-user validate-user)
     (catch validation.error {record :record errors :errors}
       (is (= errors (:errors (meta record))))
       (is (not (valid? record)))
       (is (= (error-messages record)
              (error-messages (validate-user invalid-user))))))))

(deftest test-if-option
  (let [user
        (-> saved-user
            (dissoc :password :password-confirmation)
            (assoc :id 1 :crypted-password "xxxxx")
            (validate-user))]
    (is (valid? user))))

(deftest test-unless-option
  (let [user
        (-> saved-user
            (dissoc :crypted-password :password :password-confirmation)
            (assoc :id 1)
            (validate-user))]
    (is (not (valid? user)))
    (is (= ["can't be blank."] (error-messages-on user :crypted-password)))))

(deftest test-validate-user!
  (is (= (validate-user! saved-user) saved-user))
  (let [invalid-user (assoc saved-user :nick "" :email "bob")]
    (is (thrown? slingshot.Stone (validate-user! invalid-user)))
    (try
      (validate-user! invalid-user)
      (catch slingshot.Stone e
        (is (re-matches #"Object thrown by throw\+.*" (.getMessage e)))))
    (try+
     (validate-user! invalid-user)
     (catch validation.error {record :record errors :errors}
       (is (not (valid? record)))
       (is (= errors (:errors (meta record))))
       (is (= (error-messages record)
              (error-messages (validate-user invalid-user))))))))

(deftest test-valid-user?
  (is (valid-user? saved-user))
  (is (not (valid-user? {}))))
