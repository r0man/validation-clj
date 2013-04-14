(ns validation.test.core
  (:use [slingshot.slingshot :only [try+]]
        [sqlingvo.core :only [run1 sql]]
        clojure.test
        validation.core))

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

(def bob
  {:name "Bob"
   :email "bob@example.com"
   :password "secret"
   :password-confirmation "secret"})

(def europe
  {:name "Europe"
   :iso-3166-alpha-2 "eu"
   :location {:latitude 0 :longitude 0}})

(deftest test-add-error-message-on
  (let [message "is not a valid email address."]
    (let [user (add-error-message-on bob :email message)]
      (is (= (error-messages-on user :email) [message])))
    (is (= (-> bob
               (add-error-message-on :email message)
               (add-error-message-on :email message))
           (add-error-message-on bob :email message)))
    (is (= (error-messages
            (-> bob
                (add-error-message-on :email "can't be blank.")
                (add-error-message-on :email "must be a valid email address.")))
           {:email ["can't be blank." "must be a valid email address."]}))))

(deftest test-add-error-message-on-nested
  (let [continent (add-error-message-on europe [:location :latitude] "must be between -90.0 and 90.0.")]
    (is (= (error-message-on continent [:location :latitude])
           "must be between -90.0 and 90.0."))
    (is (= (error-messages-on continent [:location :latitude])
           ["must be between -90.0 and 90.0."]))))

(deftest test-error-messages
  (is (= (error-messages bob) nil))
  (let [messages {:name ["can't be blank"]}
        record (with-meta {:name ""} {:errors messages})]
    (is (= (error-messages record) messages))))

(deftest test-error-messages-on
  (is (= (error-messages-on bob :email) nil))
  (let [record (with-meta {:name ""} {:errors {:name ["can't be blank"]}})]
    (is (= (error-messages-on record :name) ["can't be blank"]))))

(deftest test-error-messages-on-nested
  (let [continent (with-meta europe {:errors {:location {:latitude ["must be between -90.0 and 90.0."]}}})]
    (is (= (error-messages-on continent [:location :latitude])
           ["must be between -90.0 and 90.0."]))))

(deftest test-error-message-on
  (is (= (error-message-on bob :email) nil))
  (let [record (with-meta {:name ""} {:errors {:name ["can't be blank"]}})]
    (is (= (error-message-on record :name) "can't be blank"))))

(deftest test-error-message-on-nested
  (let [continent (with-meta europe {:errors {:location {:latitude ["must be between -90.0 and 90.0."]}}})]
    (is (= (error-message-on continent [:location :latitude])
           "must be between -90.0 and 90.0."))))

(deftest test-format-error-message
  (is (nil? (format-error-message nil nil)))
  (is (= (format-error-message :email "is not a valid email address.")
         "Email is not a valid email address.")))

(deftest test-format-error-messages
  (is (empty? (format-error-messages nil)))
  (is (empty? (format-error-messages {})))
  (is (= (format-error-messages (sorted-map :email ["is not a valid email address."] :nick ["can't be blank."]))
         ["Email is not a valid email address." "Nick can't be blank."])))

(deftest test-valid?
  (is (valid? {}))
  (is (valid? (with-meta {} {:errors {}})))
  (is (not (valid? (with-meta {} {:errors {:name ["can't be blank."]}})))))

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
    (is (thrown? clojure.lang.ExceptionInfo (validate invalid-user validate-user)))
    (try+
     (validate invalid-user validate-user)
     (catch [:type :validation.core/error] {:keys [errors record]}
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
    (is (thrown? clojure.lang.ExceptionInfo (validate-user! invalid-user)))
    (try+
     (validate-user! invalid-user)
     (catch [:type :validation.core/error] {:keys [errors record]}
       (is (not (valid? record)))
       (is (= errors (:errors (meta record))))
       (is (= (error-messages record)
              (error-messages (validate-user invalid-user))))))))

(deftest test-valid-user?
  (is (valid-user? saved-user))
  (is (not (valid-user? {}))))

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

(deftest test-uniqueness-of
  (with-redefs [run1 (fn [db stmt]
                       (is (= ["SELECT nick FROM users WHERE (nick = ?) LIMIT 1" "Bob"]
                              (sql stmt)))
                       [])]
    (let [errors (:errors (meta ((uniqueness-of nil :users :nick) {:nick "Bob"})))]
      (is (= "has already been taken" (:nick errors))))))
