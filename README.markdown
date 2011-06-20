# validation-clj

A simple validation library for Clojore.

## Usage

Import the library.

    (use 'validation.core 'validation.errors)

Define a validator like this. This generates the validate-user and the
validate-user! functions.

    (defvalidator validate-user
      (validate-presence :nick)
      (validate-min-length :nick 2)
      (validate-max-length :nick 16)
      (validate-presence :email)
      (validate-email :email)
      (validate-presence :password)
      (validate-confirmation :password))

Define some examples to validate.

    (def *alice* {:nick "alice" :email "alice"})

    (def *bob*
      {:nick "bob"
       :email "bob@example.com"
       :password "secret"
       :password-confirmation "secret"})

The validate-user fn returns the record itself with errors attached to
the metadata.

    (validate-user *alice*)
    ;=> {:nick "alice"}

    (validate-user *bob*)
    ;=> {:nick "bob"
    ;    :email "bob@example.com"
    ;    :password "secret"
    ;    :password-confirmation "secret"}

The valid? fn checks if the record returned from a validation fn is
valid or not.

    (valid? (validate-user *alice*))
    ;=> false

    (valid? (validate-user *bob*))
    ;=> true

The error-messages returns the error messages from an invalid record.

    (error-messages (validate-user *bob*))
    ;=> nil

    (error-messages (validate-user *alice*))
    ;=> {:email ["is not a valid email address." "can't be blank."]
    ;    :password ["can't be blank."]}

The validate-user! fn behaves like validate-user, but throws an
error-kit error if the record is not valid.

    (use 'clojure.contrib.error-kit)

    (with-handler (validate-user! *alice*)
      (handle *validation-error* [record]
              (meta record)))
    ;=> {:email ["is not a valid email address." "can't be blank."]
    ;    :password ["can't be blank."]}

;; For anything else look at the tests ...

## Installation

Via Clojars: http://clojars.org/validation-clj

## License

Copyright (C) 2010 FIXME

Distributed under the Eclipse Public License, the same as Clojure.
