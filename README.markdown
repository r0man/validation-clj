# validation-clj

A simple validation library for Clojure.

## Usage

Need some records validated?

    (def *alice* {:nick "alice" :email "alice"})

    (def *bob*
      {:nick "bob"
       :email "bob@example.com"
       :password "secret"
       :password-confirmation "secret"})

Use the validation library.

    (use 'validation.core 'validation.errors)

Define a validation which generates the valid-user?, validate-user and
validate-user! functions.

    (defvalidation user
      (validate-presence-of :nick)
      (validate-min-length-of :nick 2)
      (validate-max-length-of :nick 16)
      (validate-presence-of :email)
      (validate-email :email)
      (validate-presence-of :password)
      (validate-confirmation-of :password))

The valid-user? fn checks if the record is valid or not.

    (valid-user? *alice*)
    ;=> false

    (valid-user? *bob*)
    ;=> true


The validate-user fn returns the record itself with possible error
messages attached to the metadata.

    (validate-user *alice*)
    ;=> {:nick "alice"}

    (validate-user *bob*)
    ;=> {:nick "bob"
    ;    :email "bob@example.com"
    ;    :password "secret"
    ;    :password-confirmation "secret"}

The error-messages reads the error messages from the meta data of the
validated record.

    (error-messages (validate-user *bob*))
    ;=> nil

    (error-messages (validate-user *alice*))
    ;=> {:email ["is not a valid email address." "can't be blank."]
    ;    :password ["can't be blank."]}

The validate-user! fn is similar to validate-user, but uses the
error-kit condition system to signal validation errors.

    (use 'clojure.contrib.error-kit)

    (with-handler (validate-user! *alice*)
      (handle *validation-error* [record]
              (meta record)))
    ;=> {:email ["is not a valid email address." "can't be blank."]
    ;    :password ["can't be blank."]}

    (validate-user! *bob*)
    ;=> {:nick "bob"
    ;    :email "bob@example.com"
    ;    :password "secret"
    ;    :password-confirmation "secret"}

For anything else look at the tests ...

## Installation

Via Clojars: http://clojars.org/validation-clj

## License

Copyright (C) 2010 FIXME

Distributed under the Eclipse Public License, the same as Clojure.
