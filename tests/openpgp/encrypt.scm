#!/usr/bin/env gpgscm

(load (in-srcdir "defs.scm"))

(define (check-encrypt-decrypt args input)
  (lambda (source sink)
    (lettmp (encrypted)
	    (call-popen `(,GPG --output ,encrypted --yes --encrypt
			       --always-trust ;; XXX
			       ,@args ,source) input)
	    (call-popen `(,GPG --output ,sink --yes ,encrypted) ""))))

(for-each-p
 "Test encryption"
 (lambda (source)
   (check-identity source (check-encrypt-decrypt
			   `(--recipient ,usrname2) "")))
 (append plain-files data-files))

(for-each-p
 "Test encryption using a specific cipher algorithm"
 (lambda (cipher)
   (for-each-p
    ""
    (lambda (source)
      (check-identity source
		      (check-encrypt-decrypt
		       `(--recipient ,usrname2 --cipher-algo ,cipher) "")))
    (append plain-files data-files)))
 all-cipher-algos)

(for-each-p
 "Test encryption using DSA"
 (lambda (source)
   (check-identity source (check-encrypt-decrypt
			   `(--recipient ,dsa-usrname2) "")))
 (append plain-files data-files))

(for-each-p
 "Test encryption using DSA and a specific cipher algorithm"
 (lambda (cipher)
   (for-each-p
    ""
    (lambda (source)
      (check-identity source
		      (check-encrypt-decrypt
		       `(--recipient ,dsa-usrname2 --cipher-algo ,cipher) "")))
    (append plain-files data-files)))
 all-cipher-algos)
