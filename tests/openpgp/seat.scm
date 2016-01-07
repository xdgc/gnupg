#!/usr/bin/env gpgscm

(load (in-srcdir "defs.scm"))

(define (check-seat args input)
  (lambda (source sink)
    (lettmp (encrypted)
	    (call-popen `(,GPG --output ,encrypted --yes -seat
			       --always-trust ;; XXX
			       ,@args ,source) input)
	    (call-popen `(,GPG --output ,sink --yes ,encrypted) ""))))

(for-each-p
 "Test encryption, signing, and producing armored output"
 (lambda (source)
   (check-identity source
		   (check-seat '(-r two@example.com --passphrase-fd "0")
			       usrpass1)))
 plain-files)
