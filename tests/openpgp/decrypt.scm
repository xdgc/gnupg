#!/usr/bin/env gpgscm

(load (in-srcdir "defs.scm"))

(define (decrypt source sink)
  (call-popen `(,GPG --output ,sink --yes ,source) ""))

(define (check-file plain-name cipher-name)
  (lettmp (decrypted-name)
	  (decrypt cipher-name decrypted-name)
	  (if (not (file=? plain-name decrypted-name))
	      (error "mismatch"))))

(for-each-p "Checking decryption of supplied files"
	    (lambda (f) (check-file
			 f (in-srcdir (string-append f ".asc"))))
	    plain-files)
(for-each-p "Checking decryption of supplied DSA encrypted file"
	    (lambda (f) (check-file
			 f (in-srcdir (string-append f "-pgp.asc"))))
	    (list (car plain-files)))
