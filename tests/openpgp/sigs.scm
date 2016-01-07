#!/usr/bin/env gpgscm

(load (in-srcdir "defs.scm"))

(define (check-signature args input)
  (lambda (source sink)
    (lettmp (signed)
	    (call-popen `(,GPG --output ,signed --yes --sign
			       ,@args ,source) input)
	    (call-popen `(,GPG --output ,sink --yes ,signed) ""))))

(for-each-p
 "Test signing with the default hash algorithm"
 (lambda (source) (check-identity source (check-signature '() "")))
 (append plain-files data-files))

(for-each-p
 "Test signing with a specific hash algorithm"
 (lambda (hash)
   (if (have-pubkey-algo? "RSA")
       ;; RSA key, so any hash is okay.
       (check-identity (car plain-files)
		       (check-signature
			`(--user ,usrname3 --digest-algo ,hash) "")))
   (if (not (equal? "MD5" hash))
       ;; Using the DSA sig key - only 160 bit or larger hashes
       (check-identity (car plain-files)
		       (check-signature
			`(--passphrase-fd "0" --digest-algo ,hash) usrpass1))))
 all-hash-algos)

(for-each-p
 "Test signing using DSA with the default hash algorithm"
 (lambda (source)
   (check-identity source
		   (check-signature `(--user ,dsa-usrname1) "")))
 (append plain-files data-files))

(define algos (if (have-hash-algo? "RIPEMD160")
		  '("SHA1" "RIPEMD160")
		  '("SHA1")))
(for-each-p
 "Test signing using DSA with a specific hash algorithm"
 (lambda (hash)
   (check-identity (car plain-files)
		   (check-signature
		    `(--user ,dsa-usrname1 --digest-algo ,hash) "")))
 algos)
