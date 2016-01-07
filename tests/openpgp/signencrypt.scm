#!/usr/bin/env gpgscm

(load (in-srcdir "defs.scm"))

(for-each-p
 "Checking signing and encryption"
 (lambda (source)
   (tr:do
    (tr:open source)
    (tr:gpg usrpass1 `(--yes --passphrase-fd "0" -se --recipient ,usrname2))
    (tr:gpg "" '(--yes))
    (tr:assert-identity source)))
 (append plain-files data-files))


(info "Checking bug 537: MDC problem with old style compressed packets.")
(lettmp (tmp)
  (call-popen `(,@GPG' --yes --passphrase-fd "0"
		       --output ,tmp ,(in-srcdir "bug537-test.data.asc"))
	      usrpass1)
  (if (not (string=? "4336AE2A528FAE091E73E59E325B588FEE795F9B"
		     (cadar (gpg-hash-string `(--print-md SHA1 ,tmp) ""))))
      (error "bug537-test.data.asc: mismatch (bug 537)")))

(for-each-p
 "Checking signing and encryption using DSA"
 (lambda (source)
   (tr:do
    (tr:open source)
    (tr:gpg usrpass1 `(--yes --passphrase-fd "0" -se
			     -u ,dsa-usrname1
			     --recipient ,dsa-usrname2))
    (tr:gpg "" '(--yes))
    (tr:assert-identity source)))
 (append plain-files data-files))

(define algos (if (have-hash-algo? "RIPEMD160")
		  '("SHA1" "RIPEMD160")
		  '("SHA1")))
(for-each-p
 "Checking signing and encryption using DSA with a specific hash algorithm"
 (lambda (hash)
   (tr:do
    (tr:open (car plain-files))
    (tr:gpg usrpass1 `(--yes --passphrase-fd "0" -se
			     -u ,dsa-usrname1
			     --recipient ,dsa-usrname2
			     --digest-algo ,hash))
    (tr:gpg "" '(--yes))
    (tr:assert-identity (car plain-files))))
 algos)
