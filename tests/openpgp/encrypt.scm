#!/usr/bin/env gpgscm

(load (in-srcdir "defs.scm"))

(for-each-p
 "Test encryption"
 (lambda (source)
   (tr:do
    (tr:open source)
    (tr:gpg "" `(--yes --encrypt --recipient ,usrname2))
    (tr:gpg "" '(--yes))
    (tr:assert-identity source)))
 (append plain-files data-files))

(for-each-p
 "Test encryption using a specific cipher algorithm"
 (lambda (cipher)
   (for-each-p
    ""
    (lambda (source)
      (tr:do
       (tr:open source)
       (tr:gpg "" `(--yes --encrypt --recipient ,usrname2
			  --cipher-algo ,cipher))
       (tr:gpg "" '(--yes))
       (tr:assert-identity source)))
    (append plain-files data-files)))
 all-cipher-algos)

(for-each-p
 "Test encryption using DSA"
 (lambda (source)
   (tr:do
    (tr:open source)
    (tr:gpg "" `(--yes --encrypt --recipient ,dsa-usrname2))
    (tr:gpg "" '(--yes))
    (tr:assert-identity source)))
 (append plain-files data-files))

(for-each-p
 "Test encryption using DSA and a specific cipher algorithm"
 (lambda (cipher)
   (for-each-p
    ""
    (lambda (source)
      (tr:do
       (tr:open source)
       (tr:gpg "" `(--yes --encrypt --recipient ,dsa-usrname2
			  --cipher-algo ,cipher))
       (tr:gpg "" '(--yes))
       (tr:assert-identity source)))
    (append plain-files data-files)))
 all-cipher-algos)
