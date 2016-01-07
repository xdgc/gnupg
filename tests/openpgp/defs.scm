;; Common definitions for the OpenPGP test scripts.
;;
;; Copyright (C) 2016 g10 Code GmbH
;;
;; This file is part of GnuPG.
;;
;; GnuPG is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; GnuPG is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, see <http://www.gnu.org/licenses/>.

;;
;; Constants.
;;

(define usrname1 "one@example.com")
(define usrpass1 "def")
(define usrname2 "two@example.com")
(define usrpass2 "")
(define usrname3 "three@example.com")
(define usrpass3 "")

(define dsa-usrname1 "pgp5")
;; we use the sub key because we do not yet have the logic to to derive
;; the first encryption key from a keyblock (I guess) (Well of course
;; we have this by now and the notation below will lookup the primary
;; first and then search for the encryption subkey.)
(define dsa-usrname2 "0xCB879DE9")

(define plain-files '("plain-1" "plain-2" "plain-3"))
(define data-files '("data-500" "data-9000" "data-32000" "data-80000"))
(define exp-files '())

(define GPG "../../g10/gpg2")

(define (get-config what)
  (let* ((config-string
	  (call-popen `(,GPG --with-colons --list-config ,what) ""))
	 (config (string-splitn
		  (filter-whitespace config-string) #\: 2)))
    (string-split (caddr config) #\;)))

(define all-pubkey-algos (get-config "pubkeyname"))
(define all-hash-algos (get-config "digestname"))
(define all-cipher-algos (get-config "ciphername"))

(define (have-pubkey-algo? x)
  (not (null? (memq x all-pubkey-algos))))
(define (have-hash-algo? x)
  (not (null? (memq x all-hash-algos))))
(define (have-cipher-algo? x)
  (not (null? (memq x all-cipher-algos))))
