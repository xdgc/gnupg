;; Test-suite runner.
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

(if (string=? "" (getenv "srcdir"))
    (begin
      (echo "Environment variable 'srcdir' not set.  Please point it to"
	    "tests/openpgp.")
      (exit 2)))

(load (string-append (getenv "srcdir") "/defs.scm"))

(define (run-tests . tests)
  (let ((n 0) (nfail 0) (nskip 0))
    (for-each
     (lambda (test)
       (let* ((cmd (car test))
	      (name (cadr test))
	      (p (inbound-pipe))
	      (pid (spawn-process-fd cmd CLOSED_FD
				     (:write-end p) (:write-end p))))
	 (close (:write-end p))
	 (splice (:read-end p) STDERR_FILENO)
	 (close (:read-end p))
	 (let ((result (wait-process name pid 1)))
	   (++! n)
	   (cond
	    ((= 0 result)
	     (echo "PASS:" name))
	    ((= 77 result)
	     (echo "SKIP:" name)
	     (++! nskip))
	    (else
	     (echo "FAIL:" name)
	     (++! nfail))))))
     tests)
    (echo n "tests run," (- n nfail nskip) "succeeded," nfail "failed,"
	  nskip "skipped.")
    (exit nfail)))

(define (verbosity n)
  (if (= 0 n) '() (cons '--verbose (verbosity (- n 1)))))

(define (test name . args)
  `((,*argv0* --base ,*basedir* ,@(verbosity *verbose*) ,@args
	      ,(in-srcdir name))
    ,name))

(run-tests
 (test "setup.scm")
 (test "version.scm")
 (test "mds.scm")
 (test "decrypt.scm")
 (test "sigs.scm")
 (test "encrypt.scm")
 (test "seat.scm")
 (test "clearsig.scm")
 (test "encryptp.scm")
 (test "detach.scm")
 (test "detachm.scm")
 (test "armsigs.scm")
 (test "armencrypt.scm")
 (test "armencryptp.scm")
 (test "signencrypt.scm")
 (test "armsignencrypt.scm")
 (test "armdetach.scm")
 (test "armdetachm.scm")
 (test "genkey1024.scm")
 (test "conventional.scm")
 (test "conventional-mdc.scm")
 (test "multisig.scm")
 (test "4gb-packet.scm")
 (test "finish.scm"))
