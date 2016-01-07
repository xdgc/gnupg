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

;; XXX below are three variants to spawn the child process. run-tests'
;; is my favorite, but it doesn't display the childs stdout and err in
;; our terminal, but spawns a separate.  For the time being,
;; run-tests'' works okayish.

;; XXX this hangs when starting an gpg-agent on demand, and also
;; displays stdout and stderr in the wrong way (it doesn't display
;; them in the order they are written by the child).
(define (run-tests . tests)
  (let ((n 0) (nfail 0) (nskip 0))
    (for-each
     (lambda (test)
       (let ((result (call-with-io test "")))
	 (++! n)
	 (if (= 0 (:retcode result))
	     (echo "PASS:" test)
	     (begin
	       (if (= 77 (:retcode result))
		   (++! nskip))
	       (++! nfail)))
	 (if (or *verbose* (not (= 0 (:retcode result))))
	     (begin
	       (display (:stdout result))
	       (display (:stderr result))))))
     tests)
    (echo n "tests run," (- n nfail nskip) "succeeded," nfail "failed,"
	  nskip "skipped.")
    (exit nfail)))

;; xxx this works, but opens new windows for the child, does not
;; display stdout or err either. maybe write to temporary file.
(define (run-tests' . tests)
  (let ((n 0) (nfail 0) (nskip 0))
    (for-each
     (lambda (test)
       (let ((result (call-with-fds test CLOSED_FD STDOUT_FILENO STDERR_FILENO)))
	 (++! n)
	 (cond
	  ((= 0 result)
	   (echo "PASS:" test))
	  ((= 77 result)
	   (echo "SKIP:" test)
	   (++! nskip))
	  (else
	   (echo "FAIL:" test)
	   (++! nfail)))))
     tests)
    (echo n "tests run," (- n nfail nskip) "succeeded," nfail "failed,"
	  nskip "skipped.")
    (exit nfail)))

;; works, but doesn't preserve the order of messages either
(define (run-tests'' . tests)
  (let ((n 0) (nfail 0) (nskip 0))
    (for-each
     (lambda (test)
       (lettmp (messages)
	 (let ((result (letfd ((msgfd (open messages (logior O_WRONLY O_CREAT) #o600)))
			      (call-with-fds test CLOSED_FD msgfd msgfd))))
	   (++! n)
	   (letfd ((msgfd (open messages O_RDONLY))) (splice msgfd STDOUT_FILENO))
	   (cond
	    ((= 0 result)
	     (echo "PASS:" test))
	    ((= 77 result)
	     (echo "SKIP:" test)
	     (++! nskip))
	    (else
	     (echo "FAIL:" test)
	     (++! nfail))))))
     tests)
    (echo n "tests run," (- n nfail nskip) "succeeded," nfail "failed,"
	  nskip "skipped.")
    (exit nfail)))

(define (gpgscm . args)
  (append (list *argv0* '--base *basedir*) args))

(run-tests''
 (gpgscm (in-srcdir "setup.scm"))
 (gpgscm (in-srcdir "version.scm"))
 (gpgscm (in-srcdir "mds.scm"))
 (gpgscm (in-srcdir "decrypt.scm"))
 (gpgscm (in-srcdir "sigs.scm"))
 (gpgscm (in-srcdir "encrypt.scm"))
 (gpgscm (in-srcdir "seat.scm"))
 (gpgscm (in-srcdir "clearsig.scm"))
 ;; this one hangs on w32, as does the pipe-thing in setup.scm. we're
 ;; doing something wrong there...
 ;;(gpgscm (in-srcdir "encryptp.scm"))
 (gpgscm (in-srcdir "4gb-packet.scm"))
 (gpgscm (in-srcdir "finish.scm")))
