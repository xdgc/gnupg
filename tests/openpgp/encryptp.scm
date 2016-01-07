#!/usr/bin/env gpgscm

(load (in-srcdir "defs.scm"))

(define :read-end car)
(define :write-end cadr)

(define (gpg-pipe args errfd)
  (lambda (source sink)
    (let* ((p (pipe))
	   (task0 (spawn-process-fd
		   `(,GPG --output - --yes ,@args ,source)
		   -1 (:write-end p) errfd))
	   (task1 (spawn-process-fd
		   `(,GPG --output ,sink --yes -)
		   (:read-end p) -1 errfd)))
      (close (:read-end p))
      (close (:write-end p))
      (wait-processes (list GPG GPG) (list task0 task1) 1))))

(for-each-p
 "Test encryption and decryption using pipes"
 (lambda (source)
   (check-identity source
		   (gpg-pipe `(--always-trust --encrypt
					      --recipient ,usrname2)
			     (if *verbose* STDERR_FILENO -1))))
 (append plain-files data-files))
