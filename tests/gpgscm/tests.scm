;; Common definitions for writing tests.
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

(define (trace x)
  (display x)
  (newline)
  x)

;; Reporting.
(define (info msg)
  (display msg)
  (newline))

(define (error msg)
  (display msg)
  (newline)
  (exit 1))

(define (skip msg)
  (display msg)
  (newline)
  (exit 77))

(define (make-counter)
  (let ((c 0))
    (lambda ()
      (let ((r c))
	(set! c (+ 1 c))
	r))))

(macro (assert form)
  `(if (not ,(cadr form))
       (error (list "Assertion failed:" (quote ,(cadr form))))))
(assert #t)

(define *progress-nesting* 0)

(define (call-with-progress msg what)
  (set! *progress-nesting* (+ 1 *progress-nesting*))
  (if (= 1 *progress-nesting*)
      (begin
	(display msg)
	(newline)
	(display "    > ")
	(what (lambda (item)
	      (display item)
	      (display " ")))
	(display "< ")
	(newline))
      (begin
	(what (lambda (item) (display ".")))
	(display " ")))
  (set! *progress-nesting* (- *progress-nesting* 1)))

(define (for-each-p msg proc lst)
  (for-each-p' msg proc (lambda (x) x) lst))

(define (for-each-p' msg proc fmt lst)
  (call-with-progress
   msg
   (lambda (progress)
     (for-each (lambda (a)
		 (progress (fmt a))
		 (proc a))
	       lst))))

;; Process management.
(define CLOSED_FD -1)
(define (call-with-fds what infd outfd errfd)
  (wait-process (car what) (spawn-process-fd what infd outfd errfd) 1))
(define (call what)
  (call-with-fds what CLOSED_FD STDOUT_FILENO STDERR_FILENO))
(define (call-check what)
  (if (not (= 0 (call what)))
      (throw (list what "failed"))))

;; Accessor functions for the results of
(define :stdin car)
(define :stdout cadr)
(define :stderr caddr)
(define :pid cadddr)


(define (call-with-io what in)
  (let ((h (spawn-process' what 0)))
    (es-write (:stdin h) in)
    (es-fclose (:stdin h))
    (let* (
	   (out (es-read-all (:stdout h)))
	   (err (es-read-all (:stderr h)))
	   (result (wait-process (car what) (:pid h) 1))
	   )
      (es-fclose (:stdout h))
      (es-fclose (:stderr h))
      ;; maybe wait earlier??
      (list result out err))))

(define :retcode car)

(define (call-popen command input-string)
  (let ((result (call-with-io command input-string)))
    (if (= 0 (:retcode result))
	(:stdout result)
	(throw (:stderr result)))))

;;
;; estream helpers.
;;

(define (es-read-all stream)
  (let loop
      ((acc ""))
    (if (es-feof stream)
	acc
	(loop (string-append acc (es-read stream 4096))))))

;; File management.
(define (in-srcdir what)
  (string-append (getenv "srcdir") "/" what))

(define (basename path)
  (let ((i (string-index path #\/)))
    (if (equal? i #f)
	path
	(basename (substring path (+ 1 i) (string-length path))))))

;; Helper for (pipe).
(define :read-end car)
(define :write-end cadr)

;; let-like macro that manages file descriptors.
;;
;; (letfd <bindings> <body>)
;;
;; Bind all variables given in <bindings> and initialize each of them
;; to the given initial value, and close them after evaluting <body>.
(macro (letfd form)
  (let ((result-sym (gensym)))
    `((lambda (,(caaadr form))
	(let ((,result-sym
	       ,(if (= 1 (length (cadr form)))
		    `(begin ,@(cddr form))
		    `(letfd ,(cdadr form) ,@(cddr form)))))
	  (close ,(caaadr form))
	  ,result-sym)) ,@(cdaadr form))))

;; let-like macro that manages temporary files.
;;
;; (lettmp <bindings> <body>)
;;
;; Bind all variables given in <bindings>, initialize each of them to
;; a string representing an unique path in the filesystem, and delete
;; them after evaluting <body>.
(macro (lettmp form)
  (let ((result-sym (gensym)))
    `((lambda (,(caadr form))
	(let ((,result-sym
	       ,(if (= 1 (length (cadr form)))
		    `(begin ,@(cddr form))
		    `(lettmp ,(cdadr form) ,@(cddr form)))))
	  (catch #t
	    (let* ((filename ,(caadr form))
		   (len (string-length filename))
		   (dirname (substring 0 (- len 2))))
	      (unlink filename)
	      (rmdir dirname)))
	  ,result-sym)) (string-append (mkdtemp "gpgscm-XXXXXX") "/a"))))

(define (check-execution source transformer)
  (lettmp (sink)
	  (transformer source sink)))

(define (check-identity source transformer)
  (lettmp (sink)
	  (transformer source sink)
	  (if (not (file=? source sink))
	      (error "mismatch"))))

;; Increment the given variable.
(macro (++! form)
  `(set! ,(cadr form) (+ 1 ,(cadr form))))
