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

(define (call-with-progress msg what)
  (display msg)
  (newline)
  (display "    > ")
  (what (lambda (item)
	  (display item)
	  (display " ")))
  (display "< ")
  (newline))

;; Process management.
(define (call what)
  (wait-process (car what) (spawn-process-fd what -1 -1 2) 1))

(define (call-with-io what in)
  (let ((h (spawn-process' what 0)))
    (es-write (:stdin h) in)
    (es-fclose (:stdin h))
    (let ((out (es-read (:stdout h) 1024))
	  (err (es-read (:stderr h) 1024)))
      (es-fclose (:stdout h))
      (es-fclose (:stderr h))
      ;; maybe wait earlier??
      (list (wait-process (car what) (:pid h) 1) out err))))

(define :stdin car)
(define :stdout cadr)
(define :stderr caddr)
(define :pid cadddr)

;; File management.
(define (in-srcdir what)
  (string-append (getenv "srcdir") "/" what))
