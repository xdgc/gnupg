;; A read-evaluate-print-loop for gpgscm.
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

;; Interactive repl using 'prompt' function.  P must be a function
;; that given the current entered prefix returns the prompt to
;; display.
(define (repl p)
  (let ((repl-environment (make-environment)))
    (call/cc
     (lambda (exit)
       (let loop ((prefix ""))
	 (let ((line (prompt (p prefix))))
	   (if (and (not (eof-object? line)) (= 0 (string-length line)))
	       (exit (loop prefix)))
	   (if (not (eof-object? line))
	       (let* ((next (string-append prefix line))
		      (c (catch (begin (echo "Parse error:" *error*)
				       (loop prefix))
				(read (open-input-string next)))))
		 (if (not (eof-object? c))
		     (begin
		       (catch (echo "Error:" *error*)
			      (echo "    ===>" (eval c repl-environment)))
		       (exit (loop ""))))
		 (exit (loop next))))))))))

(define (prompt-append-prefix prompt prefix)
  (string-append prompt (if (> (string-length prefix) 0)
			    (string-append prefix "...")
			    "> ")))

;; Default repl run by main.c.
(define (interactive-repl)
  (repl (lambda (p) (prompt-append-prefix "gpgscm " p))))