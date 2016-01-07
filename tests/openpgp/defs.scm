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

(define GPG "../../g10/gpg2")

(define (get-config what)
  (let ((h (spawn-process' `(,GPG --with-colons --list-config ,what) 0)))
    (es-fclose (:stdin h))
    (es-fclose (:stderr h))
    (let* ((cfg (string-splitn
		 (filter-whitespace (es-read (:stdout h) 1024)) #\: 2))
	   (values (string-split (caddr cfg) #\;)))
      (es-fclose (:stdout h))
      (wait-process GPG (:pid h) 1)
      values)))

(define all-hash-algos (get-config "digestname"))
(define all-cipher-algos (get-config "ciphername"))

(define (have-hash-algo? x)
  (not (null? (memq x all-hash-algos))))
(define (have-cipher-algo? x)
  (not (null? (memq x all-cipher-algos))))
