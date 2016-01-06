;; Additional library functions for TinySCHEME.
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

(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst))
         (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))

(define (any p l)
  (cond ((null? l) #f)
        ((p (car l)) #t)
        (else (any p (cdr l)))))

;; Is s1 a prefix of s2 ?
(define (string-prefix? s1 s2)
  (and (>= (string-length s2) (string-length s1))
       (string=? s1 (substring s2 0 (string-length s1)))))

;; Given a list of prefixes, does s start with any of them ?
(define (string-prefix-any? lp s)
  (any (lambda (p) (string-prefix? p s)) lp))

;; Locate the first occurrence of needle in haystack.
(define (string-index haystack needle)
  (define (index i haystack needle)
    (if (= (length haystack) 0)
        #f
        (if (char=? (car haystack) needle)
            i
            (index (+ i 1) (cdr haystack) needle))))
  (index 0 (string->list haystack) needle))

;; Split haystack at delimiter at most n times.
(define (string-splitn haystack delimiter n)
  (define (split acc haystack delimiter n)
    (if (= (string-length haystack) 0)
        (reverse acc)
        (let ((i (string-index haystack delimiter)))
          (if (not (or (eq? i #f) (= 0 n)))
              (split (cons (substring haystack 0 i) acc)
                     (substring haystack (+ i 1) (string-length haystack))
                     delimiter (- n 1))
              (split (cons haystack acc) "" delimiter 0)
              ))))
  (split '() haystack delimiter n))

;; Split haystack at delimiter.
(define (string-split haystack delimiter)
  (string-splitn haystack delimiter -1))

;; Drop whitespace.
(define (filter-whitespace s)
  (list->string (filter (lambda (c) (not (char=? #\newline c))) (string->list s))))

(define (echo . msg)
  (for-each (lambda (x) (display x) (display " ")) msg)
  (newline))
