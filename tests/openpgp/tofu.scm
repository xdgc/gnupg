#!/usr/bin/env gpgscm

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

(load (with-path "defs.scm"))

 ;; Redefine GPG without --always-trust and a fixed time.
(define GPG `(,(tool 'gpg) --no-permission-warning
	      --faked-system-time=1466684990))
(define GNUPGHOME (getenv "GNUPGHOME"))
(if (string=? "" GNUPGHOME)
    (error "GNUPGHOME not set"))

(catch (skip "Tofu not supported")
       (call-check `(,@GPG --trust-model=tofu --list-config)))

(define KEYS '("2183839A" "BC15C85A" "EE37CF96"))

;; Import the test keys.
(call-check `(,@GPG --import ,(in-srcdir "tofu-keys.asc")))

;; Make sure the keys are imported.
(for-each (lambda (keyid)
	    (catch (error "Missing key" keyid)
		   (call-check `(,@GPG --list-keys ,keyid))))
	  KEYS)

;; Get tofu policy for KEYID.  Any remaining arguments are simply
;; passed to GPG.
;;
;; This function only supports keys with a single user id.
(define (getpolicy keyid . args)
  (let ((policy
	 (list-ref (assoc "tfs" (gpg-with-colons
				 `(--trust-model=tofu --with-tofu-info
				   ,@args
				   --list-keys ,keyid))) 5)))
    (unless (member policy '("auto" "good" "unknown" "bad" "ask"))
	    (error "Bad policy:" policy))
    policy))

;; Check that KEYID's tofu policy matches EXPECTED-POLICY.  Any
;; remaining arguments are simply passed to GPG.
;;
;; This function only supports keys with a single user id.
(define (checkpolicy keyid expected-policy . args)
  (let ((policy (apply getpolicy `(,keyid ,@args))))
    (unless (string=? policy expected-policy)
	    (error keyid ": Expected policy to be" expected-policy
		   "but got" policy))))

;; Get the trust level for KEYID.  Any remaining arguments are simply
;; passed to GPG.
;;
;; This function only supports keys with a single user id.
(define (gettrust keyid . args)
  (let ((trust
	 (list-ref (assoc "pub" (gpg-with-colons
				 `(--trust-model=tofu
				   ,@args
				   --list-keys ,keyid))) 1)))
    (unless (and (= 1 (string-length trust))
		 (member (string-ref trust 0) (string->list "oidreqnmfuws-")))
	    (error "Bad trust value:" trust))
    trust))

;; Check that KEYID's trust level matches EXPECTED-TRUST.  Any
;; remaining arguments are simply passed to GPG.
;;
;; This function only supports keys with a single user id.
(define (checktrust keyid expected-trust . args)
  (let ((trust (apply gettrust `(,keyid ,@args))))
    (unless (string=? trust expected-trust)
	    (error keyid ": Expected trust to be" expected-trust
		   "but got" trust))))

;; Set key KEYID's policy to POLICY.  Any remaining arguments are
;; passed as options to gpg.
(define (setpolicy keyid policy . args)
  (call-check `(,@GPG --trust-model=tofu ,@args
		      --tofu-policy ,policy ,keyid)))

(info "Checking tofu policies and trust...")

;; Carefully remove the TOFU db.
(catch '() (unlink (string-append GNUPGHOME "/tofu.db")))
(catch '() (unlink-recursively (string-append GNUPGHOME "/tofu.d")))

;; Verify a message.  There should be no conflict and the trust
;; policy should be set to auto.
(call-check `(,@GPG --trust-model=tofu
		    --verify ,(in-srcdir "tofu-2183839A-1.txt")))

(checkpolicy "2183839A" "auto")
;; Check default trust.
(checktrust "2183839A" "m")

;; Trust should be derived lazily.  Thus, if the policy is set to
;; auto and we change --tofu-default-policy, then the trust should
;; change as well.  Try it.
(checktrust "2183839A" "f" '--tofu-default-policy=good)
(checktrust "2183839A" "-" '--tofu-default-policy=unknown)
(checktrust "2183839A" "n" '--tofu-default-policy=bad)

;; Change the policy to something other than auto and make sure the
;; policy and the trust are correct.
(for-each-p
 "Setting a fixed policy..."
 (lambda (policy)
   (let ((expected-trust
	  (cond
	   ((string=? "good" policy) "f")
	   ((string=? "unknown" policy) "-")
	   (else "n"))))
     (setpolicy "2183839A" policy)

     ;; Since we have a fixed policy, the trust level shouldn't
     ;; change if we change the default policy.
     (for-each-p
      ""
      (lambda (default-policy)
	(checkpolicy "2183839A" policy
		     '--tofu-default-policy default-policy)
	(checktrust "2183839A" expected-trust
		    '--tofu-default-policy default-policy))
      '("auto" "good" "unknown" "bad" "ask"))))
 '("good" "unknown" "bad"))

;; BC15C85A conflicts with 2183839A.  On conflict, this will set
;; BC15C85A to ask.  If 2183839A is auto (it's not, it's bad), then
;; it will be set to ask.
(call-check `(,@GPG --trust-model=tofu
		    --verify ,(in-srcdir "tofu-BC15C85A-1.txt")))
(checkpolicy "BC15C85A" "ask")
(checkpolicy "2183839A" "bad")

;; EE37CF96 conflicts with 2183839A and BC15C85A.  We change
;; BC15C85A's policy to auto and leave 2183839A's policy at bad.
;; This conflict should cause BC15C85A's policy to be changed to
;; ask (since it is auto), but not affect 2183839A's policy.
(setpolicy "BC15C85A" "auto")
(checkpolicy "BC15C85A" "auto")
(call-check `(,@GPG --trust-model=tofu
		    --verify ,(in-srcdir "tofu-EE37CF96-1.txt")))
(checkpolicy "BC15C85A" "ask")
(checkpolicy "2183839A" "bad")
(checkpolicy "EE37CF96" "ask")
