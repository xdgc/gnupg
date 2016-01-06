(echo "Testing process and IPC primitives...")

(define (qualify executable)
  (string-append executable (getenv "EXEEXT")))

(assert (= 0 (call `(,(qualify "t-child") "return0"))))
(assert (= 1 (call `(,(qualify "t-child") "return1"))))
(assert (= 77 (call `(,(qualify "t-child") "return77"))))

(let ((r (call-with-io `(,(qualify "t-child") "return0") "")))
  (assert (= 0 (:retcode r)))
  (assert (string=? "" (:stdout r)))
  (assert (string=? "" (:stderr r))))

(let ((r (call-with-io `(,(qualify "t-child") "return1") "")))
  (assert (= 1 (:retcode r)))
  (assert (string=? "" (:stdout r)))
  (assert (string=? "" (:stderr r))))

(let ((r (call-with-io `(,(qualify "t-child") "return77") "")))
  (assert (= 77 (:retcode r)))
  (assert (string=? "" (:stdout r)))
  (assert (string=? "" (:stderr r))))

(let ((r (call-with-io `(,(qualify "t-child") "hello_stdout") "")))
  (assert (= 0 (:retcode r)))
  (assert (string=? "hello" (:stdout r)))
  (assert (string=? "" (:stderr r))))

(let ((r (call-with-io `(,(qualify "t-child") "hello_stderr") "")))
  (assert (= 0 (:retcode r)))
  (assert (string=? "" (:stdout r)))
  (assert (string=? "hello" (:stderr r))))

(let ((r (call-with-io `(,(qualify "t-child") "cat") "hellohello")))
  (assert (= 0 (:retcode r)))
  (assert (string=? "hellohello" (:stdout r)))
  (assert (string=? "" (:stderr r))))

(echo "All good.")
