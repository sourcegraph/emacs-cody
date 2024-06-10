(require 'ert)
(require 'cody)

;; Unit tests
(ert-deftest test-cody--uri-for-unix-path ()
  "Test cody--uri-for with a Unix-style path."
  (should (equal (cody--uri-for "/Users/stevey/src/sg/cody/agent/src/agent.ts")
                 "file:///Users/stevey/src/sg/cody/agent/src/agent.ts")))

(ert-deftest test-cody--uri-for-windows-path ()
  "Test cody--uri-for with a Windows-style path."
  (should (equal (cody--uri-for "C:\\Users\\stevey\\src\\sg\\cody\\agent\\src\\agent.ts")
                 "file:///c%3A/Users/stevey/src/sg/cody/agent/src/agent.ts")))

(ert-deftest test-cody--uri-for-remote ()
  "Test cody--uri-for with a remote path."
  (should (equal (cody--uri-for "/ssh:remotehost:/path/to/remote/file")
                 "/ssh:remotehost:/path/to/remote/file")))

(ert-deftest test-cody--uri-for-invalid-input ()
  "Test cody--uri-for with invalid input."
  (should (equal (cody--uri-for nil) "*scratch*")))

;; Run tests
;;(ert-run-tests-batch-and-exit)
