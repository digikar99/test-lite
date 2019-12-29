(defsystem "test-lite"
  :version "0.1.0"
  :author "Shubhamkar B. Ayare"
  :license "MIT"
  :depends-on ("cl-ppcre"
               "cl-ansi-text"
               "cl-colors"
               "alexandria"
               "uiop")
  :components ((:file "test-lite")))
