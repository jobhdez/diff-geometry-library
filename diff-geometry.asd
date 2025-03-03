(defsystem "diff-geometry"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "diff-geometry/tests"))))

(defsystem "diff-geometry/tests"
  :author ""
  :license ""
  :depends-on ("diff-geometry"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for diff-geometry"
  :perform (test-op (op c) (symbol-call :rove :run c)))
