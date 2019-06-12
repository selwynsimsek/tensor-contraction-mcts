#|
  This file is a part of tensor-contraction-mcts project.
  Copyright (c) 2019 Selwyn Simsek (sgs16@ic.ac.uk)
|#

(defsystem "tensor-contraction-mcts-test"
  :defsystem-depends-on ("prove-asdf")
  :author "Selwyn Simsek"
  :license ""
  :depends-on ("tensor-contraction-mcts"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "tensor-contraction-mcts"))))
  :description "Test system for tensor-contraction-mcts"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
