#|
  This file is a part of tensor-contraction-mcts project.
  Copyright (c) 2019 Selwyn Simsek (sgs16@ic.ac.uk)
|#

#|
  Author: Selwyn Simsek (sgs16@ic.ac.uk)
|#

(defsystem "tensor-contraction-mcts"
  :version "0.1.0"
  :author "Selwyn Simsek"
  :license ""
  :depends-on ("micmac")
  :components ((:module "src"
                :components
                ((:file "tensor-contraction-mcts"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "tensor-contraction-mcts-test"))))
