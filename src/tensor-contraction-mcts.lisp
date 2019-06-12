(defpackage tensor-contraction-mcts
  (:use :cl))
(in-package :tensor-contraction-mcts)

(defvar *example-tensor-list* '((p i) (q j) (i j k l) (r k) (s l)))

(defclass tensor-contraction-node (micmac.uct:uct-node)
  ((tensor-list :accessor tensor-list
                :initarg :tensor-list
                :type list)))

(defclass tensor-contraction-edge (micmac.uct:uct-edge)
  ((first-tensor :accessor first-tensor
                 :initarg :first-tensor
                 :type integer)
   (second-tensor :accessor second-tensor
                  :initarg :second-tensor
                  :type integer)))

(defmethod micmac.uct:list-edges ((node tensor-contraction-node) state)
  (loop for i from 0 below (length (tensor-list node)) appending
        (loop for j from (+ 1 i) below (length (tensor-list node)) collecting
              (make-instance 'tensor-contraction-edge
                             :first-tensor i
                             :second-tensor j
                             :from-node node))))

(defmethod micmac.uct:state ((node tensor-contraction-node) parent edge parent-state)
  (max parent-state (tensor-contraction-scaling parent edge)))

(defmethod micmac.uct:play-out ((node tensor-contraction-node) state path)
  "Uses random playout."
  (if (= 1 (length (tensor-list node)))
      state
      (max state
           (let ((moves (list node)))
             (loop with current-node = node
                   with scaling = 0
                   do (let ((edge (random-edge (length (tensor-list current-node))
                                               node)))
                        (setf scaling (tensor-contraction-scaling current-node edge))
                        (push (setf current-node (micmac.uct:make-uct-node
                                                  current-node edge 0))
                              moves))
                   maximize scaling
                   until (= (length (tensor-list current-node)) 1))))))

(defmethod micmac.uct:make-uct-node ((parent tensor-contraction-node) (edge tensor-contraction-edge) parent-state)
  (make-instance 'tensor-contraction-node
                 :tensor-list (contract-tensor-list (tensor-list parent)
                                                    (first-tensor edge)
                                                    (second-tensor edge))))

(defmethod micmac.uct:outcome->reward ((node tensor-contraction-node) outcome) (- outcome))
(defun contract-tensors (first-tensor second-tensor) (set-exclusive-or first-tensor second-tensor))
(defun contract-tensor-list (tensor-list first-tensor-index second-tensor-index)
  (append (remove (nth first-tensor-index tensor-list)
                  (remove (nth second-tensor-index tensor-list) tensor-list))
          (list (contract-tensors (nth first-tensor-index tensor-list)
                                  (nth second-tensor-index tensor-list)))))

(defun tensor-contraction-scaling (tensor-node tensor-edge)
  (with-slots (tensor-list) tensor-node
    (with-slots (first-tensor second-tensor) tensor-edge
      (length (union (nth first-tensor tensor-list)
                     (nth second-tensor tensor-list))))))

(defun random-edge (n &optional node)
  (let ((list (alexandria:random-elt
               (loop for i from 0 below n appending
                     (loop for j from (+ 1 i) below n collecting (list i j))))))
    (make-instance 'tensor-contraction-edge :from-node node
                                            :first-tensor (first list)
                                            :second-tensor (second list))))

(defun do-mcts (tensor-list &key (max-n-playouts 100))
  (let ((result
          (micmac.uct:uct :root (make-instance 'tensor-contraction-node :tensor-list tensor-list)
                          :fresh-root-state (lambda () 0)
                          :exploration-bias 1
                          :max-n-playouts max-n-playouts)))
    (multiple-value-bind (child-node scaling)
        (most-visited-node result)
      (format t "~a~a~@[ ~%[~a]=> ~]" #\Tab (tensor-list result) scaling)
      (if scaling
          (max scaling (do-mcts (tensor-list child-node)))
          0))))

(defun most-visited-node (node)
  (when node
    (let ((edge (alexandria:extremum (micmac.uct:edges node) #'> :key #'micmac.uct::n-visits)))
      (when edge (values (micmac.uct:to-node edge)
                         (tensor-contraction-scaling node edge))))))


(defun random-equation (n regularity &key (n-out 0))
  (let* ((number-of-indices (floor (+ n-out
                                      (/ (* n regularity)
                                         2))))
         (indices (alexandria:shuffle
                          (loop for i from (char-code #\A) repeat number-of-indices
                                when (< i (+ (char-code #\A) n-out))
                                collecting #1=(intern (format nil "~a" (code-char i)))
                                when (>= i (+ (char-code #\A) n-out))
                                appending (list #1# #1#))))
         (output (make-list n :initial-element nil)))
    (loop for index in indices
          for count from 0
          do
          (let ((random-tensor
                  (if (< count n)
                      count
                      (loop with candidate = #2=(random (length output))
                            do (if (member index (nth candidate output))
                                   (setf candidate #2#)
                                   (return candidate))))))
            (push index (nth random-tensor output)))
          finally (return output))))
