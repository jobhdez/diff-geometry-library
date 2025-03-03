(uiop:define-package diff-geometry
    (:use #:cl))

(in-package #:diff-geometry)

(defclass manifold ()
  ((dimension :initarg :dimension :initform 2 :accessor manifold-dimension)
   (name :initarg :name :initform 'M :accessor manifold-name)
   (structure :initarg :structure :initform 'Euclidean :accessor manifold-structure)  ;; Corrected spelling from Euclidian
   (chart :initarg :chart :initform #() :accessor manifold-chart)
   (point :initarg :point :initform #() :accessor manifold-point)
   (tangent-space :initarg :tangent-space :initform #() :accessor manifold-tangent-space)
   (metric :initarg :metric :initform #() :accessor manifold-metric))  ;; Changed field name to :metric
  (:documentation "A manifold with a given dimension, name, and structure."))

(defclass metric ()
  ((lorentzian :initarg :lorentzian :initform #() :accessor metric-lorentzian)
   (christoffel :initarg :christoffel :initform #() :accessor metric-christoffel))  ;; Fixed typo "chrisoffel" to "christoffel"
  (:documentation "A Lorentzian metric for a 4 Dimensional Smooth Manifold."))

(defclass chart ()
  ((name :initarg :name :initform 'C :accessor chart-name)
   (coordinates :initarg :coordinates :accessor chart-coordinates)
   (fn :initarg :coordinate :accessor chart-fn))  ;; Fixed argument name to be more consistent with method definition
  (:documentation "A chart for a given manifold"))

(defclass tangent-space ()
  ((name :initarg :name :accessor tangent-space-name)
   (dimension :initarg :dimension :accessor tangent-space-dimension)
   (point :initarg :point :accessor tangent-space-point)
   (chart :initarg :chart :accessor tangent-space-chart)
   (vector-bases :initarg :vector-bases :accessor tangent-space-vector-bases))  ;; Fixed typo "vector-spaces" to "vector-bases"
  (:documentation "A tangent space to associated with a given manifold M at point p."))

(defgeneric indices (manifold)
  (:documentation "Returns indices of given manifold."))

(defgeneric chart (manifold)
  (:documentation "Make a chart of the given manifold"))

(defgeneric set-chart (manifold chart)
  (:documentation "Sets the chart for a given manifold."))

(defgeneric fn (chart)
  (:documentation "Function for a given chart of a given manifold."))

(defgeneric point (manifold)
  (:documentation "Assign a point p on given Manifold's chart."))

(defgeneric tangent-space (manifold)
  (:documentation "A tangent vector space associated with a given manifold at point p."))

(defgeneric vector-bases (tangent-space)
  (:documentation "Vector bases associated with a given manifold."))

(defgeneric metric (manifold)
  (:documentation "The metric tensor of the manifold M."))

(defmethod indices ((m manifold))
  (let ((n (- (manifold-dimension m) 1))
        (*indices* (make-array `(,(+ n 1)))))
    (dotimes (i n)
      (setf (aref *indices* i) i))  ;; Corrected array usage
    *indices*))

(defmethod chart ((m manifold))
  (manifold-chart m))

(defmethod set-chart ((m manifold) chart)
  (setf (manifold-chart m) chart))

(defmethod fn ((c chart))
  (chart-fn c))

(defmethod point ((m manifold))
  (let ((chart (manifold-chart m))
        (point (manifold-point m)))
    (if (equal (length chart) (length point))
        point
        (error "Point ~s is not defined by chart coordinates."))))

(defmethod tangent-space ((m manifold))
  (let ((space (manifold-tangent-space m)))
    (let ((point (manifold-point m)))
      (let ((chart (manifold-chart m)))
        (setf (tangent-space-point space) point)
        (setf (tangent-space-chart space) chart)
        space))))

(defmethod vector-bases ((tspace tangent-space))
  (let ((chart (tangent-space-chart tspace)))
    (let ((n (- (length chart) 1)))
      (let ((coordinates (make-array `(,(length chart)) :initial-contents chart)))
        (let ((bases (make-array `(,(+ n 1)))))
          (dotimes (i n)
            (setf (aref bases i) `(/ partial-diff (* partial-diff ,(aref coordinates i)))))
          bases)))))

(defmethod metric ((m manifold))
  (let ((metric (metric-lorentzian (manifold-metric m)))
        (matrix-metric (make-array '(4 4))))
    (dotimes (i 4)
      (dotimes (j 4)
        (if (= i j)
            (setf (aref matrix-metric i j) (aref metric i))
            (setf (aref matrix-metric i j) 0))))
    (setf (manifold-metric m) matrix-metric)))

(defun make-manifold (dimension name structure)
  (make-instance 'manifold :dimension dimension :name name :structure structure))

(defun make-metric (lorentzian)
  (make-instance 'metric :lorentzian lorentzian))

(defparameter manifold (make-manifold 4 'M 'Lorentzian))
(defparameter metric (make-metric #('-(1 - 2 * m / r) '(1 / 1 - 2 * m / r) '(r ^ 2) '(r * sin(th)^2))))

(setf (manifold-metric manifold) metric)
