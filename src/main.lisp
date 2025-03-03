(uiop:define-package diff-geometry
    (:use #:cl))
(in-package #:diff-geometry)

(defclass manifold ()
  ((dimension :initarg :dimension :accessor manifold-dimension)
   (name :initarg :name :accessor manifold-name)
   (structure :initarg :structure :accessor manifold-structure)
   (chart :initarg :chart :accessor manifold-chart)
   (point :initarg :point :accessor manifold-point)
   (tangent-space :initarg :tangent-space :accessor  manifold-tangent-space))
  (:documentation "A manifold with a given dimension, name, and structure."))

(defclass chart ()
  ((name :initarg :name :accessor chart-name)
   (coordinates :initarg :coordinates :accessor chart-coordinates)
   (fn :initarg :coordinate :accessor chart-fn))
  (:documentation "A chart for a given manifold"))

(defclass tangent-space ()
  ((name :initarg :name :accessor tangent-space-name)
   (dimension :initarg :dimension :accessor tangent-space-dimension)
   (point :initarg :point :accessor tangent-space-point)
   (chart :initarg :chart :accessor tangent-space-chart)
   (vector-bases :initarg :vector-bases :accessor tangent-space-vector-spaces))
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

(defmethod indices ((m manifold))
  (let ((n (- (manifold-dimension m) 1))
	(*indices* (make-array `(,(+ n 1)))))
    (progn
      (dotimes (i n)
	(setf (aref arr i) i))
      *indices*)))

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
	(progn (setf (tangent-space-point space) point)
	       (setf (tangent-space-chart space) chart)
	       space)))))

(defmethod vector-bases ((tspace tangent-space))
  (let ((point (tangent-space-chart tspace)))
    (let ((n (- (length ) 1)))
      (let ((coordinates (make-array `(,(length chart)) :initial-contents chart)))
	(progn
	  (defparameter bases (make-array `(,(n+1))))
	  (dotimes (i n)
	    (defparameter exp `(/ partial-diff (* partial-diff ,(aref coordinates i))))
	    (setf (aref bases i) exp))
	  bases)))))



