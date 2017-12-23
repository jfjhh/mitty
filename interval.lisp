;;;;
;;;; Real Intervals
;;;; Alex Striff
;;;;

(in-package #:mitty)

(declaim (optimize (speed 1) (safety 1) (debug 3) (compilation-speed 0)))

(deftype rnum () 'real)

(defclass interval ()
  ((a :initarg :a
      :accessor a
      :type rnum
      :documentation "The start of the interval.")
   (b :initarg :b
      :accessor b
      :type rnum
      :documentation "The end of the interval.")
   (ac :initarg :ac
       :accessor ac
       :type boolean
       :documentation "t if the interval is closed at a, nil if open.")
   (bc :initarg :bc
       :accessor bc
       :type boolean
       :documentation "t if the interval is closed at b, nil if open.")))

(defvar +empty-interval+ (make-instance 'interval))

(defmethod print-object ((object interval) stream)
  "Prints the interval object to stream, in a human-readable way."
  (print-unreadable-object (object stream :type t)
    (format stream "~:[(~;[~]~a, ~a~:[)~;]~]"
	    (ac object) (a object) (b object) (bc object))))

(defmethod print-object ((object (eql +empty-interval+)) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "Empty")))

(defun interval (a b &optional (ac t) (bc t))
  "Creates an interval from a to b, given a <= b."
  (cond ((> a b)
	 (error "Interval [~a, ~a] is not min-max." a b))
	((and (= a b) (or (not ac) (not bc)))
	 +empty-interval+)
	(t
	 (make-instance 'interval :a a :b b :ac ac :bc bc))))

(defmethod interval= ((x interval) (y interval))
  "Returns first value t if intervals are equal (mathematically),
   otherwise nil. Returns second value t if endpoints of interval
   are equal, otherwise nil."
  (let ((p (%emptyp% x))
	(q (%emptyp% y)))
    (cond ((and p q) (values t t))
	  ((not (or p q))
	   (let ((endpoints-equal (and (= (a x) (a y))
				       (= (b x) (b y)))))
	     (values
	      (and (not (xor (ac x) (ac y)))
		   (not (xor (bc x) (bc y)))
		   endpoints-equal)
	      endpoints-equal)))
	  (t (values nil nil)))))

(defmethod %emptyp% ((x interval))
  "Returns t if x is the empty interval."
  (eq +empty-interval+ x))

(defvar %ltrels% (cons #'<= #'<))
(defvar %gtrels% (cons #'>= #'>))

(defun %choose-rel% (p q rels inc)
  "Chooses the appropriate ordering relation to compare different
   combinations of open and closed intervals. p and q are t if closed
   and nil if open, and inc t if this is an inclusive relation or nil
   if this is an exclusive relation."
  (if (or inc (and (or p q) (or inc (and p q))))
      (car rels)
      (cdr rels)))

(defmethod %interval-rel% ((x interval) (y interval) &key (lt t) (inc t))
  "Calculates the needed relations for comparing two intervals."
  (let ((rels (if lt %ltrels% %gtrels%)))
    (values (%choose-rel% (ac x) (ac y) rels inc)
	    (%choose-rel% (ac x) (bc y) rels inc)
	    (%choose-rel% (bc x) (ac y) rels inc)
	    (%choose-rel% (bc x) (bc y) rels inc))))

(defmethod unite ((x interval) (y interval))
  "Returns the union of the intervals x and y,
   or +empty-interval+ if the intervals are not coincident."
  (or (when (%emptyp% x) y)
      (when (%emptyp% y) x)
      (let ((xa (a x))
	    (xb (b x))
	    (ya (a y))
	    (yb (b y)))
	(or (when (or (< xb ya)
		      (< yb xa)
		      (and (= xb ya) (not (or (bc x) (ac y))))
		      (and (= yb xa) (not (or (bc y) (ac x)))))
	      +empty-interval+)
	    (interval
	     (min xa ya)
	     (max xb yb)
	     (cond ((= xa ya) (or (ac x) (ac y)))
		   ((< xa ya) (ac x))
		   (t (ac y)))
	     (cond ((= xb yb) (or (bc x) (bc y)))
		   ((< xb yb) (bc y))
		   (t (bc x))))))))

(defmethod intersect ((x interval) (y interval))
  "Returns the intersection of the intervals x and y,
   or +empty-interval+ if the intervals are not coincident."
  (or (when (or (%emptyp% x) (%emptyp% y)) +empty-interval+)
      (let ((xa (a x))
	    (xb (b x))
	    (ya (a y))
	    (yb (b y)))
	(or (when (or (< xb ya)
		      (< yb xa)
		      (and (= xb ya) (not (and (bc x) (ac y))))
		      (and (= yb xa) (not (and (bc y) (ac x)))))
	      +empty-interval+)
	    (interval
	     (max xa ya)
	     (min xb yb)
	     (cond ((= xa ya) (and (ac x) (ac y)))
		   ((< xa ya) (ac y))
		   (t (ac x)))
	     (cond ((= xb yb) (and (bc x) (bc y)))
		   ((< xb yb) (bc x))
		   (t (bc y))))))))

(defmethod abs ((x interval))
  "Returns the length of the interval."
  (if (%emptyp% x)
      0
      (- (b x) (a x))))

(defmethod in-interval ((e real) (x interval))
  "Returns t if e is contained in the interval x, nil otherwise."
  (and (not (%emptyp% x))
       (let ((lrel (if (ac x) (car %ltrels%) (cdr %ltrels%)))
	     (rrel (if (bc x) (car %ltrels%) (cdr %ltrels%))))
	 (and (funcall lrel (a x) e)
	      (funcall rrel e (b x))))))
