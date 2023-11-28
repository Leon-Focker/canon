;; * VIOLA.lsp

(in-package :sc)

(in-scale :quarter-tone)

;; ** classes and parsing

(defclass note ()
  ((id :accessor id :initarg :id :initform nil)
   (root :accessor root :initarg :root :initform nil)
   (root-f :accessor root-f :initarg :root-f :initform nil)
   (times-used :accessor times-used :type integer :initform 0)))

(defclass viola-harmonic (note)
  ((saite :type integer :accessor saite :initarg :saite :initform 4)
   (sounding :accessor sounding :initarg :sounding :initform nil)
   (sounding-f :accessor sounding-f :initarg :sounding-f :initform nil)
   (interval :accessor interval :initarg :interval :type integer
	     :initform 0)))

(defclass cantus-firmus (note)
  ;; the viola-harmonic/note:
  ((note :accessor note :initarg :note :initform nil) 
   ;; pitch only (to form melody of voice1 = cantus firmus):
   (this :accessor this :initarg :this :initform nil)
   (options :accessor options :initarg :options :initform '() :type list)))

;; *** printing 
(defmethod print-object :after ((nt note) stream)
  (format stream "~&~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~%"))

(defmethod print-object ((vh viola-harmonic) stream)
  (format stream "<VIOLA-HARMONIC ~a>" (id vh)))

;; *** matchp
(defmethod matchp ((vh1 viola-harmonic) (vh2 viola-harmonic)
		   &optional (scale 'chromatic-scale))
  (let* ((r1 (root-f vh1))
	 (r2 (root-f vh2))
	 (s1 (sounding-f vh1))
	 (s2 (sounding-f vh2)))
    (flet ((samenotep (a b)
	     (equal (freq-to-note a scale)
		    (freq-to-note b scale))))
      (or (samenotep r1 r2)
	  (samenotep s1 s2)))))

(defmethod match-in (pitch (vh viola-harmonic)
		     &optional (scale 'chromatic-scale))
  (let* ((r (root-f vh))
	 (s (sounding-f vh))
	 (i (note-to-freq (interval vh)))
	 (p (note-to-freq pitch)))
    (flet ((samenotep (a b)
	     (equal (freq-to-note a scale)
		    (freq-to-note b scale))))
      (or (samenotep p r)
	  (samenotep p s)
	  (samenotep p i)))))

;; *** parse-to-event

(defmethod parse-to-event ((vh viola-harmonic) &optional (start-time 1))
  (make-event
   (make-chord `(,(root vh)
		 ,(interval vh)
		 ,(sounding vh)))
   'q
   :start-time start-time))

;; *** MAKE
(defun get-interval-from-harmonic (root sounding)
  (let* ((diff (- (note-to-midi sounding) (note-to-midi root))))
    (midi-to-note
     (+ (cond ((= diff 0) 0)
	      ((= diff 12) 12)
	      ((= diff 19) 7)
	      ((= diff 24) 5)
	      ((= diff 28) 4)
	      ((= diff 31) 3)
	      (t (progn (warn "weird interval: ~a ~a, diff: ~a" root sounding diff)
			0)))
	(note-to-midi root)))))

(defun make-viola-harmonic (id root sounding saite &optional interval)
  (make-instance 'viola-harmonic
		 :id (or id (format nil "~a-~a-~a" saite root sounding))
		 :root root
		 :sounding sounding
		 :saite saite
		 :root-f (note-to-freq root)
		 :sounding-f (note-to-freq sounding)
		 :interval (or interval (get-interval-from-harmonic root sounding))))

(defun make-cf (note this &optional options)
  (make-instance 'cantus-firmus
		 :note note
		 :this this
		 :options options))

;; *** find-with-id
(defun find-with-id (id ls)
  (find (string id) ls :test #'(lambda (x y) (equal x (id y)))))

;; *** viola

;;; all possible artificial viola harmonics:
(defun get-all-artificial-harmonics ()
  (loop for froms in '((f3 c4 g4 d5)(cs3 gs3 ds4 as4)(cs3 gs3 ds4 as4)(cs3 gs3 ds4 as4))
	for tos in '((b4 fs4 cs5 gs5)(c4 g4 d5 a5)(c4 g4 d5 a5)(c4 g4 d5 a5))
	for interval in '(19 24 28 31)
	append
	(loop for saite in '(4 3 2 1)
	      and from in froms
	      and to in tos
	      append (loop for note from (note-to-midi from) to (note-to-midi to)
			   collect (make-viola-harmonic (format nil "~a_~a_~a"
								saite
								(midi-to-note note)
								(midi-to-note (+ interval note)))
							(midi-to-note note)
							(midi-to-note (+ interval note))
							saite)))))
;; what about the different positions to play one harmonic?
(defun get-all-natural-harmonics ()
  (loop for root in '(c3 g3 d4 a4)
	for saite in '(4 3 2 1)
	append (loop for interval in '(12 19 24 28 31 34 36)
		     for played-interval in '((12)(7 19)(5 24)(4 9 16 28)(3 31)(3 6 10 15 22 34)(2 8 17 36))
		     append (loop for played in played-interval
				  collect (make-viola-harmonic (format nil "~a_~a_~a"
								       saite
								       interval
								       (midi-to-note (+ (note-to-midi root)
								   interval)))
						  root
						  (midi-to-note (+ (note-to-midi root)
								   interval))
						  saite
						  (midi-to-note (+ (note-to-midi root)
								   played)))))))

(defun get-all-normal-notes ()
  (loop for string in '(c3 g3 d4 a4)
	for saite in '(4 3 2 1)
	append (loop for note from (note-to-midi string) repeat 25
		     collect (make-viola-harmonic (format nil "~a_~a" saite(midi-to-note note))
						  (midi-to-note note)
						  (midi-to-note note)
						  saite))))

(defparameter *all-viola-harmonics* (get-all-artificial-harmonics))
(defparameter *all-viola-notes* (append (get-all-artificial-harmonics)
					(get-all-natural-harmonics)
					(get-all-normal-notes)))

;; *** find-similar
(defun find-similar (root sounding interval
		     &optional (scale 'chromatic-scale))
  (flet ((samenotep (a b)
	   (equal (freq-to-note a scale)
		  (freq-to-note b scale))))
    (loop for vn in *all-viola-notes*
	  when (and (samenotep (note-to-freq root) (root-f vn))
		    (samenotep (note-to-freq sounding) (sounding-f vn))
		    (samenotep (note-to-freq interval)
			       (note-to-freq (interval vn))))
	    collect vn)))

;; *** notate
(defun notate (lst-of-harmonics file &optional (instrument 'viola))
  (unless (listp lst-of-harmonics) (error "not a list: ~a" lst-of-harmonics))
  (unless (listp (car lst-of-harmonics))
    (setf lst-of-harmonics (list lst-of-harmonics)))
  (let* ((events (loop for i in (flatten lst-of-harmonics)
			      collect (parse-to-event i)))
	 (letters (loop for i in lst-of-harmonics
			with n = 2
			collect n
			do (setf n (+ n (length i)))))
	 (len (length events))
	 (sc (make-slippery-chicken
              '+harmonics-to-notation+
              :ensemble `(((ins (,instrument :midi-channel 1))))
              :set-palette '((1 ((c4))))
              :set-map (loop for i from 1 to len collect `(,i (1)))
              :rthm-seq-palette '((1 ((((1 4) q)))))
	      :rehearsal-letters letters
              :rthm-seq-map (loop for i from 1 to len collect `(,i ((ins (1))))))))
    (map-over-events sc 0 nil 'ins
		     #'(lambda (e) (setf (pitch-or-chord e)
				    (pitch-or-chord (pop events)))))
    (write-xml sc :file file)))

;; ** compose
  
;; Einsatzabstand should be #'1- for voice 2 and 3 ...
(defun compose (first-pitch length &optional (einsatzabstand 1) (n 0))
  (let* ((all-notes (append (get-all-artificial-harmonics)
			    (get-all-natural-harmonics)
			    (get-all-normal-notes)))
	 (first-note (find-similar first-pitch first-pitch first-pitch))
	 (cnt n))
    (unless first-note
      (error "coulnd't find similar note in *all-viola-notes*: ~a" first-pitch))
    (labels ((compose-aux2 (note v2 v3 saite)
	       ;; SORT THE OPTIONS FOR BEST RESULTS:
	       ;; repeated melody notes go back
	       (lambda (x y)
		 (let* ((pitches1 `(,(root x) ,(sounding x) ,(interval x)))
			(pitches2 `(,(root y) ,(sounding y) ,(interval y))))
		   (setf pitches1
			 (remove v2 (remove v3 pitches1 :count 1) :count 1)
			 pitches2
			 (remove v2 (remove v3 pitches2 :count 1) :count 1))
		   (or (equal note (car pitches2))
		       ;; try and use different harmonics:
		       (and (not (equal note (car pitches1)))
			    (< (times-used x) (times-used y)))
		       (and (not (equal note (car pitches1)))
			    (= (times-used x) (times-used y))
			    (= saite (saite x)))))))
	     ;; COMPOSE THE CANON
	     (compose-aux (ls-of-cf &optional rmv)
	       (let* ((this (first ls-of-cf))
		      (len (length ls-of-cf))
		      (voice2 (midi-to-note
			       (+ (note-to-midi
				   (this (nth (min (1- len) einsatzabstand)
					      ls-of-cf)))
				  7)))
		      (voice3 (midi-to-note
			       (+ (note-to-midi
				   (this (nth (min (1- len) (* einsatzabstand 2))
					      ls-of-cf)))
				  12))))
		 ;; look for option that matches 
		 (unless (options this)
		   (setf (options this)
			 (loop for note in all-notes
			       when (and (match-in voice2 note)
					 (match-in voice3 note))
				 collect note))
		   #+nil(let* ((without (remove (note this) (options this))))
			  (when without (setf (options this) without))))
		 ;; sort options for best results:
		 (unless rmv (setf (options this)
		       (sort (options this)
			     (compose-aux2 (this this) voice2 voice3
					   (saite (note this))))))
		 ;; if it didn't lead anywhere before, remove option:
		 (when rmv (setf (options this)
				 (remove (note rmv) (options this))))
		 ;; debug:
		 ;;(when rmv (format t "~&len: ~a, len-opt: ~a" len (length (options this))))
		 ;; see if we found any options or wheter we are done:
		 (cond ((>= (length ls-of-cf) length) ls-of-cf) ; done
		       ;; if an option is found:
		       ((options this)
			(let* ((next (nth (mod cnt (length (options this)))
					  (options this)))
			       (pitches `(,(root next)
					  ,(sounding next)
					  ,(interval next))))
			  ;(incf cnt)
			  (remove voice2 pitches :count 1)
			  (remove voice3 pitches :count 1)
			  (incf (times-used next))
			  (compose-aux (push (make-cf next (first pitches))
					     ls-of-cf))))
		       ;; if no option is found:
		       (t (let* ((old (pop ls-of-cf)))
			    (incf (times-used (note old)) -1)
			    (when ls-of-cf
			      (compose-aux ls-of-cf old))))))))
      (reverse
       (compose-aux `(,(make-cf (first first-note)
				first-pitch)))))))

(defun decode-canon (ls-of-cf)
  (let* ((cantus-firmus (loop for i in ls-of-cf collect (this i))))
    cantus-firmus))

;; *** the canon

#|
(defparameter *canon* (compose 'a3 40 2))

(notate (loop for i in *canon* collect (note i)) "/E/code/canon/test.xml")

(decode-canon *canon*)
|#

;; EOF VIOLA.lsp

