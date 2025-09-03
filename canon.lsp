;; * canon.lsp

;;; To generate my canon yourself, you only have to change the global parameter
;;; *target-dir* to a directory on your system. Then load the script and watch
;;; it get to work. To get a different canon, experiment with the #'compose
;;; function. Definitely start with shorter canons though, as the brute force
;;; method I am using can really take a while.

;;; The nomenclature in the following object definitions and functions is not
;;; good. For example, I use root as the midi-value for the root note and root-f
;;; as the same note as a frequency value. A worse offender might be 'interval,
;;; as it does not refer to a musical interval but another note (where the
;;; harmonic is played). Also, some things are referred to by the german word,
;;; because it was easier to avoid duplicates (string?) and at some points I
;;; just couldn't be bothered to translate some of the words...

(in-package :sc)

(defparameter *target-dir* "c:/Users/leonf/Desktop/")
(defparameter *use-quarter-notes* nil)

(in-scale :quarter-tone)

;; ** classes

(defclass note ()
  ((id :accessor id :initarg :id :initform nil)
   (root :accessor root :initarg :root :initform nil)
   (root-f :accessor root-f :initarg :root-f :initform nil)
   (times-used :accessor times-used :type integer :initform 0)))

(defclass violin-harmonic (note)
  ((saite :type integer :accessor saite :initarg :saite :initform 4)
   (sounding :accessor sounding :initarg :sounding :initform nil)
   (sounding-f :accessor sounding-f :initarg :sounding-f :initform nil)
   (interval :accessor interval :initarg :interval :type integer
	     :initform 0)))

(defclass cantus-firmus (note)
  ;; the violin-harmonic/note:
  ((note :accessor note :initarg :note :initform nil) 
   ;; pitch only (to form melody of voice1 = cantus firmus):
   (this :accessor this :initarg :this :initform nil)
   (options :accessor options :initarg :options :initform '() :type list)))

;; ** printing

;; *** printing 
(defmethod print-object :after ((nt note) stream)
  (format stream "~&~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~%"))

(defmethod print-object ((vh violin-harmonic) stream)
  (format stream "<VIOLIN-HARMONIC ~a>" (id vh)))

;; ** finding matches

;; *** matchp
(defmethod matchp ((vh1 violin-harmonic) (vh2 violin-harmonic)
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

(defmethod match-in (pitch (vh violin-harmonic)
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

;; ** parsing to sc-event

;; *** parse-to-event

(defmethod parse-to-event ((vh violin-harmonic) &optional (start-time 1))
  (make-event
   (make-chord `(,(root vh)
		 ,(interval vh)
		 ,(sounding vh)))
   'q
   :start-time start-time))

;; ** utilities and make-functions for classes

;; *** note-to-midi
;;; define this to get quarter notes as .5 midi notes
(defun canon-note-to-midi (note)
  (/ (note-to-degree note 'quarter-tone) 2.0))

;; *** MAKE
(defun get-interval-from-harmonic (root sounding)
  ;; use note-to-degree to allow quarter notes
  (let* ((diff (- (canon-note-to-midi sounding) (canon-note-to-midi root))))
    (midi-to-note
     (+ (cond ((= diff 0) 0)
	      ((= diff 12) 12)
	      ((= diff 19) 7)
	      ((= diff 24) 5)
	      ((= diff 28) 4)
	      ((= diff 31) 3)
	      (t (progn (warn "weird interval: ~a ~a, diff: ~a" root sounding diff)
			0)))
	(canon-note-to-midi root)))))

(defun make-violin-harmonic (id root sounding saite &optional interval)
  (make-instance 'violin-harmonic
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

;; ** all violin harmonics and notes

;; *** get-all-artificial-harmonics
;;; all possible artificial violin harmonics:
;;; Firstly loop through the types of harmonic (third harmonic, fourth, fifth and sixth)
;;; For each type and per string also specify lowest and highest note to
;;; calculate the harmonics for.
;;; (froms is a list of lists (one for each type of harmonic) with 4 notes (one per string)
;;; which will be the starting pitch to collect artificial harmonics from.)
;;; TODO why was the first list of froms and tos different than the others for viola?
;;; just made them the same here...
(defun get-all-artificial-harmonics ()
  (loop for froms = '(gs3 ds4 as4 f5)
	for tos = '(g4 d5 a5 e6)
	for interval in '(19 24 28 31)
	append
	(loop for saite in '(4 3 2 1)
	      and from in froms
	      and to in tos
	      append (loop for note from (note-to-midi from) to (note-to-midi to)
			   by (if *use-quarter-notes* 0.5 1)
			   collect (make-violin-harmonic (format nil "~a_~a_~a"
								 saite
								 (midi-to-note note)
								 (midi-to-note (+ interval note)))
							 (midi-to-note note)
							 (midi-to-note (+ interval note))
							 saite)))))

;;; loop through all strings and get theirs harmonics by sounding-interval and 
;;; position at which they are played.
(defun get-all-natural-harmonics ()
  (loop for root in '(g3 d4 a4 e5)
	for saite in '(4 3 2 1)
	append (loop for interval in '(12 19 24 28 31 34 36)
		     for played-interval in '((12)(7 19)(5 24)(4 9 16 28)(3 31)
					      (3 6 10 15 22 34)(2 8 17 36))
		     append (loop for played in played-interval
				  collect (make-violin-harmonic
					   (format nil "~a_~a_~a"
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

;;; collect 2 octaves of pitches for each string. 
(defun get-all-normal-notes ()
  (loop for string in '(g3 d4 a4 e5)
	for saite in '(4 3 2 1)
	append (loop for note from (note-to-midi string) by (if *use-quarter-notes* 0.5 1) repeat 25
		     collect (make-violin-harmonic (format nil "~a_~a" saite (midi-to-note note))
						   (midi-to-note note)
						   (midi-to-note note)
						   saite))))

(defparameter *all-violin-harmonics* (get-all-artificial-harmonics))
(defparameter *all-violin-notes* (append (get-all-artificial-harmonics)
					 (get-all-natural-harmonics)
					 (get-all-normal-notes)))

;; ** more match-making

;; *** find-similar
;;; find an object that matches all three input pitches in some way
(defun find-similar (root sounding interval
		     &optional (scale 'chromatic-scale))
  (flet ((samenotep (a b)
	   (equal (freq-to-note a scale)
		  (freq-to-note b scale))))
    (loop for vn in *all-violin-notes*
	  when (and (samenotep (note-to-freq root) (root-f vn))
		    (samenotep (note-to-freq sounding) (sounding-f vn))
		    (samenotep (note-to-freq interval)
			       (note-to-freq (interval vn))))
	    collect vn)))

;; ** notation

;; *** notate
;;; write a list of objects into an xml file for musical notation
(defun notate (lst-of-harmonics file &optional (instrument 'violin))
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
;;; - einsatzabstand: how many notes lie between beginning of first and second,
;;;   and second and third voice.
;;; - intervall1:  Interval of the second voice relative to voice 1
;;; - intervall2:  Interval of the third voice relative to voice 1
;;; - n: choose the nth option out of a list of options for next notes.
(defun compose (first-pitch length &optional
				     (einsatzabstand 1)
				     (intervall1 7)
				     (intervall2 12)
				     (n 0))
  (let* ((all-notes (append (get-all-artificial-harmonics)
			    (get-all-natural-harmonics)
			    (get-all-normal-notes)))
	 (first-note (find-similar first-pitch first-pitch first-pitch))
	 (cnt n))
    (unless first-note
      (error "coulnd't find similar note in *all-violin-notes*: ~a" first-pitch))
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
			       (+ (canon-note-to-midi
				   (this (nth (min (1- len) (1- einsatzabstand))
					      ls-of-cf)))
				  intervall1)))
		      (voice3 (midi-to-note
			       (+ (canon-note-to-midi
				   (this (nth (min (1- len) (1- (* einsatzabstand 2)))
					      ls-of-cf)))
				  intervall2))))
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
		       (t
			(let* ((old (pop ls-of-cf)))
			  (incf (times-used (note old)) -1)
			  (if ls-of-cf
			      (compose-aux ls-of-cf old)
			      (error "no canon found!"))))))))
      (reverse
       (compose-aux `(,(make-cf (first first-note)
				first-pitch)))))))

;; *** decode-canon
;;; get the "cantus firmus" (could've just called it first voice) of the
;;; generated canon.
(defun decode-canon (ls-of-cf)
  (let* ((cantus-firmus (loop for i in ls-of-cf collect (this i))))
    cantus-firmus))

;; *** the canon

(defparameter *canon* (compose 'e4 10 2 3.5))

(notate (loop for i in *canon* collect (note i))
	(format nil "~acanon_chords.xml" *target-dir*))

(decode-canon *canon*)

;; EOF canon.lsp

