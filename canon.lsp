(in-package :sc)

;; we will use layers
(unless (fboundp 'sc::layers-has-been-loaded)
  (load "/E/code/layers/src/all.lsp"))

(in-package :ly)

(defun scale-for-piano (midi)
  (cond ((< midi 12) (scale-for-piano (+ midi 12)))
	((> midi 120) (scale-for-piano (- midi 12)))
	(t midi)))

(defmacro midi-helper (i note up?)
  `(loop repeat i collect
       (midi-to-note (scale-for-piano (incf note
			   (funcall (if (= (mod i 2) 0) #'+ #'-)
				    (if up? ,up ,right)))))))

(defun spirals (start dur up right howmany &key (name "test.mid") (dir "/E/"))
  (declare (special up)
	   (special right))
  (let* ((note start)
	 (pitch (append (list (midi-to-note note))
			(loop for i from 1 to howmany append
			     (append (midi-helper i note t)
				     (midi-helper i note nil)))))
	 (durations (loop for i in pitch collect dur))
	 (starts (loop for i from 0 and j in pitch collect (* i dur))))
    (print pitch)
    (ly::lists-to-midi pitch durations starts :name name :dir dir)))

(spirals 60 0.5 7 4 10 :name first.mid)
(spirals 60 0.5 4 -7 10 :name second.mid)
