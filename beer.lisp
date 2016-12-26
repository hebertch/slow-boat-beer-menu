;;;; beer.lisp

(defparameter *row-height* 35)
(defparameter *frame-top* -250.0)
(defparameter *top* 0.0)
(defparameter *left* 23) 

(defvar *beers*)
(defvar *beers-csv*)
(defvar *group-measures*)
(defvar *font-sizes*)
(defvar *offset-xs*)
(defvar *offset-ys*)
(defvar *rect-widths*)
(defvar *rect-heights*)

(ql:quickload :cl-ppcre)
(ql:quickload :cl-ppcre-unicode)
(ql:quickload :cl-fad)
(ql:quickload :cl-csv)
(ql:quickload :trivial-utf-8)

(defun subst-template (template syms)
  (cl-ppcre:regex-replace-all "%([^%^<^>^\"]+)%"
			      template
			      (lambda (match &rest registers)
				(declare (ignore match))
				(format nil "~A" (cadr (assoc (car registers) syms :test 'string=))))
			      :simple-calls t))

(defun read-entire-file (fname)
  (with-open-file (stream fname  :element-type '(unsigned-byte 8))
    (trivial-utf-8:read-utf-8-string stream :stop-at-eof t)))

(defun process-template-file (in-fname out-fname syms)
  (ensure-directories-exist out-fname)
  (with-open-file (out out-fname
		       :element-type '(unsigned-byte 8)
		       :direction :output
		       :if-exists :supersede)
    (let ((template (read-entire-file in-fname)))
      (trivial-utf-8:write-utf-8-bytes (subst-template template syms) out))))

(defun strip-extension (str) (subseq str 0 (- (length str) 4)))
(defun pad-num (num) (format nil "~2,'0D" num))

(defun story-id-syms (num)
  (let ((num-str (pad-num num)))
    `(("ch-name-id" ,(concatenate 'string "ch_name" num-str))
      ("en-name-id" ,(concatenate 'string "en_name" num-str))
      ("desc-id" ,(concatenate 'string "desc" num-str))
      ("price-id" ,(concatenate 'string "price" num-str)))))

(defun story-path (fname num)
  (concatenate 'string "Stories/"
	       (strip-extension (file-namestring fname))
	       (format nil "~2,'0D" num)
	       ".xml"))

(defun story-syms (ch-name en-name en-desc ch-desc price)
  `(("ch-name" ,ch-name)
    ("en-name" ,en-name)
    ("en-desc" ,en-desc)
    ("ch-desc" ,ch-desc)
    ("price" ,price)))

(defun font-size-syms (ch-name en-name en-desc ch-desc price)
  `(("ch-name-font-size" ,ch-name)
    ("en-name-font-size" ,en-name)
    ("en-desc-font-size" ,en-desc)
    ("ch-desc-font-size" ,ch-desc)
    ("price-font-size" ,price)))

(defun process-stories (idx story-syms)
  (let* ((syms (append story-syms (story-id-syms idx))))
    (mapc (lambda (in-fname)
	    (let ((out-fname (concatenate 'string "output/" (story-path in-fname idx))))
	      (process-template-file in-fname out-fname syms)))
	  (cl-fad:list-directory "drinks-template/Stories"))))

(defun join (strs)
  (format nil "~{~A ~}" strs))
(defun join-newline (strs)
  (format nil "~{~A~%~}" strs))

(defun story-list (num-stories)
  (join (loop for i from 0 below num-stories
	   appending (mapcar 'cadr (story-id-syms i)))))

(defun story-elements (num-stories)
  (join-newline (loop for i below num-stories
		   appending
		     (mapcar (lambda (fname)
			       (format nil "<idPkg:Story src=\"~A\" />" (story-path fname i)))
			     (cl-fad:list-directory "drinks-template/Stories")))))

(defun design-map-syms (num-stories)
  `(("story-list" ,(story-list num-stories))
    ("story-elements" ,(story-elements num-stories))))

(defvar *id* 0)
(defun gen-id ()
  (let ((res (format nil "generated_id~A" *id*)))
    (incf *id*)
    res))

(defun text-frame-syms (parent-id left top right bottom text-column-fixed-width)
  `(("child-id" ,(gen-id))
    ("parent-id" ,parent-id)
    ("left" ,left)
    ("top" ,top)
    ("right" ,right)
    ("bottom" ,bottom)
    ("fixed-width" ,text-column-fixed-width)))

(defun generate-text-frame-for-story (text-frame-syms)
  (let ((template (read-entire-file "drinks-template/text-frame.xml")))
    (format nil "~A" (subst-template template text-frame-syms))))

(defun text-frame-syms-for-beer (id)
  (let ((y-off (* id *row-height*)))
    (list (text-frame-syms (format nil "ch_name~A" (pad-num id))
			   (+ *left* (cdr (assoc :ch-name *offset-xs*)))
			   (+ y-off (cdr (assoc :ch-name *offset-ys*)))
			   (+ *left* (cdr (assoc :ch-name *offset-xs*)) (cdr (assoc :ch-name *rect-widths*)))
			   (+ y-off (cdr (assoc :ch-name *offset-ys*)) (cdr (assoc :ch-name *rect-heights*)))
			   (+ (cdr (assoc :ch-name *offset-xs*)) (cdr (assoc :ch-name *rect-widths*))))
	  (text-frame-syms (format nil "en_name~A" (pad-num id))
			   (+ *left* (cdr (assoc :en-name *offset-xs*)))
			   (+ y-off (cdr (assoc :en-name *offset-ys*)))
			   (+ *left* (cdr (assoc :en-name *offset-xs*)) (cdr (assoc :en-name *rect-widths*)))
			   (+ y-off (cdr (assoc :en-name *offset-ys*)) (cdr (assoc :en-name *rect-heights*)))
			   (+ (cdr (assoc :en-name *offset-xs*)) (cdr (assoc :en-name *rect-widths*))))
	  (text-frame-syms (format nil "desc~A" (pad-num id))
			   (+ *left* (cdr (assoc :desc *offset-xs*)))
			   (+ y-off (cdr (assoc :desc *offset-ys*)))
			   (+ *left* (cdr (assoc :desc *offset-xs*)) (cdr (assoc :desc *rect-widths*)))
			   (+ y-off (cdr (assoc :desc *offset-ys*)) (cdr (assoc :desc *rect-heights*)))
			   (+ (cdr (assoc :desc *offset-xs*)) (cdr (assoc :desc *rect-widths*))))
	  (text-frame-syms (format nil "price~A" (pad-num id))
			   (+ *left* (cdr (assoc :price *offset-xs*)))
			   (+ y-off (cdr (assoc :price *offset-ys*)))
			   (+ *left* (cdr (assoc :price *offset-xs*)) (cdr (assoc :price *rect-widths*)))
			   (+ y-off (cdr (assoc :price *offset-ys*)) (cdr (assoc :price *rect-heights*)))
			   (+ (cdr (assoc :price *offset-xs*)) (cdr (assoc :price *rect-widths*)))))))

(defun generate-text-frames-for-beer (id)
  (join-newline (mapcar (lambda (syms)
			  (let ((template (read-entire-file "drinks-template/text-frame.xml")))
			    (format nil "~A" (subst-template template
							     (cons (list "frame-top" *frame-top*) syms)))))
			(text-frame-syms-for-beer id))))

(defun process-spread-file (num-beers)
  (let ((syms `(("text-frames" ,(join-newline (loop for i below num-beers
						 collecting (generate-text-frames-for-beer i)))))))
    (process-template-file "drinks-template/Spreads/Spread_ub9.xml" "output/Spreads/Spread_ub9.xml" syms)))

(defun process-menu ()
  (setq *id* 0)

  (setq *beers-csv* (with-input-from-string (str (read-entire-file "beers.txt"))
		      (cl-csv:read-csv str 
				       :separator #\Tab
				       :quote #\"
				       :escape #\\)))

  (setq *group-measures* (pairlis
			  '(:top :left :row-height)
			  (mapcar 'read-from-string (subseq (first (cdr *beers-csv*)) 0 3))))
  (setq *frame-top* (cdr (assoc :top *group-measures*))
	*left* (cdr (assoc :left *group-measures*))
	*row-height* (cdr (assoc :row-height *group-measures*)))

  (setq *font-sizes* (pairlis
		      '(:ch-name :en-name :en-desc :ch-desc :price)
		      (mapcar 'read-from-string (cdr (cadddr *beers-csv*)))))

  (setq *offset-xs* (pairlis
		     '(:ch-name :en-name :desc :price)
		     (mapcar 'read-from-string (subseq (nth 5 *beers-csv*) 1 5))))
  (setq *offset-ys* (pairlis
		     '(:ch-name :en-name :desc :price)
		     (mapcar 'read-from-string (subseq (nth 6 *beers-csv*) 1 5))))
  (setq *rect-widths* (pairlis
		       '(:ch-name :en-name :desc :price)
		       (mapcar 'read-from-string (subseq (nth 7 *beers-csv*) 1 5))))
  (setq *rect-heights* (pairlis
			'(:ch-name :en-name :desc :price)
			(mapcar 'read-from-string (subseq (nth 8 *beers-csv*) 1 5))))

  (setq *beers* (nthcdr 10 *beers-csv*))

  (let ((beers (mapcar 'butlast
		       (remove-if-not (lambda (beer)
					(string= "TRUE" (car (last beer))))
				      *beers*))))
    (loop for i from 0
       for beer in beers
       do (process-stories i (append (apply 'story-syms beer)
				     (font-size-syms (cdr (assoc :ch-name *font-sizes*))
						     (cdr (assoc :en-name *font-sizes*))
						     (cdr (assoc :en-desc *font-sizes*))
						     (cdr (assoc :ch-desc *font-sizes*))
						     (cdr (assoc :price *font-sizes*))))))
    (process-template-file "drinks-template/designmap.xml" "output/designmap.xml" (design-map-syms (length beers)))
    (process-spread-file (length beers))))

(process-menu)
(exit)
