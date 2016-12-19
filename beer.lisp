;;;; beer.lisp

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

(defparameter *row-height* 50)
(defparameter *top* 23)
(defparameter *left* 157)

(defun text-frame-syms-for-beer (id)
  (let ((y-off (* id *row-height*)))
    (list (text-frame-syms (format nil "ch_name~A" (pad-num id)) (+ *left* 0) (+ *top* y-off 10.5) (+ *left* 161.5) (+ *top* y-off 22.5) 161.6)
	  (text-frame-syms (format nil "en_name~A" (pad-num id)) (+ *left* 0) (+ *top* y-off .6) (+ *left* 210.7) (+ *top* y-off 22.3) 210.4)
	  (text-frame-syms (format nil "desc~A" (pad-num id)) (+ *left* 0) (+ *top* y-off 18) (+ *left* 254) (+ *top* y-off 42) 254)
	  (text-frame-syms (format nil "price~A" (pad-num id)) (+ *left* 220.6) (+ *top* y-off 0) (+ *left* 241.9) (+ *top* y-off 10.5) 21.3))))

(defun generate-text-frames-for-beer (id)
  (join-newline (mapcar (lambda (syms)
			  (let ((template (read-entire-file "drinks-template/text-frame.xml")))
			    (format nil "~A" (subst-template template syms))))
			(text-frame-syms-for-beer id))))

(defun process-spread-file (num-beers)
  (let ((syms `(("text-frames" ,(join-newline (loop for i below num-beers
						 collecting (generate-text-frames-for-beer i)))))))
    (process-template-file "drinks-template/Spreads/Spread_ub9.xml" "output/Spreads/Spread_ub9.xml" syms)))


(defvar *beers*)

(defun process-menu ()
  (setq *id* 0)
  (setq *beers* (with-open-file (in "beers.csv") (cl-csv:read-csv in)))

  (loop for i from 0
     for beer in *beers*
     do (process-stories i (apply 'story-syms beer)))
  (process-template-file "drinks-template/designmap.xml" "output/designmap.xml" (design-map-syms (length *beers*)))
  (process-spread-file (length *beers*))

  (uiop:chdir "output")
  (uiop:delete-file-if-exists "generated_menu.idml")

  #+windows
  (uiop:run-program "7z a -tzip generated_menu.idml output/* -mx0 -r")
  #-windows
  (uiop:run-program "zip -X0 -r -J generated_menu.idml *")

  (uiop:chdir ".."))

(process-menu)
