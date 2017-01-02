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
				(if (string= "percent" (car registers))
				    "%"
				    (format nil "~A" (cadr (assoc (car registers) syms :test 'string=)))))
			      :simple-calls t))

(defun read-entire-file (fname)
  (with-open-file (stream fname  :element-type '(unsigned-byte 8))
    (trivial-utf-8:read-utf-8-string stream :stop-at-eof t)))

(defun write-utf-8-file (fname string)
  (ensure-directories-exist fname)
  (with-open-file (out fname
		       :element-type '(unsigned-byte 8)
		       :direction :output
		       :if-exists :supersede)
    (trivial-utf-8:write-utf-8-bytes string out)))

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

(defun beer-story-syms (ch-name en-name en-desc ch-desc price ibu abv num)
  `(("ch-name" ,ch-name)
    ("en-name" ,en-name)
    ("en-desc" ,en-desc)
    ("ch-desc" ,ch-desc)
    ("price" ,price)
    ("ibu" ,ibu)
    ("abv" ,abv)
    ("num" ,num)))

(defun font-size-syms (ch-name en-name en-desc ch-desc price)
  `(("ch-name-font-size" ,ch-name)
    ("en-name-font-size" ,en-name)
    ("en-desc-font-size" ,en-desc)
    ("ch-desc-font-size" ,ch-desc)
    ("price-font-size" ,price)))

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

(defun ch-name-field (pixel-spec) (nth 0 pixel-spec))
(defun en-name-field (pixel-spec) (nth 1 pixel-spec))
(defun desc-field (pixel-spec) (nth 2 pixel-spec))
(defun price-field (pixel-spec) (nth 3 pixel-spec))

(defun text-frame-syms-for-beer (id left row-height offset-xs offset-ys rect-widths rect-heights)
  (let ((y-off (* id row-height)))
    (list (text-frame-syms (format nil "ch_name~A" (pad-num id))
			   (+ left (ch-name-field offset-xs))
			   (+ y-off (ch-name-field offset-ys))
			   (+ left (ch-name-field offset-xs) (ch-name-field rect-widths))
			   (+ y-off (ch-name-field offset-ys) (ch-name-field rect-heights))
			   (+ (ch-name-field offset-xs) (ch-name-field rect-widths)))
	  (text-frame-syms (format nil "en_name~A" (pad-num id))
			   (+ left (en-name-field offset-xs))
			   (+ y-off (en-name-field offset-ys))
			   (+ left (en-name-field offset-xs) (en-name-field rect-widths))
			   (+ y-off (en-name-field offset-ys) (en-name-field rect-heights))
			   (+ (en-name-field offset-xs) (en-name-field rect-widths)))
	  (text-frame-syms (format nil "desc~A" (pad-num id))
			   (+ left (desc-field offset-xs))
			   (+ y-off (desc-field offset-ys))
			   (+ left (desc-field offset-xs) (desc-field rect-widths))
			   (+ y-off (desc-field offset-ys) (desc-field rect-heights))
			   (+ (desc-field offset-xs) (desc-field rect-widths)))
	  (text-frame-syms (format nil "price~A" (pad-num id))
			   (+ left (price-field offset-xs))
			   (+ y-off (price-field offset-ys))
			   (+ left (price-field offset-xs) (price-field rect-widths))
			   (+ y-off (price-field offset-ys) (price-field rect-heights))
			   (+ (price-field offset-xs) (price-field rect-widths))))))

(defun text-frames-beer (id csv)
  (let ((frame-top (first (group-measures csv)))
	(left (second (group-measures csv)))
	(row-height (third (group-measures csv)))

	(offset-xs (offset-xs csv))
	(offset-ys (offset-ys csv))
	(rect-widths (rect-ws csv))
	(rect-heights (rect-hs csv)))
   (join-newline (mapcar (lambda (syms)
			   (let ((template (read-entire-file "drinks-template/text-frame.xml")))
			     (format nil "~A" (subst-template template
							      (cons (list "frame-top" frame-top) syms)))))
			 (text-frame-syms-for-beer id left row-height offset-xs offset-ys rect-widths rect-heights)))))

(defun text-frames (num-beers csv)
  (join-newline (loop for i below num-beers collecting (text-frames-beer i csv))))

(defun spread-syms (text-frames)
  `(("text-frames" ,text-frames)))

(defun group-measures (csv)
  (mapcar 'read-from-string (subseq (nth 1 csv) 0 3)))
(defun font-sizes (csv)
  (mapcar 'read-from-string (subseq (nth 3 csv) 1 6)))

(defun offset-xs (csv)
  (mapcar 'read-from-string (subseq (nth 5 csv) 1 5)))
(defun offset-ys (csv)
  (mapcar 'read-from-string (subseq (nth 6 csv) 1 5)))
(defun rect-ws (csv)
  (mapcar 'read-from-string (subseq (nth 7 csv) 1 5)))
(defun rect-hs (csv)
  (mapcar 'read-from-string (subseq (nth 8 csv) 1 5)))

(defun beers (csv)
  (nthcdr 10 csv))

(defun read-beers-tsv ()
  (with-input-from-string (str (read-entire-file "beers.txt"))
		      (cl-csv:read-csv str 
				       :separator #\Tab
				       :quote #\"
				       :escape #\\)))

(defun beer-visible? (beer-row)
  (string= "TRUE" (nth 5 beer-row)))
(defun visible-beers (csv)
  (remove-if-not 'beer-visible? (beers csv)))

(defun story-syms (beer-story-syms font-size-syms idx)
  (append beer-story-syms font-size-syms (story-id-syms idx)))

(defun output-path (path) (concatenate 'string "output/" path))

(defun story-template-files ()
  (cl-fad:list-directory "drinks-template/Stories"))
(defun story-output-files (idx story-template-files)
  (mapcar (lambda (fname)
	    (output-path (story-path fname idx)))
	  story-template-files))

(defun beer-row-story-syms (beer-row idx)
  (beer-story-syms (nth 0 beer-row)
		   (nth 1 beer-row)
		   (nth 2 beer-row)
		   (nth 3 beer-row)
		   (nth 4 beer-row)
		   (nth 7 beer-row)
		   (nth 6 beer-row)
		   (1+ idx)))

(defun num-visible-beers (csv)
  (length (visible-beers csv)))

(defun spread-output (csv)
  (let ((spread-syms (spread-syms (text-frames (num-visible-beers csv) csv)))
	(spread-template (read-entire-file "drinks-template/Spreads/Spread_ub9.xml")))
    (subst-template spread-template spread-syms)))

(defun design-map-output (csv)
  (let ((design-map-syms (design-map-syms (num-visible-beers csv)))
	(design-map-template (read-entire-file "drinks-template/designmap.xml"))) 
    (subst-template design-map-template design-map-syms)))

(defun idxs (count) (loop for i below count collecting i))

(defun all-story-output-files (beers story-template-files)
  (mapcan (lambda (i)
	    (story-output-files i story-template-files))
	  (idxs (length beers))))

(defun all-story-outputs (story-templates csv)
  (let ((beers (visible-beers csv))
	(font-size-syms (apply 'font-size-syms (font-sizes csv))))
   (mapcan (lambda (beer-row i)
	     (let ((story-syms (story-syms (beer-row-story-syms beer-row i)
					   font-size-syms i)))
	       (let ((story-outputs (mapcar (lambda (template)
					      (subst-template template story-syms))
					    story-templates)))
		 story-outputs)))
	   beers
	   (idxs (length beers)))))

(defun process-menu ()
  (setq *id* 0)

  (let*((csv (read-beers-tsv))
	(story-template-files (story-template-files))
	(story-templates (mapcar 'read-entire-file story-template-files)))

    (mapc 'write-utf-8-file
	  (all-story-output-files (visible-beers csv) story-template-files)
	  (all-story-outputs story-templates csv))

    (write-utf-8-file "output/designmap.xml" (design-map-output csv)) 
    (write-utf-8-file "output/Spreads/Spread_ub9.xml" (spread-output csv))))

(all-story-output-files (visible-beers (read-beers-tsv)) (story-template-files))
(third
 (all-story-outputs (mapcar 'read-entire-file (story-template-files))
		    (read-beers-tsv)))

(process-menu)
(exit)
