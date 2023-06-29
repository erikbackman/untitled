(in-package :untitled)

(defstruct shader-src vs fs)

(defun make-string-stream ()
  (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t))

(defun load-shader (file-path)
  (let ((vs-lines (make-string-stream))
	(fs-lines (make-string-stream))
	(type :none))
    (with-open-file (lines file-path)
      (loop for line = (read-line lines nil)
	    while line
	    if (uiop:string-prefix-p "#shader" line) do
	      (switch ((subseq line 8) :test #'equal)
		("vertex" (setf type :vertex))
		("fragment" (setf type :fragment)))
	    else do
	      (switch (type :test #'equal)
		(:vertex (with-output-to-string (s vs-lines)
			   (format s "~a~%" line)))
		(:fragment (with-output-to-string (s fs-lines)
			     (format s "~a~%" line))))
	    finally (return (make-shader-src :vs vs-lines :fs fs-lines)))
      )))
