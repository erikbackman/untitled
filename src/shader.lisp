(in-package :untitled)

(defvar *shader-locations* (make-hash-table :test 'equal))

(defstruct shader-src vs fs)

(defun make-string-stream ()
  (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t))

(defun my-compile-shader (type source)
  (let ((shader (gl:create-shader type)))
    (gl:shader-source shader source)
    (gl:compile-shader shader)
    shader))

(defun create-shader (vs-src fs-src)
  (let ((program (gl:create-program))
	(vs (my-compile-shader :vertex-shader vs-src))
	(fs (my-compile-shader :fragment-shader fs-src)))
    (gl:attach-shader program vs)
    (gl:attach-shader program fs)
    (gl:link-program program)
    (gl:validate-program program)
    (gl:delete-shader vs)
    (gl:delete-shader fs)
    program))

(defun parse-shader-file (filepath)
  (let ((content `#(,(make-string-stream) ,(make-string-stream)))
	(type 0))
    (with-open-file (lines filepath)
      (flet ((next () (read-line lines nil)))
	(do ((line (next) (next)))
	    ((null line)
	     (make-shader-src :vs (elt content 0) :fs (elt content 1)))
	  (if (uiop:string-prefix-p "#shader" line)
	      (setf type (if (string= "vertex" (subseq line 8)) 0 1))
	      (with-output-to-string (s (elt content type))
		(format s "~a~%" line))))))))

(defun shader-from-file (filepath)
  (with-slots (vs fs) (parse-shader-file filepath)
    (create-shader vs fs)))

(defun shader-get-uniform (shader name)
  (or (gethash name *shader-locations*)
      (setf (gethash name *shader-locations*)
	    (gl:get-uniform-location shader name))))

(defun shader-set-float (shader name x &optional y z w)
  (gl:uniformf (shader-get-uniform shader name) x y z w))

(defun shader-set-mat4 (shader name matrix)
  (gl:uniform-matrix-4fv (shader-get-uniform shader name) matrix))
