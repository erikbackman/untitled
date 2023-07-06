(in-package :untitled)

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

(defun load-shader (file-path)
  (let ((content `#(,(make-string-stream)
		    ,(make-string-stream)))
	(type))
    (with-open-file (lines file-path)
      (loop for line = (read-line lines nil)
	    while line
	    if (uiop:string-prefix-p "#shader" line) do
	      (switch ((subseq line 8) :test #'equal)
		("vertex" (setf type 0))
		("fragment" (setf type 1)))
	    else do
	      (with-output-to-string (s (elt content type))
		(format s "~a~%" line))
	    finally (return (make-shader-src :vs (elt content 0) :fs (elt content 1)))))))

(defun shader-set-float (shader name x &optional y z w)
  (gl:uniformf (funcall 'gl:get-uniform-location shader name) x y z w))

(defun shader-set-mat4 (shader name matrix)
  (gl:uniform-matrix-4fv (gl:get-uniform-location shader name) matrix))
