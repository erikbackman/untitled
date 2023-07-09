(in-package :untitled)

(defstruct (shape-data (:conc-name sd-)) verts inds vert-count)

;; (concatenate '(vector int) v1 v2)

(defun make-rect ()
  (make-shape-data
   :verts #(+0.5 +0.5 +0.0
	    +1.0 +0.0 +0.0 ;; color
	    +0.5 -0.5 +0.0
	    +1.0 +0.0 +0.0 ;; color
	    -0.5 -0.5 +0.0
	    +1.0 +0.0 +0.0 ;; color
	    -0.5 +0.5 +0.0
	    +1.0 +0.0 +0.0 ;; color
	    )
   :vert-count 4
   :inds #(0 1 2
	   2 3 0)))

#|================================================================================|# 
#|                                      CUBE                                      |# 
#|================================================================================|# 
(defun make-cube ()
  (make-shape-data
   :verts #(;; front           ;; color
	    -0.5 -0.5 +0.5     +0.7 +0.0 +0.0 +1.0
	    +0.5 -0.5 +0.5     +0.7 +0.0 +0.0 +1.0
	    -0.5 +0.5 +0.5     +0.7 +0.0 +0.0 +1.0
	    +0.5 +0.5 +0.5     +0.7 +0.0 +0.0 +1.0
	    ;; back
	    -0.5 -0.5 -0.5     +0.7 +0.0 +0.0 +1.0
	    +0.5 -0.5 -0.5     +0.7 +0.0 +0.0 +1.0
	    -0.5 +0.5 -0.5     +0.7 +0.0 +0.0 +1.0
	    +0.5 +0.5 -0.5     +0.7 +0.0 +0.0 +1.0
	    ;; top
	    -0.5 +0.5 +0.5     +0.0 +0.7 +0.0 +1.0
	    +0.5 +0.5 +0.5     +0.0 +0.7 +0.0 +1.0
	    -0.5 +0.5 -0.5     +0.0 +0.7 +0.0 +1.0
	    +0.5 +0.5 -0.5     +0.0 +0.7 +0.0 +1.0
	    ;; bottom
	    -0.5 -0.5 +0.5     +0.0 +0.7 +0.0 +1.0
	    +0.5 -0.5 +0.5     +0.0 +0.7 +0.0 +1.0
	    -0.5 -0.5 -0.5     +0.0 +0.7 +0.0 +1.0
	    +0.5 -0.5 -0.5     +0.0 +0.7 +0.0 +1.0
	    ;; right
	    +0.5 -0.5 +0.5     +0.0 +0.0 +0.7 +1.0
	    +0.5 -0.5 -0.5     +0.0 +0.0 +0.7 +1.0
	    +0.5 +0.5 +0.5     +0.0 +0.0 +0.7 +1.0
	    +0.5 +0.5 -0.5     +0.0 +0.0 +0.7 +1.0
	    ;; left
	    -0.5 -0.5 +0.5     +0.0 +0.0 +0.7 +1.0
	    -0.5 -0.5 -0.5     +0.0 +0.0 +0.7 +1.0
	    -0.5 +0.5 +0.5     +0.0 +0.0 +0.7 +1.0
	    -0.5 +0.5 -0.5     +0.0 +0.0 +0.7 +1.0
            ;; Rect
	    +0.5 +0.5 +0.0     +0.0 +0.8 +0.8 +0.3
	    +0.5 -0.5 +0.0     +0.0 +0.8 +0.8 +0.3
	    -0.5 -0.5 +0.0     +0.0 +0.8 +0.8 +0.3
	    -0.5 +0.5 +0.0     +0.0 +0.8 +0.8 +0.3
	    ;; Prism
	    -0.5 -0.5 +0.5     +1.0 +1.0 +1.0 +1.0
	    +0.5 -0.5 +0.5     +0.0 +0.0 +1.0 +1.0
	    -0.5 -0.5 -0.5     +0.0 +0.0 +1.0 +1.0
	    +0.5 -0.5 -0.5     +1.0 +1.0 +1.0 +1.0
	    +0.0 +1.0 +0.0     +1.0 +1.0 +1.0 +1.0
	    +0.0 -1.5 +0.0     +1.0 +1.0 +1.0 +1.0
	    )
   :vert-count 23
   :inds #(;; front
	   0 1 2     2 3 1
	   ;; back
	   4 5 6     6 7 5
	   ;; top
	   8 9 10    10 9 11
	   ;; bottom
	   12 13 14  14 13 15
	   ;; right
	   16 17 18  18 17 19
	   ;; left
	   20 21 22  22 21 23
	   )))

(defparameter *prism-ix*
  #(28 29 32	; side1
    30 31 32	; side2
    28 30 32	; side3
    29 31 32	; side4
    
    28 29 33	; bot side1
    30 31 33	; bot side2
    28 30 33	; bot side3
    29 31 33	; bot side4
    ))

(defparameter *rect-ix* #(24 25 26 26 27 24))

#|================================================================================|# 
#|                                    CYLINDER                                    |#
#|================================================================================|# 

(defun make-unit-circle-verticies (sector-count)
  (let ((sector-step (/ +2pi+ sector-count))
	(circle-verticies
	  (make-array 3 :initial-element 0.0 :fill-pointer 0 :adjustable t)))
    (loop for i from 0 below sector-count
	  for a = (* i sector-step)
	  do (vector-push-extend (cos a) circle-verticies)
	     (vector-push-extend (sin a) circle-verticies)
	     (vector-push-extend 0.0 circle-verticies))
    circle-verticies))


;; (defun make-cylinder-verticies ()
;;   (let ((sector-count 3)
;; 	(unit-verticies (make-unit-circle-verticies sector-count))
;; 	(height 4)
;; 	(radius 4)
;; 	(verticies (make-array 3 :initial-element 0.0 :fill-pointer 0 :adjustable t)))
;;     (loop for i from 0 below 2
;; 	  for h = (+ (/ (-height) 2) (* i height))
;; 	  for t = (- 1.0 i) do
;; 	    (loop for j from 0 to sector-count
;; 		  for k from 0 by 3
;; 		  for ux = (aref unit-verticies k)
;; 		  for uy = (aref unit-verticies (+ k 1))
;; 		  for uz = (aref unit-verticies (+ k 2))

;; 		  (vector-push-extend (* ux radius) verticies)
;; 		  (vector-push-extend (* uy radius) verticies)
;; 		  (vector-push-extend h verticies)

;; 		  (vector-push-extend ux verticies)
;; 		  (vector-push-extend uy verticies)
;; 		  (vector-push-extend uz verticies)
;; 		  ) 
	  
;; 	  )))

