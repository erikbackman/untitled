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
   :verts #(;; front
	    -0.5 -0.5 +0.5 ;; 0 (bottom left)
	    +0.7 +0.0 +0.0 ;; color
	    +0.5 -0.5 +0.5 ;; 1 (bottom right)
	    +0.7 +0.0 +0.0 ;; color
	    -0.5 +0.5 +0.5 ;; 2 (top left)
	    +0.7 +0.0 +0.0 ;; color
	    +0.5 +0.5 +0.5 ;; 3 (top right)
	    +0.7 +0.0 +0.0 ;; color
	    
	    ;; back
	    -0.5 -0.5 -0.5 ;; 4 (bottom left)
	    +0.7 +0.0 +0.0 ;; color
	    +0.5 -0.5 -0.5 ;; 5 (bottom right)
	    +0.7 +0.0 +0.0 ;; color
	    -0.5 +0.5 -0.5 ;; 6 (top left)
	    +0.7 +0.0 +0.0 ;; color
	    +0.5 +0.5 -0.5 ;; 7 (top right)
	    +0.7 +0.0 +0.0 ;; color
	    
	    ;; top
	    -0.5 +0.5 +0.5 ;; 8 (top left near)
	    +0.0 +0.7 +0.0 ;; color
	    +0.5 +0.5 +0.5 ;; 9 (top right near)
	    +0.0 +0.7 +0.0 ;; color
	    -0.5 +0.5 -0.5 ;; 10 (top left far)
	    +0.0 +0.7 +0.0 ;; color
	    +0.5 +0.5 -0.5 ;; 11 (top right far)
	    +0.0 +0.7 +0.0 ;; color
	    
	    ;; bottom
	    -0.5 -0.5 +0.5 ;; 12 (bottom left near)
	    +0.0 +0.7 +0.0 ;; color
	    +0.5 -0.5 +0.5 ;; 13 (bottom right near)
	    +0.0 +0.7 +0.0 ;; color
	    -0.5 -0.5 -0.5 ;; 14 (bottom left far)
	    +0.0 +0.7 +0.0 ;; color
	    +0.5 -0.5 -0.5 ;; 15 (bottom right far)
	    +0.0 +0.7 +0.0 ;; color
	    
	    ;; right
	    +0.5 -0.5 +0.5 ;; 16 (right bottom near)
	    +0.0 +0.0 +0.7 ;; color
	    +0.5 -0.5 -0.5 ;; 17 (right bottom far)
	    +0.0 +0.0 +0.7 ;; color
	    +0.5 +0.5 +0.5 ;; 18 (right top near)
	    +0.0 +0.0 +0.7 ;; color
	    +0.5 +0.5 -0.5 ;; 19 (right top far)
	    +0.0 +0.0 +0.7 ;; color
	    
	    ;; left
	    -0.5 -0.5 +0.5 ;; 20 (left bottom near)
	    +0.0 +0.0 +0.7 ;; color
	    -0.5 -0.5 -0.5 ;; 21 (left bottom far)
	    +0.0 +0.0 +0.7 ;; color
	    -0.5 +0.5 +0.5 ;; 22 (left top near)
	    +0.0 +0.0 +0.7 ;; color
	    -0.5 +0.5 -0.5 ;; 23 (left top far)
	    +0.0 +0.0 +0.7 ;; color

	    ;;; Rect
	    +0.5 +0.5 +0.0 ;; 24
	    +1.0 +0.0 +0.0 ;; color
	    +0.5 -0.5 +0.0 ;; 25
	    +1.0 +0.0 +0.0 ;; color
	    -0.5 -0.5 +0.0 ;; 26
	    +1.0 +0.0 +0.0 ;; color
	    -0.5 +0.5 +0.0 ;; 27
	    +1.0 +0.0 +0.0 ;; color

	    ;;; Prism
	    -0.5 -0.5 +0.5 ;; 28 (left near)
	    +1.0 +1.0 +1.0 ;; color
	    +0.5 -0.5 +0.5 ;; 29 (right near)
	    +0.0 +0.0 +1.0 ;; color
	    -0.5 -0.5 -0.5 ;; 30 (left far)
	    +0.0 +0.0 +1.0 ;; color
	    +0.5 -0.5 -0.5 ;; 31 (right far)
	    +1.0 +1.0 +1.0 ;; color
	    +0.0 +1.0 +0.0 ;; 32 top
	    +1.0 +1.0 +1.0 ;; color
	    +0.0 -1.5 +0.0 ;; 33 bot
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

