(in-package :untitled)

(defstruct (shape-data (:conc-name sd-)) verts inds vert-count)

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

(defun make-cube ()
  (make-shape-data
   :verts #(;; front
	    -0.5 -0.5 +0.5 ;; 0 (bottom left)
	    ;; +1.0 +1.0 +1.0 ;; color
	    +0.5 -0.5 +0.5 ;; 1 (bottom right)
	    ;; +1.0 +1.0 +1.0 ;; color
	    -0.5 +0.5 +0.5 ;; 2 (top left)
	    ;; +1.0 +1.0 +1.0 ;; color
	    +0.5 +0.5 +0.5 ;; 3 (top right)

	    ;; back
	    -0.5 -0.5 -0.5 ;; 4 (bottom left)
	    ;; +1.0 +1.0 +1.0 ;; color
	    +0.5 -0.5 -0.5 ;; 5 (bottom right)
	    ;; +1.0 +1.0 +1.0 ;; color
	    -0.5 +0.5 -0.5 ;; 6 (top left)
	    ;; +1.0 +1.0 +1.0 ;; color
	    +0.5 +0.5 -0.5 ;; 7 (top right)

	    ;; top
	    -0.5 +0.5 +0.5 ;; 8 (top left near)
	    ;; +1.0 +1.0 +1.0 ;; color
	    +0.5 +0.5 +0.5 ;; 9 (top right near)
	    ;; +1.0 +1.0 +1.0 ;; color
	    -0.5 +0.5 -0.5 ;; 10 (top left far)
	    ;; +1.0 +1.0 +1.0 ;; color
	    +0.5 +0.5 -0.5 ;; 11 (top right far)

	    ;; bottom
	    -0.5 -0.5 +0.5 ;; 12 (bottom left near)
	    ;; +1.0 +1.0 +1.0 ;; color
	    +0.5 -0.5 +0.5 ;; 13 (bottom right near)
	    ;; +1.0 +1.0 +1.0 ;; color
	    -0.5 -0.5 -0.5 ;; 14 (bottom left far)
	    ;; +1.0 +1.0 +1.0 ;; color
	    +0.5 -0.5 -0.5 ;; 15 (bottom right far)

	    ;; right
	    +0.5 -0.5 +0.5 ;; 16 (right bottom near)
	    ;; +1.0 +1.0 +1.0 ;; color
	    +0.5 -0.5 -0.5 ;; 17 (right bottom far)
	    ;; +1.0 +1.0 +1.0 ;; color
	    +0.5 +0.5 +0.5 ;; 18 (right top near)
	    ;; +1.0 +1.0 +1.0 ;; color
	    +0.5 +0.5 -0.5 ;; 19 (right top far)

	    ;; left
	    -0.5 -0.5 +0.5 ;; 20 (left bottom near)
	    ;; +1.0 +1.0 +1.0 ;; color
	    -0.5 -0.5 -0.5 ;; 21 (left bottom far)
	    ;; +1.0 +1.0 +1.0 ;; color
	    -0.5 +0.5 +0.5 ;; 22 (left top near)
	    ;; +1.0 +1.0 +1.0 ;; color
	    -0.5 +0.5 -0.5 ;; 23 (left top far)

	    )
   :vert-count 11
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
