(in-package :g3)

(defmacro ->> (value &optional form &rest more)
  (cond
    ((null form) value)
    ((null more) (if (listp form)
		     `(,@form ,value)
		     (list form value)))
    (t `(->> (->> ,value ,form) ,@more))))
