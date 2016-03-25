;=====================================================
; Author: Oniel Toledo
; Date: 11-27-2014
; Description: LISP functions to determine the min/max 
;   		   of a list numberic elements
;=====================================================

;return (<maxvalue> <minvalue>) from given list
(defun maxmin (x)
	(cond
		;do error checking on the list first
		((not(listp x)) "error: maxmin called with non-list")
		((null x) "error: maxmin called with empty list")
		((equal (listCheck x) 1) "error: non-number passed to maxmin")
		;list is good, find min/max values
		(t (list (max x) (min x)))
	)
)

;find the minimum value of the list
(defun min (x)
	(cond
		((null(cdr x)) (car x))
		((<= (car x) (min(cdr x))) (car x))
		((> (car x) (min(cdr x))) (min(cdr x)))
	)
)

;find the maximum value of the list
(defun max (x)
	(cond
		((null(cdr x)) (car x))
		((>= (car x) (max(cdr x))) (car x))
		((< (car x) (max(cdr x))) (max(cdr x)))
	)
)

;check that list contains only atomics in it
; returns 0 if good
; returns 1 if bad
(defun listCheck (x)
	(cond
		((not(numberp(car x))) 1)
		((null (cdr x)) 0)
		(t (listCheck(cdr x)))
	)
)