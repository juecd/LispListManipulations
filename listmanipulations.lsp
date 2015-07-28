;; SUB-LIST takes 3 arguments, list L, number START, and number LEN specifying the sub-list (starting from index START of length LEN) to grab from list L
;; SPLIT-LIST takes one argument, list L and splits it in half. If the list is of odd length, the second half of the list of one element longer than the first
;; LIST2BTREE takes one argument, list LEAVES and creates a binary tree. If the list is of odd length, the first element (in the first call of the function) is the root node and the rest are split evenly into left and right nodes recursively
;; BTREE2LIST takes one argument, list (binary tree) TREE and creates a list. It recursively appends all elements to each other forming one list.

; SUB-LIST:
; If valid parameters
;; If START begins at index 0, append the first element with all other elements until LEN is reached
;; Else, decrement START until have reached correct starting point
(defun SUB-LIST(L START LEN)
	(cond ((NULL L) NIL)
		((> START (length L)) NIL)
		((= START (length L)) NIL)
		((= LEN 0) NIL)
		((= 0 START) (cons (first L) (SUB-LIST (rest L) START (- LEN 1))))
		(t (SUB-LIST (rest L) (- START 1) LEN))))

; SPLIT-LIST:
; If a list of even length, divide in half
; Else, create the first sublist from 1/2 the list - 1 and the second sublist from 1/2 the list + 1
(defun SPLIT-LIST(L)
	(cond ((NULL L) nil)
		((= 1 (length L)) L)
		((evenp (length L)) (cons (SUB-LIST L 0 (/ (length L) 2)) (cons (SUB-LIST L (/ (length L) 2) (/ (length L) 2)) NIL)))
		(t (cons (SUB-LIST L 0 (/ (- (length L) 1) 2)) (cons (SUB-LIST L (/ (- (length L) 1) 2) (/ (+ (length L) 1) 2)) NIL)))))

; LIST2BTREE:
; Return the item (atom or list) if of length one or two
; else construct a binary tree of the first half of LEAVES (from SPLIT-LIST) plus the second half of LEAVES (from SPLIT-LIST)
(defun LIST2BTREE(LEAVES)
	(cond ((NULL LEAVES) nil)
		((= 1 (length LEAVES)) (first LEAVES))
		((= 2 (length LEAVES)) LEAVES)
		(t (cons (LIST2BTREE (first (SPLIT-LIST LEAVES))) (cons (LIST2BTREE (second (SPLIT-LIST LEAVES))) nil)))))

; BTREE2LIST:
; If the parameter is an atom, return it in list form
; Else if the tree is already in list form (ie of length 1) return it
; Else create a list consisting of all elements from the left (first) node of TREE and all elements from the right (second) node of TREE 
(defun BTREE2LIST(TREE)
	(cond ((NULL TREE) nil)
		((atom TREE) (list TREE))
		((= 1 (length TREE)) TREE)
		(t (append (BTREE2LIST (first TREE)) (BTREE2LIST (second TREE))))))