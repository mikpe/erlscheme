;;; eq-qquote.scm -- quasiquote expander for ErlScheme
;;;
;;; Based on qquote.s from MIT C-Scheme:
;;;
;;;	Copyright (c) 1987 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;
;;; Ported to XScheme 0.16 by Mikael Pettersson
;;; - changed CASEs to CONDs
;;; - eliminate self-reference in finalize-quasiquote and system
;;; - change system
;;; - make quasiquote a compiler macro
;;;
;;; Ported to ErlScheme 0.1 by Mikael Pettersson
;;; - downcase keyword symbols
;;; - temporarily use %map rather than map
;;; - replace "illegal" with "invalid" in finalize-quasiquote error message

;;;; cons* compability (used by quasiquote)

(define (cons* first-element . rest-elements)
  (define (loop this-element rest-elements)
    (if (null? rest-elements)
	this-element
	(cons this-element
	      (loop (car rest-elements)
		    (cdr rest-elements)))))
  (loop first-element rest-elements))

;;;; Quasiquote

(let ()

(define (descend-quasiquote x level return)
  (cond ((pair? x) (descend-quasiquote-pair x level return))
	((vector? x) (descend-quasiquote-vector x level return))
	(else (return 'quote x))))

(define (descend-quasiquote-pair x level return)
  (define (descend-quasiquote-pair* level)
    (descend-quasiquote (car x) level
      (lambda (car-mode car-arg)
	(descend-quasiquote (cdr x) level
	  (lambda (cdr-mode cdr-arg)
	    (cond ((and (eq? car-mode 'quote)
			(eq? cdr-mode 'quote))
		   (return 'quote x))
		  ((eq? car-mode 'unquote-splicing)
		   (if (and (eq? cdr-mode 'quote)
			    (null? cdr-arg))
		       (return 'unquote car-arg)
		       (return (system 'append)
			       (list car-arg
				     (finalize-quasiquote cdr-mode cdr-arg)))))
		  ((and (eq? cdr-mode 'quote)
			(null? cdr-arg))
		   (return 'list
			   (list (finalize-quasiquote car-mode car-arg))))
		  ((and (eq? cdr-mode 'quote)
			(list? cdr-arg))
		   (return 'list
			   (cons (finalize-quasiquote car-mode car-arg)
				 (map (lambda (el)
					(finalize-quasiquote 'quote el))
				      cdr-arg))))
		  ((memq cdr-mode '(list cons))
		   (return cdr-mode
			   (cons (finalize-quasiquote car-mode car-arg)
				 cdr-arg)))
		  (else
		   (return
		    'cons
		    (list (finalize-quasiquote car-mode car-arg)
			  (finalize-quasiquote cdr-mode cdr-arg))))))))))
  (cond ((eq? (car x) 'quasiquote) (descend-quasiquote-pair* (+ level 1)))
	((memq (car x) '(unquote unquote-splicing))
	 (if (zero? level)
	     (return (car x) (cadr x))
	     (descend-quasiquote-pair* (- level 1))))
	(else (descend-quasiquote-pair* level))))

(define (descend-quasiquote-vector x level return)
  (descend-quasiquote (vector->list x) level
    (lambda (mode arg)
      (cond ((eq? mode 'quote)
	     (return 'quote x))
	    ((eq? mode 'list)
	     (return (system 'vector) arg))
	    (else
	     (return (system 'list->vector)
		     (list (finalize-quasiquote mode arg))))))))

(define (finalize-quasiquote mode arg)
  (cond ((eq? mode 'quote) (list 'quote arg))
	((eq? mode 'unquote) arg)
	((eq? mode 'unquote-splicing) (error ",@ in invalid context" arg))
	((eq? mode 'list) (cons (system 'list) arg))
	((eq? mode 'cons)
	 (if (null? (cddr arg))		; (= (length arg) 2)
	     (cons (system 'cons) arg)
	     (cons (system 'cons*) arg)))
	(else (cons mode arg))))

(define (system name)
  ;; Here's a problem: quasiquote expansions need to use some standard
  ;; procedures, like CONS or VECTOR.  But these names can have been
  ;; rebound in the user's code, so to be extra safe we should try to
  ;; get the `system' bindings for these names: `(ACCESS ,name #F) does
  ;; that in C-Scheme.  An approximation here could be:
  ;; (list 'ACCESS name 'initial-user-environment), but that doesn't work
  ;; (gives #!unassigned for some reason) so the only thing left to do
  ;; is to return `name' as-is and hope there are no clashes.
  ;; XXX: ErlScheme: use package-qualified names once packages are in.
  name)

;;;; the glue

(macro quasiquote
  (lambda (form)
    (descend-quasiquote (cadr form) 0 finalize-quasiquote)))

)
