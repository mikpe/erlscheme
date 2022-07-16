;;;   Copyright 2014 Mikael Pettersson
;;;
;;;   Licensed under the Apache License, Version 2.0 (the "License");
;;;   you may not use this file except in compliance with the License.
;;;   You may obtain a copy of the License at
;;;
;;;       http://www.apache.org/licenses/LICENSE-2.0
;;;
;;;   Unless required by applicable law or agreed to in writing, software
;;;   distributed under the License is distributed on an "AS IS" BASIS,
;;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;;   See the License for the specific language governing permissions and
;;;   limitations under the License.
;;;
;;; es-macros.scm -- ErlScheme macros and compiler syntax

;;; helpers

(define %map
  (letrec ((map
	    (lambda (f lst)
	      (if (null? lst)
		  '()
		  (cons (f (car lst)) (map f (cdr lst)))))))
    map))

;;; the expander engine

(define %expand-macros
  (lambda (expr)
    (if (pair? expr)
	(let ((head (car expr)))
	  (if (symbol? head)
	      (let ((expander (getprop head '%syntax)))
		(if expander
		    (expander expr)
		    (let ((expander (getprop head '%macro)))
		      (if expander
			  (%expand-macros (expander expr))
			  (cons head (%expand-list (cdr expr)))))))
	      (%expand-list expr)))
	expr)))

(define %expand-list
  (lambda (lst)
    (%map %expand-macros lst)))

;;; redefine eval to expand macros/syntax before calling the primitive eval

(define eval
  (let ((%eval eval))
    (lambda (expr)
      (%eval (%expand-macros expr)))))

;;; define (macro ...) and (compiler-syntax ...) [the latter depends on the new eval]

(putprop 'macro '%macro
  (lambda (form)
    (list 'putprop
	  (list 'quote (cadr form))
	  (list 'quote '%macro)
	  (caddr form))))

(macro compiler-syntax
  (lambda (form)
    (list 'putprop
	  (list 'quote (cadr form))
	  (list 'quote '%syntax)
	  (caddr form))))

;;; various compiler-syntax forms implemented below

;;; QUOTE

(compiler-syntax quote
  (lambda (form)
    form))

;;; (SET! var exp)

(compiler-syntax set!
  (lambda (form)
    (list 'set! (cadr form) (%expand-macros (caddr form)))))

;;; COND

(compiler-syntax cond
  (letrec ((expand-cond
	     (lambda (cond0 rest)
	       (let ((pred (car cond0))
		     (conseq (cons 'begin (%expand-list (cdr cond0)))))
		 ;; XXX: This expansion fails to handle a clause like (<test>)
		 ;; which should return the value of <test> if it is non-false.
		 (if (eq? pred 'else)
		     conseq
		     (cons 'if
			   (cons (%expand-macros pred)
				 (cons conseq
				       (if (null? rest)
					   '()
					   (list (expand-cond (car rest) (cdr rest))))))))))))
    (lambda (form)
      (if (null? (cdr form))	; (cond)
	  '(begin)		; (cond) -> (begin), both forms are invalid
	  (expand-cond (cadr form) (cddr form))))))

;;; expand (define ...) forms at the start of a body to (letrec ...)

(define %expand-body
  (let ((def->bnd
	  (lambda (def)
	    (list (cadr def) (caddr def)))))
    (let ((make-body
	    (lambda (bnds body)
	      (if (null? bnds)
		  body
		  (list (cons 'letrec (cons bnds body)))))))
      (letrec ((scan
		 (lambda (forms bnds)
		   (if (null? forms)
		       (make-body bnds '((begin)))	; empty body -> (begin), both forms are invalid
		       (if (and (pair? (car forms)) (eq? (caar forms) 'define))
			   (scan (cdr forms) (cons (def->bnd (car forms)) bnds))
			   (make-body bnds forms))))))
	(lambda (forms)
	  (scan (%expand-list forms) '()))))))

;;; (DEFINE var exp) and (DEFINE (var . formals) . body)

(compiler-syntax define
  (lambda (form)
    (let ((lhs (cadr form))
	  (rhs (cddr form)))
      (if (pair? lhs)
	  (list 'define (car lhs) (cons 'lambda (cons (cdr lhs) (%expand-body rhs))))
	  (list 'define lhs (%expand-macros (car rhs)))))))

;;; (LAMBDA formals . body)

(compiler-syntax lambda
  (lambda (form)
    (cons 'lambda (cons (cadr form) (%expand-body (cddr form))))))

;;; LET/LET*/LETREC/LET <name>

(define %expand-let-binding
  (lambda (binding)
    (if (pair? binding)
	(list (car binding) (%expand-macros (cadr binding)))
	binding)))

(define %expand-let-or-letrec
  (lambda (form)
    (cons (car form)	; LET or LETREC
	  (cons (%map %expand-let-binding (cadr form))
		(%expand-body (cddr form))))))

(define %expand-let*
  (letrec ((expand-bindings
	     (lambda (bindings body)
	       (if (null? bindings)
		   body
		   (list 'let
			 (list (%expand-let-binding (car bindings)))
			 (expand-bindings (cdr bindings) body))))))
    (lambda (form)
      (expand-bindings (cadr form) (cons 'begin (%expand-body (cddr form)))))))

(define %expand-let
  (let ((expand-init
	 (lambda (binding)
	   (%expand-macros (cadr binding)))))
    (lambda (form)
      (if (symbol? (cadr form))	; (LET <name> <bindings> . <body>)
	  (list 'letrec
		(list (list (cadr form)
			    (cons 'lambda
				  (cons (%map car (caddr form))
					(%expand-body (cdddr form))))))
		(cons (cadr form) (%map expand-init (caddr form))))
	  (%expand-let-or-letrec form)))))

(compiler-syntax letrec %expand-let-or-letrec)
(compiler-syntax let* %expand-let*)
(compiler-syntax let %expand-let)

;;; CASE

(compiler-syntax case
  (let ((expand-case
	 (lambda (clause)
	   (cons (car clause) (%expand-list (cdr clause))))))
    (lambda (form)
      (cons 'case
	    (cons (%expand-macros (cadr form))
		  (%map expand-case (cddr form)))))))

;;; (EVAL-AT-COMPILE expr)

(compiler-syntax eval-at-compile
  (lambda (form)
    (list 'quote (eval (cadr form)))))
