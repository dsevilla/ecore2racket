#lang racket/base

;; Minimal interface that resembles ecore metadata
;; As ecore models representational elements are in turn classes
;; and instances, I only include here the static typing information
;; needed to know how to treat each feature of a class (for example,
;; the differentiation between attributes and references even when
;; they're represented by the same abstraction (private field + get &
;; set functions).

(require racket/class)

(provide
   named-element<%>
   classifier<%>
   structural-feature<%>
   reference<%>
   attribute<%>
   eobject%
   package%
   to-xml
   to-xexpr)

(define named-element<%> (interface () e-name e-name-set!))
(define classifier<%> (interface (named-element<%>)
                      ;; superclass
                      e-package e-package-set! e-attributes e-references))
(define structural-feature<%> (interface (named-element<%>) e-type))
(define reference<%> (interface (structural-feature<%>)))
(define attribute<%> (interface (structural-feature<%>)))

(define eobject%
  (class* object% (classifier<%>)
    
    (super-new)
    
    (define -e-name "EObject")
    (define -e-package "ecore")
    
    ;; classifier<%> interface methods
    (define/public (e-name) -e-name)
    (define/public (e-name-set! n) (set! -e-name n))
    
    (define/public (e-package) -e-package)
    (define/public (e-package-set! p) (set! -e-package p))
    (define/public (e-attributes) null)
    (define/public (e-references) null)
    (define/public (e-all-attributes) null)
    (define/public (e-all-references) null)))

(define package%
  (class* eobject% ()
    (super-new)
    
    (define -e-name "EPackage")
    (define -e-package "ecore")
    
    
))

;; TODO: provide contracts for these.

(require xml)

;; Build the xexpr
(define (to-xml o-base)
  (xexpr->string (to-xexpr o-base)))

(define (attr-to-xexpr obj att-name)
  `(,att-name ,(dynamic-send obj att-name)))

(define (to-xexpr o-base)
  `(,(string->symbol (string-append (send o-base e-package) ":" (send o-base e-name)))
    (,@(map (lambda (att) (attr-to-xexpr o-base att)) (send o-base e-all-attributes))
     (xmlns:box "http://www.catedrasaes.org/Box")
     (xmlns:xmi "http://www.omg.org/XMI")
     (xmlns:xsi "http://www.w3.org/2001/XMLSchema-instance"))))
