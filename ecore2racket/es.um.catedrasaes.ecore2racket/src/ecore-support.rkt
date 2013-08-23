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
   eobject%)

(define named-element<%> (interface () e-name))
(define classifier<%> (interface (named-element<%>)
                      ;; superclass
                      e-attributes e-references))
(define structural-feature<%> (interface (named-element<%>) e-type))
(define reference<%> (interface (structural-feature<%>)))
(define attribute<%> (interface (structural-feature<%>)))


(define eobject%
  (class* object% (classifier<%>)
    
    (super-new)
    
    ;; classifier<%> interface methods
    (define/public (e-name) "EObject")
    (define/public (e-attributes) null)
    (define/public (e-references) null)))
