#lang racket

;; Minimal interface that resembles ecore metadata
;; As ecore models representational elements are in turn classes
;; and instances, I only include here the static typing information
;; needed to know how to treat each feature of a class (for example,
;; the differentiation between attributes and references even when
;; they're represented by the same abstraction (private field + get &
;; set functions).

(provide
   named-element<%>
   classifier<%>
   structural-feature<%>
   reference<%>
   attribute<%>
   eobject%
   epackage%
   to-xml
   to-xexpr)

;;; Interfaces describing the different Ecore metamodel elements. In a
;;; perfect world, these entities would have been generated with the
;;; same interface all other metamodels have, but we need a bootstrap
;;; process first.
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

    (field [-e-name "EObject"]
           [-e-package null])

    ;; classifier<%> interface methods
    (define/public (e-name) -e-name)
    (define/public (e-name-set! n) (set! -e-name n))

    (define/public (e-package) -e-package)
    (define/public (e-package-set! p) (set! -e-package p))
    
    (field [-e-attributes null]
           [-e-references null]
           [-e-all-attributes null]
           [-e-all-references null])
    (define/public (e-attributes) -e-attributes)
    (define/public (e-references) -e-references)
    (define/public (e-all-attributes) -e-all-attributes)
    (define/public (e-all-references) -e-all-references)))

(define epackage%
  (class* eobject% ()
    (super-new)

    (inherit-field -e-name -e-package)
    (set! -e-name "EPackage")
    (set! -e-package "ecore")


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require compatibility/defmacro (for-syntax racket/match racket/list))
(provide eclass)

(begin-for-syntax
  ;; Cannot use define-macro for syntax...
  (define-syntax with-gensyms
    (syntax-rules ()
      ((_ (vars ...) body ...)
       (let ((vars (gensym)) ...)
         body ...))))

;  (define-macro (with-gensyms list . body)
;    `(let (,@(map (λ (v) `(,v (gensym))) list))
;       ,@body))


  (define (filter-by-application-symbol symbol list)
    (filter-map (lambda (x) (and (eq? (car x) symbol) (cadr x))) list))

  (define (append-id . list)
    (string->symbol
     (apply
      string-append
      (map (lambda (s-s)
             (if (symbol? s-s)
                 (symbol->string s-s)
                 s-s))
           list))))

  (define (expand-class-attribute list)
    (match list
      ((list 'attribute name type minoccur maxoccur)
       (let ((field-name (append-id "-" name))
             (set-name (append-id name "-set!")))
         (if (= maxoccur 1)
             `((field [,field-name 0])
               (define/public (,name) ,field-name)
               (define/public (,set-name value)
                 (set! ,field-name value)))
             ;; multi-valuated
             (with-gensyms (tmp-vec-n tmp-pos-n tmp-val-n)
               `((field [,field-name (make-vector 0)])
                 (define ,name
                   (case-lambda
                     (() ,field-name)
                     ((,tmp-pos-n)
                      (when (<= (vector-length ,field-name) ,tmp-pos-n)
                        (let ((,tmp-vec-n (make-vector (add1 ,tmp-pos-n) null)))
                          ;; grow the vector
                          (vector-copy! ,tmp-vec-n 0 ,field-name)
                          (set! ,field-name ,tmp-vec-n)))
                      (vector-ref ,field-name ,tmp-pos-n))))
                 (public ,name)
                 (define ,set-name
                   (case-lambda
                     ((,tmp-val-n) (set! ,field-name ,tmp-val-n))
                     ((,tmp-val-n ,tmp-pos-n)
                      (when (<= (vector-length ,field-name) ,tmp-pos-n)
                        (let ((,tmp-vec-n (make-vector (add1 ,tmp-pos-n) null)))
                          ;; grow the vector
                          (vector-copy! ,tmp-vec-n 0 ,field-name)
                          (set! ,field-name ,tmp-vec-n)))
                      (vector-set! ,field-name ,tmp-pos-n ,tmp-val-n))))
                 (public ,set-name))))))))

  (define (expand-class-reference list)
    (match list
      ((list 'reference name type minoccur maxoccur)
       (let ((field-name (append-id "-" name))
             (set-name (append-id name "-set!")))
         (if (= maxoccur 1)
             `((field [,field-name null])
               (define/public (,name) ,field-name)
               (define/public (,set-name value)
                 (set! ,field-name value)))
             ;; multi-valuated
             (with-gensyms
              (tmp-vec-n tmp-pos-n tmp-val-n)
              `((field [,field-name (make-vector 0 null)])
                (define ,name
                  (case-lambda
                    (() ,field-name)
                    ((,tmp-pos-n)
                     (when (<= (vector-length ,field-name) ,tmp-pos-n)
                       (let ((,tmp-vec-n (make-vector (add1 ,tmp-pos-n) null)))
                         ;; grow the vector
                         (vector-copy! ,tmp-vec-n 0 ,field-name)
                         (set! ,field-name ,tmp-vec-n)))
                     (vector-ref ,field-name ,tmp-pos-n))))
                 (public ,name)
                 (define ,set-name
                   (case-lambda
                     ((,tmp-val-n) (set! ,field-name ,tmp-val-n))
                     ((,tmp-val-n ,tmp-pos-n)
                      (when (<= (vector-length ,field-name) ,tmp-pos-n)
                        (let ((,tmp-vec-n (make-vector (add1 ,tmp-pos-n) null)))
                          ;; grow the vector
                          (vector-copy! ,tmp-vec-n 0 ,field-name)
                          (set! ,field-name ,tmp-vec-n)))
                      (vector-set! ,field-name ,tmp-pos-n ,tmp-val-n))))
                 (public ,set-name))))))))

  (define (expand-eclass-body body)
    (if (null? body)
        '()
        (if (and (list? (car body)) (not (null? (car body))))
            (append (cond
                      ;; Attribute
                      ((eq? (caar body) 'attribute)
                       (expand-class-attribute (car body)))
                      ;; Reference
                      ((eq? (caar body) 'reference)
                       (expand-class-reference (car body)))
                      (else
                       (list (car body))))
                    (expand-eclass-body (cdr body)))
            (cons (car body) (expand-eclass-body (cdr body))))))

  )

;; The macro proper.
(define-macro (eclass n super . body)
  `(define ,n
     (class ,super
       (super-new)
       
       (inherit-field -e-name -e-package)
       (set! -e-name ,(symbol->string n))
       (set! -e-package null)
       
       ;; Normal class fields
       (field [-e-attributes #[,@(filter-by-application-symbol 'attribute body)]])
       (define (e-attributes) -e-attributes)
       (field [-e-references #[,@(filter-by-application-symbol 'reference body)]])
       (define (e-references) -e-references)
       
       ,@(expand-eclass-body body))))


;;; test
(eclass x% eobject%
       (define/public (test1) 1)
       (attribute pepe 'string 1 10)
       (attribute juan 'string 1 1)
       (reference abc eobject% 1 1)
       (define/public (test2) 2))
