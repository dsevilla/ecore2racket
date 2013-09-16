#lang racket

;; Minimal interface that resembles ecore metadata
;; As ecore models representational elements are in turn classes
;; and instances, I only include here the static typing information
;; needed to know how to treat each feature of a class (for example,
;; the differentiation between attributes and references even when
;; they're represented by the same abstraction (private field + get &
;; set functions).

(provide
   enamed-element<%>
   eclassifier<%>
   estructuralfeature<%>
   ereference<%>
   eattribute<%>
   eobject%
   epackage%
   to-xml
   to-xexpr)

;;; Interfaces describing the different Ecore metamodel elements. In a
;;; perfect world, these entities would have been generated with the
;;; same interface all other metamodels have, but we need a bootstrap
;;; process first.
(define enamed-element<%> (interface () e-name e-name-set!))
(define eclassifier<%> (interface (enamed-element<%>)
                      ;; superclass
                      e-package e-package-set! e-attributes e-references))
(define epackage<%> (interface (enamed-element<%>)
                      e-parent e-children))
(define estructuralfeature<%> (interface (enamed-element<%>) e-type))
(define ereference<%> (interface (estructuralfeature<%>)))
(define eattribute<%> (interface (estructuralfeature<%>)))

(define eobject%
  (class* object% ()

    (super-new)

    (field [-e-name ""]
           [-e-package null])

    ;; classifier<%> interface methods
    (define/public (e-name) -e-name)
    (define/public (e-name-set! n) (set! -e-name n))

    (define/public (e-package) -e-package)
    (define/public (e-package-set! p) (set! -e-package p))))

(define -eclass%
  (class* eobject% (eclassifier<%>)

    (super-new)
    
    (inherit-field -e-name -e-package)
    (set! -e-name "")
    (set! -e-package null)
    
    (field [-e-attributes null]
           [-e-references null]
           [-e-all-attributes null]
           [-e-all-references null])
    (define/public (e-attributes) -e-attributes)
    (define/public (e-references) -e-references)
    (define/public (e-all-attributes) -e-all-attributes)
    (define/public (e-all-references) -e-all-references)))

(define -epackage%
  (class* -eclass% (epackage<%>)
    (super-new)
    ;; Fake class symbols to close the circle
    (field [-e-parent null]
           [-e-children null])
    
    (define/public (e-parent) -e-parent)
    (define/public (e-parent-set! n) (set! -e-parent n))
    (define/public (e-children) -e-children)
    (define/public (e-children-set! n) (set! -e-children n))
    
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
      ((list 'reference name type contained? minoccur maxoccur)
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
       (set! -e-name "")
       (set! -e-package the-epackage)
       
       ;; Normal class fields
       (inherit-field -e-attributes -e-references)
       (set! -e-attributes #[,@(filter-by-application-symbol 'attribute body)])
       (set! -e-references #[,@(filter-by-application-symbol 'reference body)])
       
       ,@(expand-eclass-body body))))


(define-syntax (with-epackage stx)
    (syntax-case stx ()
      ((_ package body ...)
       (with-syntax ([the-epackage (datum->syntax stx (string->symbol "the-epackage"))])
         #'(begin 
             (define the-epackage package)
             body ...)))))

(define-syntax (with-eclass stx)
    (syntax-case stx ()
      ((_ eclass body ...)
       (with-syntax ([the-eclass (datum->syntax stx (string->symbol "the-eclass"))])
         #'(begin
             (define the-eclass eclass)
             body ...)))))

;;; Ecore classes
(define ecore-package (new -epackage%))
(send ecore-package e-name-set! "ecore")
(with-epackage
 ecore-package
 (eclass 
  eclass% -eclass%)

 (eclass
  epackage% eclass%
  (reference e-parent epackage% #f 0 1)
  (reference e-children eobject% #t 0 -1))

 (eclass
  estructuralfeature% eclass%
  (attribute e-changeable 'boolean 0 1)
  (attribute e-volatile 'boolean 0 1)
  (attribute e-transient 'boolean 0 1)
  (attribute e-unsettable 'boolean 0 1)
  (attribute e-derived 'boolean 0 1)
  (reference e-containing-class eclass% #f 1 1))
 
 (eclass
  eattribute% estructuralfeature%)

 (eclass
  ereference% estructuralfeature%)

 )