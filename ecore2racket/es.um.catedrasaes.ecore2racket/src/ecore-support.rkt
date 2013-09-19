#lang racket

;; Minimal interface that resembles ecore metadata
;; As ecore models representational elements are in turn classes
;; and instances, I only include here the static typing information
;; needed to know how to treat each feature of a class (for example,
;; the differentiation between attributes and references even when
;; they're represented by the same abstraction (private field + get &
;; set functions). Sorry for the camel case, but it is easier to
;; match what will appear in the XMI.

(provide
   ENamedElement<%>
   EClassifier<%>
   EStructuralFeature<%>
   EReference<%>
   EAttribute<%>
   EObject<%>
   EObject%
   eclass
   with-epackage
   ecore-package
   to-xml
   to-xexpr)

;;; Interfaces describing the different Ecore metamodel elements. In a
;;; perfect world, these entities would have been generated with the
;;; same interface all other metamodels have, but we need a bootstrap
;;; process first.
(define EObject<%> (interface ()))
(define ENamedElement<%> (interface (EObject<%>) name name-set!))
(define EClassifier<%> (interface (ENamedElement<%>)
                      ;; superclass
                      ePackage ePackage-set!))
(define EClass<%> (interface (EClassifier<%>)
                      abstract interface eIDAttribute eOperations eStructuralFeatures eAttributes eReferences))
(define EPackage<%> (interface (ENamedElement<%>)
                      eSuperPackage eClassifiers))
(define EStructuralFeature<%> (interface (ENamedElement<%>) eType))
(define EReference<%> (interface (EStructuralFeature<%>)))
(define EAttribute<%> (interface (EStructuralFeature<%>)))

(define EObject%
  (class* object% ()

    (super-new)))

;    (field [-e-name ""]
;           [-e-package null])
;
;    ;; classifier<%> interface methods
;    (define/public (e-name) -e-name)
;    (define/public (e-name-set! n) (set! -e-name n))
;
;    (define/public (e-package) -e-package)
;    (define/public (e-package-set! p) (set! -e-package p))))

;(define -eclass%
;  (class* eobject% (eclassifier<%>)
;
;    (super-new)
;
;    (inherit-field -e-name -e-package)
;    (set! -e-name "")
;    (set! -e-package null)
;
;    (field [-e-attributes null]
;           [-e-references null]
;           [-e-all-attributes null]
;           [-e-all-references null])
;    (define/public (e-attributes) -e-attributes)
;    (define/public (e-references) -e-references)
;    (define/public (e-all-attributes) -e-all-attributes)
;    (define/public (e-all-references) -e-all-references)))

(define -EPackage%
  (class* EObject% (EPackage<%>)
    (super-new)
    ;; Fake class symbols to close the circle
    (field [-name ""]
           [-eSuperPackage null]
           [-eClassifiers (make-vector 0)])

    (define/public (name) -name)
    (define/public (name-set! n) (set! -name n))

    (define/public (eSuperPackage) -eSuperPackage)
    (define/public (eSuperPackage-set! n) (set! -eSuperPackage n))
    (define/public (eClassifiers) -eClassifiers)
    (define/public (eClassifiers-set! n) (set! -eClassifiers n))
    (define/public (eClassifiers-append! c)
      (set! -eClassifiers
            (vector-append -eClassifiers (vector c))))))

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
(require compatibility/defmacro 
         (for-syntax racket/match 
                     racket/list 
                     racket))

(begin-for-syntax
  
;  (define-macro (with-gensyms list . body)
;    `(let (,@(map (λ (v) `(,v (gensym ',v))) list))
;       ,@body))

  
  ;; Cannot use define-macro for syntax...
  (define-syntax (with-gensyms stx)
    (syntax-case stx ()
      ((_ (vars ...) body ...)
       (with-syntax
           ([(gensyms ...) #'((gensym 'vars) ...)])
         #'(let ((vars gensyms) ...)
             body ...)))))
  
;  ;; same
;  (define-syntax (with-gensyms stx)
;    (syntax-case stx ()
;      ((_ (vars ...) body ...)
;       (with-syntax
;           ([(gensyms ...)
;             (map (lambda (s)
;                    (datum->syntax 
;                     stx 
;                     `(gensym ',s)))
;                  (syntax->datum #'(vars ...)))])
;         #'(let ((vars gensyms) ...)
;             body ...)))))

  
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

  (define (mono-attribute att-name)
    (let ((field-name (append-id "-" att-name))
          (set-name (append-id att-name "-set!")))
      `(begin
         (field [,field-name 0])
         (define/public (,att-name) ,field-name)
         (define/public (,set-name value)
           (set! ,field-name value)))))
    
  (define (multi-attribute att-name)
    (let ((field-name (append-id "-" att-name))
          (set-name (append-id att-name "-set!"))
          (append-name (append-id att-name "-append!")))
      (with-gensyms (tmp-vec-n tmp-pos-n tmp-val-n)
        `(begin
          (field [,field-name (make-vector 0)])
          (define ,att-name
            (case-lambda
              (() ,field-name)
              ((,tmp-pos-n)
               (when (<= (vector-length ,field-name) ,tmp-pos-n)
                 (let ((,tmp-vec-n (make-vector (add1 ,tmp-pos-n) null)))
                   ;; grow the vector
                   (vector-copy! ,tmp-vec-n 0 ,field-name)
                   (set! ,field-name ,tmp-vec-n)))
               (vector-ref ,field-name ,tmp-pos-n))))
          (public ,att-name)
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
          (public ,set-name)
           (define ,append-name
             (lambda (,tmp-val-n)
               (let* ((,tmp-pos-n (vector-length ,field-name))
                      (,tmp-vec-n (make-vector (add1 ,tmp-pos-n))))
                 ;; grow the vector
                 (vector-copy! ,tmp-vec-n 0 ,field-name)
                 (set! ,field-name ,tmp-vec-n)
                 (vector-set! ,field-name ,tmp-pos-n ,tmp-val-n))))
           (public ,append-name)))))

  (define (expand-class-attribute list)
    (match list
      ((list 'attribute name type minoccur maxoccur)
       (if (= maxoccur 1)
           (mono-attribute name)
           ;; multi-valuated
           (multi-attribute name)))))
         
  (define (mono-reference ref-name)
    (let ((field-name (append-id "-" ref-name))
          (set-name (append-id ref-name "-set!")))
      `(begin
         (field [,field-name null])
         (define/public (,ref-name) ,field-name)
         (define/public (,set-name value)
           (set! ,field-name value)))))
        
  (define (multi-reference ref-name)
    (let ((field-name (append-id "-" ref-name))
          (set-name (append-id ref-name "-set!"))
          (append-name (append-id ref-name "-append!")))
      (with-gensyms (tmp-vec-n tmp-pos-n tmp-val-n)
        `(begin
           (field [,field-name (make-vector 0 null)])
           (define ,ref-name
             (case-lambda
               (() ,field-name)
               ((,tmp-pos-n)
                (when (<= (vector-length ,field-name) ,tmp-pos-n)
                  (let ((,tmp-vec-n (make-vector (add1 ,tmp-pos-n) null)))
                    ;; grow the vector
                    (vector-copy! ,tmp-vec-n 0 ,field-name)
                    (set! ,field-name ,tmp-vec-n)))
                (vector-ref ,field-name ,tmp-pos-n))))
           (public ,ref-name)
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
           (public ,set-name)
           (define ,append-name
             (lambda (,tmp-val-n)
               (let* ((,tmp-pos-n (vector-length ,field-name))
                      (,tmp-vec-n (make-vector (add1 ,tmp-pos-n))))
                 ;; grow the vector
                 (vector-copy! ,tmp-vec-n 0 ,field-name)
                 (set! ,field-name ,tmp-vec-n)
                 (vector-set! ,field-name ,tmp-pos-n ,tmp-val-n))))
           (public ,append-name)))))
  
  (define (expand-class-reference list)
    (match list
      ((list 'reference name type contained? minoccur maxoccur)
       (if (= maxoccur 1)
           (mono-reference name)
             ;; multi-valuated
           (multi-reference name)))))
    
  (define (expand-eclass-body body)
    (map (lambda (e)
           (if (pair? e)
               (cond
                 ;; Attribute
                 ((eq? (car e) 'attribute)
                  (expand-class-attribute e))
                 ;; Reference
                 ((eq? (car e) 'reference)
                  (expand-class-reference e))
                 (else
                  e))
               e))
         body))
)

;; The macro proper. Private version to generate Ecore itself.
(define-macro (-eclass n super . body)
  `(begin
     (define ,n
       (class ,super
         (super-new)
         
         ;; Normal class fields
;       (inherit-field -e-attributes -e-references)
;       (set! -e-attributes #[,@(filter-by-application-symbol 'attribute body)])
;       (set! -e-references #[,@(filter-by-application-symbol 'reference body)])

         ,@(expand-eclass-body body)))
     (send the-epackage eClassifiers-append! (new ,n))))


(define-syntax (with-epackage stx)
    (syntax-case stx ()
      ((_ package body ...)
       (with-syntax ([the-epackage (datum->syntax stx 'the-epackage)])
         #'(begin
             (define the-epackage package)
             body ...)))))

(define-syntax (with-eclass stx)
    (syntax-case stx ()
      ((_ eclass body ...)
       (with-syntax ([the-eclass (datum->syntax stx 'the-eclass)])
         #'(begin
             (define the-eclass eclass)
             body ...)))))

;;; XXX Temporary
(define-syntax reference*
  (syntax-rules ()
    ((_ elems ...)
     (begin))))

;;; Ecore classes
(define ecore-package (new -EPackage%))
(send ecore-package name-set! "ecore")
(with-epackage
 ecore-package

 (-eclass
  EModelElement% EObject%)
 (provide EModelElement%)

 (-eclass
  ENamedElement% EModelElement%
  (attribute name 'string 1 1))

 (-eclass
  EClassifier% ENamedElement%
  (reference ePackage EPackage% #f 0 1))

 (-eclass
  EClass% EClassifier%
  (attribute abstract 'boolean 0 1)
  (attribute interface 'boolean 0 1)
  (reference eIDAttribute EAttribute% #f 0 1)
  (reference eOperations EOperation% #t 0 -1)
  (reference eStructuralFeatures EStructuralFeature% #t 0 -1)

  ;; Note: as eAttributes and eReferences are derived, and so
  ;; frequently used, we'll devise a mechanism to recreate the
  ;; list of eAttributes and eReferences from the
  ;; e-structural-features field.
  
  (reference* eReferences EReference% #f 0 -1)
  (reference* eAllReferences EReference% #f 0 -1)
  (reference* eAttributes EAttribute% #f 0 -1)
  (reference* eAllAttributes EAttribute% #f 0 -1)

  (reference* eAllOperations EOperation% #f 0 -1)

  (reference* eAllStructuralFeatures EStructuralFeature% #f 0 -1)

  (reference* eAllSuperTypes EClass% #f 0 -1))
 (provide EClass%)

 (-eclass
  EPackage% ENamedElement%
  (attribute nsUri 'string 0 1)
  (attribute nsPrefix 'string 0 1)
  (reference eSuperPackage EPackage% #f 0 1)
  (reference eClassifiers EClassifier% #t 0 -1)
  (reference eSubpackages EPackage% #t 0 -1))
 (provide EPackage%)

 (-eclass
  ETypedElement% EClass%
  (attribute ordered 'boolean 0 1)
  (attribute unique 'boolean 0 1)
  (attribute lowerBound 'number 0 1)
  (attribute upperBound 'number 0 1)
  (attribute many 'boolean 0 1)
  (attribute required 'boolean 0 1)
  (reference eType EClassifier% #f 0 1))

 (-eclass
  EOperation% ETypedElement%
  (reference eContainingClass EClass% #f 1 1)
  (reference eParameters EParameter% #t 0 -1))
 (provide EOperation%)

 (-eclass
  EParameter% ETypedElement%
  (reference eOperation EOperation% #f 1 1))
 (provide EParameter%)

 (-eclass
  EStructuralFeature% EClass%
  (attribute changeable 'boolean 0 1)
  (attribute volatile 'boolean 0 1)
  (attribute transient 'boolean 0 1)
  (attribute unsettable 'boolean 0 1)
  (attribute derived 'boolean 0 1)
  (reference eContainingClass EClass% #f 0 1))
 (provide EStructuralFeature%)

 (-eclass
  EAttribute% EStructuralFeature%
  (attribute iD 'boolean 0 1)
  (reference eAttributeType EDataType% #f 1 1))
 (provide EAttribute%)

 (-eclass
  EReference% EStructuralFeature%
  (attribute containment 'boolean 0 1)
  (attribute container 'boolean 0 1)
  (reference eOpposite EReference% #f 0 1)
  (reference eReferenceType EClass% #f 1 1))
 (provide EReference%)

 (-eclass
  EDataType% EClassifier%
  (attribute serializable 'boolean 0 1))
 (provide EDataType%)

 )

;; The macro proper. Private version to generate Ecore itself.
(define-macro (eclass n super ifaces . body)
  `(begin
     (define ,n
       (class* ,super ,ifaces
         (super-new)
         
         ;; Normal class fields
;       (inherit-field -e-attributes -e-references)
;       (set! -e-attributes #[,@(filter-by-application-symbol 'attribute body)])
;       (set! -e-references #[,@(filter-by-application-symbol 'reference body)])

         ,@(expand-eclass-body body)))
     (send the-epackage eClassifiers-append! (new ,n))))

