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
   eclass-of
   eobject->xexpr
   to-xml
   to-xexpr)

;;; Interfaces describing the different Ecore metamodel elements. In a
;;; perfect world, these entities would have been generated with the
;;; same interface all other metamodels have, but we need a bootstrap
;;; process first.
(define EObject<%> (interface () eClass eClass-set!))
(define ENamedElement<%> (interface (EObject<%>) name name-set!))
(define EClassifier<%> (interface (ENamedElement<%>)
                         ;; superclass
                         ePackage ePackage-set!))
(define EClass<%> (interface (EClassifier<%>)
                    abstract interface eIDAttribute eOperations eSuperTypes eStructuralFeatures eAttributes eReferences))
(define EPackage<%> (interface (ENamedElement<%>)
                      eSuperPackage eClassifiers))
(define EStructuralFeature<%> (interface (ENamedElement<%>) eType))
(define EReference<%> (interface (EStructuralFeature<%>)))
(define EAttribute<%> (interface (EStructuralFeature<%>)))

(define EObject-base%
  (class* object% (EObject<%>)

    (super-new)

    ;; Class pointer
    (field [-eClass null])
    (define/public (eClass) -eClass)
    (define/public (eClass-set! klass) (set! -eClass klass))))

(define (eclassifier-hash-table-mixin% %)
  (class %
    (super-new)
    (field [-eClassifiers-hash-table (make-hash)])
    (define/public (eClassifiers-table-add! id classifier)
      (hash-set! -eClassifiers-hash-table id classifier))
    (define/public (eClassifiers-get-by-id id)
      (hash-ref -eClassifiers-hash-table id null))))

(define -EPackage-base%
  (class* EObject-base% (EPackage<%>)
    (super-new)
    ;; Fake class symbols to close the circle
    (field [-name ""]
           [-nsURI ""]
           [-nsPrefix ""]
           [-eSuperPackage null]
           [-eClassifiers (make-vector 0)])

    (define/public (name) -name)
    (define/public (name-set! n) (set! -name n))
    (define/public (nsURI) -nsURI)
    (define/public (nsURI-set! n) (set! -nsURI n))
    (define/public (nsPrefix) -nsPrefix)
    (define/public (nsPrefix-set! n) (set! -nsPrefix n))

    (define/public (eSuperPackage) -eSuperPackage)
    (define/public (eSuperPackage-set! n) (set! -eSuperPackage n))
    (define/public (eClassifiers) -eClassifiers)
    (define/public (eClassifiers-set! n) (set! -eClassifiers n))
    (define/public (eClassifiers-append! c)
      (set! -eClassifiers
            (vector-append -eClassifiers (vector c))))))

(define -EPackage%
  (eclassifier-hash-table-mixin% -EPackage-base%))

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

  (define (expand-class-attribute list)
    (match list
      ((list 'attribute name type minoccur maxoccur)
       (if (= maxoccur 1)
           (new-field-mono name 0) ;; TODO: exact type
           ;; multi-valuated
           (new-field-multi name 0)))))

  (define (new-field-mono f-name (default-value null))
    (let ((field-name (append-id "-" f-name))
          (set-name (append-id f-name "-set!")))
      `(begin
         (field [,field-name ',default-value])
         (define/public (,f-name) ,field-name)
         (define/public (,set-name value)
           (set! ,field-name value)))))

  (define (new-field-multi f-name (default-value null))
    (let ((field-name (append-id "-" f-name))
          (set-name (append-id f-name "-set!"))
          (append-name (append-id f-name "-append!")))
      (with-gensyms (tmp-vec-n tmp-pos-n tmp-val-n)
        `(begin
           (field [,field-name (make-vector 0 ',default-value)])
           (define ,f-name
             (case-lambda
               (() ,field-name)
               ((,tmp-pos-n)
                (when (<= (vector-length ,field-name) ,tmp-pos-n)
                  (let ((,tmp-vec-n (make-vector (add1 ,tmp-pos-n) ',default-value)))
                    ;; grow the vector
                    (vector-copy! ,tmp-vec-n 0 ,field-name)
                    (set! ,field-name ,tmp-vec-n)))
                (vector-ref ,field-name ,tmp-pos-n))))
           (public ,f-name)
           (define ,set-name
             (case-lambda
               ((,tmp-val-n) (set! ,field-name ,tmp-val-n))
               ((,tmp-val-n ,tmp-pos-n)
                (when (<= (vector-length ,field-name) ,tmp-pos-n)
                  (let ((,tmp-vec-n (make-vector (add1 ,tmp-pos-n) ',default-value)))
                    ;; grow the vector
                    (vector-copy! ,tmp-vec-n 0 ,field-name)
                    (set! ,field-name ,tmp-vec-n)))
                (vector-set! ,field-name ,tmp-pos-n ,tmp-val-n))))
           (public ,set-name)
           (define ,append-name
             (lambda (,tmp-val-n)
               (,set-name ,tmp-val-n (vector-length ,field-name))))
           (public ,append-name)))))

  (define (expand-class-reference list)
    (match list
      ((list 'reference name type contained? minoccur maxoccur)
       (if (= maxoccur 1)
           (new-field-mono name)
           ;; multi-valuated
           (new-field-multi name)))))

  (define (-expand-eclass-body body)
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

         ,@(-expand-eclass-body body)))
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

(define-macro (ref/derived name type contained? minoccur maxoccur body)
  `(begin
     (field [,(append-id "-" name) (make-vector 0)])
     (define/public (,name)
       ,body)))

;;; Ecore classes
(define ecore-package (new -EPackage%))
(send ecore-package name-set! "ecore")
(send ecore-package nsURI-set! "http://www.eclipse.org/emf/2002/Ecore")
(send ecore-package nsPrefix-set! "ecore")
(with-epackage
 ecore-package

 (-eclass
  EObject% EObject-base%)
 (provide EObject%)

 (-eclass
  EModelElement% EObject%)
 (provide EModelElement%)

 (-eclass
  ENamedElement% EModelElement%
  (attribute name 'string 1 1))

 (-eclass
  EClassifier% ENamedElement%
  (reference ePackage EPackage% #f 0 1))

 (define-macro (collect-from-supers all-att-super att)
   (let ([att-value-n (gensym)])
     `(let* ([,att-value-n ,att]
             [the-package (send this ePackage)]
             [direct-superclasses 
              (vector-map (curry dynamic-send the-package 'eClassifiers-get-by-id)
                          -eSuperTypes)]
             [all-superclasses
              (apply vector-append
                     direct-superclasses
                     (vector->list
                      (vector-map
                       (lambda (c) (send c eAllSuperTypes))
                       direct-superclasses)))])
             (apply vector-append
               ,att-value-n
               (vector->list
                (vector-map 
                 (lambda (c) (send c ,all-att-super))
                 all-superclasses))))))
                 
 (-eclass
  EClass% EClassifier%
  (attribute abstract 'boolean 0 1)
  (attribute interface 'boolean 0 1)
  (reference eIDAttribute EAttribute% #f 0 1)
  (reference eOperations EOperation% #t 0 -1)
  (reference eSuperTypes EClass% #f 0 -1)
  (reference eStructuralFeatures EStructuralFeature% #t 0 -1)

  ;; Note: as eAttributes and eReferences (and all the "allXX" references) are derived,
  ;; and so frequently used, so we implement them by hand

  (ref/derived eReferences EReference% #f 0 -1
              (vector-filter (lambda (f) (is-a? f EReference%)) -eStructuralFeatures))
  
  (ref/derived eAllReferences EReference% #f 0 -1
              (collect-from-supers eAllReferences (eReferences)))
  
  (ref/derived eAttributes EAttribute% #f 0 -1
              (vector-filter (lambda (f) (is-a? f EAttribute%)) -eStructuralFeatures))

  (ref/derived eAllAttributes EAttribute% #f 0 -1
              (collect-from-supers eAllAttributes (eAttributes)))

  (ref/derived eAllOperations EOperation% #f 0 -1
              (collect-from-supers eAllOperations -eOperations))

  (ref/derived eAllStructuralFeatures EStructuralFeature% #f 0 -1
              (collect-from-supers eAllStructuralFeatures -eStructuralFeatures))

  (ref/derived eAllSuperTypes EClass% #f 0 -1
              (collect-from-supers eAllSuperTypes -eSuperTypes)))
 (provide EClass%)

 (-eclass
  EPackage-base% ENamedElement%
  (attribute nsURI 'string 0 1)
  (attribute nsPrefix 'string 0 1)
  (reference eSuperPackage EPackage% #f 0 1)
  (reference eClassifiers EClassifier% #t 0 -1)
  (reference eSubpackages EPackage% #t 0 -1))
 (define EPackage%
  (eclassifier-hash-table-mixin% EPackage-base%))
 (provide EPackage%)

 (-eclass
  ETypedElement% ENamedElement%
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
  EStructuralFeature% ETypedElement%
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
(begin-for-syntax

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

  (define (create-attribute-metaclass list)
    (match list
      ((list 'attribute name type minoccur maxoccur)
       `(begin
          (let ((att (new EAttribute%)))
            (send* att
              (name-set! ,(symbol->string name))
              (lowerBound-set! ,minoccur)
              (upperBound-set! ,maxoccur))

            (send the-eclass eStructuralFeatures-append! att))))))

  (define (create-reference-metaclass list)
    (match list
      ((list 'reference name type contained? minoccur maxoccur)
       `(begin
          (let ((ref (new EReference%)))
            (send* ref
              (name-set! ,(symbol->string name))
              (eType-set! ,type)
              (lowerBound-set! ,minoccur)
              (upperBound-set! ,maxoccur))

            (send the-eclass eStructuralFeatures-append! ref))))))

  (define (metaclass-creation body)
    (map (lambda (e)
           (if (pair? e)
               (cond
                 ;; Attribute
                 ((eq? (car e) 'attribute)
                  (create-attribute-metaclass e))
                 ;; Reference
                 ((eq? (car e) 'reference)
                  (create-reference-metaclass e))
                 (else
                  e))
               e))
         body))

  (define (create-metaclass n super ifaces body)
    `(let ((the-eclass (new EClass%)))
       (send* the-eclass
         (name-set! ,(symbol->string n))
         (ePackage-set! the-epackage))

       ,(unless (eq? super 'EObject%)
            `(send the-eclass eSuperTypes-append! ',super))

       ,@(metaclass-creation body)

       (send* the-epackage
         (eClassifiers-append! the-eclass)
         (eClassifiers-table-add! ',n the-eclass))))
)

(define-macro (eclass n super ifaces . body)
  `(begin
     (define ,n
       (class* ,super ,ifaces
         (super-new)

         (inherit-field -eClass)
         (set! -eClass (send the-epackage eClassifiers-get-by-id ',n))

         ,@(expand-eclass-body body)))

     ,(create-metaclass n super ifaces body)))

;; Utility macros
(define-syntax eclass-of
  (syntax-rules ()
    ((_ c)
     (send c eClass))))


(define (eobject->xexpr o nameattr)
  (-eobject->xexpr o null nameattr 0))


;; Keeps track of the different objects used in the serialization/deserialization
(struct xmi-pos (self parent label pos))

(define xmi-object-hash (make-hasheq))

(define (ref-repr xmip)
  (let* ((parent (xmi-pos-parent xmip))
         (parent-string 
          (if (null? parent)
              ""
              (ref-repr (hash-ref xmi-object-hash parent)))))
    (string-append parent-string (format "/@~a.~a" (xmi-pos-label xmip) (xmi-pos-pos xmip)))))

(define (ref->xexpr o refname r)
  (let ((refval (dynamic-send o refname)))
    ;; Mono or multi-valuated?
    (if (= (send r upperBound) 1)
        ;; Mono
        (if (not (null? refval))
            (list (-eobject->xexpr refval o refname 0))
            null)
        ;; Multi
        (let ((counter 0))
          (filter-map (lambda (ref) (begin0
                                      (and (not (null? ref)) 
                                           (-eobject->xexpr ref o refname counter))
                                      (set! counter (+ 1 counter))))
                      (vector->list refval))))))


(define (-eobject->xexpr o parent nameattr n)  

  ;; Insert this object in the hash
  (let ((xmip (xmi-pos o parent nameattr n)))
    (displayln (ref-repr xmip))
    (hash-set! xmi-object-hash o xmip))
   
  (apply
   append
   (list
    nameattr
    
    ;; Attributes
    (map
     (lambda (att) 
       (let ((attname (string->symbol (send att name))))
         (list attname (dynamic-send o attname))))
     (filter (lambda (att)
               (= (send att upperBound) 1))
             (vector->list (send (eclass-of o) eAllAttributes)))))
   
   ;; Multi-valuated attributes
   (filter-map
    (lambda (att) 
      (and (not (= (send att upperBound) 1))
           (let* ((attname (string->symbol (send att name)))
                  (attvalue (vector->list (dynamic-send o attname))))
             (and (not (null? attvalue))
                  (map (lambda (v) 
                         (list attname (list) v)) 
                       attvalue)))))
    (vector->list (send (eclass-of o) eAllAttributes)))
   
   ;; References
   (filter-map
    (lambda (ref)
      (let ((result (ref->xexpr o (string->symbol (send ref name)) ref)))
        (and (not (null? result))
             result)))
    (vector->list (send (eclass-of o) eAllReferences)))))