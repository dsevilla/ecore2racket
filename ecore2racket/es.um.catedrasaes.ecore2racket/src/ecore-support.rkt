#lang racket

;; Minimal interface that resembles ecore metadata
;; As ecore models representational elements are in turn classes
;; and instances, I only include here the static typing information
;; needed to know how to treat each feature of a class (for example,
;; the differentiation between attributes and references even when
;; they're represented by the same abstraction (private field + get &
;; set functions). Sorry for the camel case, but it is easier to
;; match what will appear in the XMI.

(provide (prefix-out ecore: ENamedElement<%>)
         (prefix-out ecore: EClassifier<%>)
         (prefix-out ecore: EStructuralFeature<%>)
         (prefix-out ecore: EReference<%>)
         (prefix-out ecore: EAttribute<%>)
         (prefix-out ecore: EObject<%>)
         eclass
         edatatype
         eenum
         with-epackage
         ecore-package
         ~eclass
         eobject->xexpr
         to-xml
         to-xexpr
         alias-id)

(define-syntax-rule (alias-id from to)
  (define-syntax to
    (syntax-id-rules ()
      [to from])))

;;; Interfaces describing the different Ecore metamodel elements. In a
;;; perfect world, these entities would have been generated with the
;;; same interface all other metamodels have, but we need a bootstrap
;;; process first.
(define EResource<%> (interface () ))
(define EResourceSet<%> (interface () ))
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

(define EResourceSet
  (class* object% (EResourceSet<%>)
    (super-new)))

(define EResource
  (class* object% (EResource<%>)
    (super-new)))

(define EObject-base
  (class* EResource (EObject<%>)

    (super-new)

    ;; Class pointer
    (field [-eClass null])
    (define/public (eClass) -eClass)
    (define/public (eClass-set! klass) (set! -eClass klass))))

;(define (eclassifier-hash-table-mixin% %)
;  (class %
;    (super-new)
;    (field [-eClassifiers-hash-table (make-hash)])
;    (define/public (eClassifiers-table-add! id classifier)
;      (hash-set! -eClassifiers-hash-table id classifier))
;    (define/public (eClassifiers-get-by-id id)
;      (hash-ref -eClassifiers-hash-table id null))))


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
         (for-syntax racket))

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

  (define (default-value type)
    (let ((val (assq type '([EInt 0]
                            [EFloat 0.0]
                            [EByte 0]
                            [EChar #\x0]
                            [EDouble 0.0]
                            [ELong 0]
                            [EString ""]
                            [EBoolean #f]))))
      (if val (cadr val) null)))

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
           (new-field-mono name (default-value type))
           ;; multi-valuated
           (new-field-multi name (default-value type))))))

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
      (with-gensyms (tmp-val-n)
        `(begin
           (field [,field-name null])
           (define ,f-name
             (lambda () ,field-name))
           (public ,f-name)
           (define ,set-name
             (lambda (,tmp-val-n) (set! ,field-name ,tmp-val-n)))
           (public ,set-name)
           (define ,append-name
             (lambda (,tmp-val-n)
               (set! ,field-name
                     (append ,field-name
                             (if (list? ,tmp-val-n)
                                 ,tmp-val-n
                                 (list ,tmp-val-n))))))
           (public ,append-name)))))

  (define (expand-class-reference list)
    (match list
      ((list 'reference name type contained? minoccur maxoccur)
       (if (= maxoccur 1)
           (new-field-mono name)
           ;; multi-valuated
           (new-field-multi name)))))

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

  (define (generate-fast-accessors body)
    (define method-hash (make-hasheq))

    (define (gen-fast-accessors)
      (hash-map
       method-hash
       (lambda (k v)
         (let ((method-name
                (if (eq? (caddr v) 'EBoolean)
                    (append-id "~" k "?")
                    (append-id "~" k))))
           `(begin
              (define-syntax ,method-name
                (syntax-id-rules (,method-name)
                  [(_ o)
                   (send o ,k)]
                  ;; Allow also to be apply-able
                  [,method-name
                   (lambda (o) (send o ,k))]))
              (provide ,method-name))))))

    (define (add-methods-to-hash)
      (for-each
       (lambda (e)
         (when (and (pair? e)
                    (eq? (car e) 'eclass))
           (for-each
            (lambda (e)
              (when (and (pair? e)
                         (memq (car e) '(attribute reference ref/derived)))
                (hash-set! method-hash (cadr e) e)))
            (cdddr e))))
       body))

    (add-methods-to-hash)
    (gen-fast-accessors))

  (define (generate-defines-for-classifiers package-prefix body)
    (filter-map
     (lambda (e)
       (and (pair? e)
            (memq (car e) '(eclass edatatype eenum))
            (let ((class-name (cadr e))
                  (metaclass-name (append-id (cadr e) "-eclass")))
              `(begin
                 (define ,class-name null)
                 (alias-id ,class-name ,(append-id package-prefix ":" class-name))
                 (define ,metaclass-name null)
                 (alias-id ,metaclass-name ,(append-id package-prefix ":" metaclass-name))))))
     body))

  ;; TODO: Generalize this recursive search
  (define (create-package name uri package-prefix body)
    `(begin
       (set!
        the-epackage
        (new
         (class ecore:EPackage
           (super-new)
           ,@(filter-map
              (lambda (e)
                (when (pair? e)
                  (cond
                    [(memq (car e) '(eclass -eclass))
                     (let* ((class-name (cadr e))
                            (metaclass-name (append-id class-name "-eclass"))
                            (method-name (append-id "get-" class-name)))
                       ;; TODO: do the same for attributes and references
                       `(define/public (,method-name) ,metaclass-name))]
                    [(memq (car e) '(edatatype -edatatype))
                     (let* ((class-name (cadr e))
                            (metaclass-name (append-id class-name "-eclass"))
                            (method-name (append-id "get-" class-name)))
                       `(define/public (,method-name) ,metaclass-name))]
                    [else
                     #f])))
              body))))
       (send* the-epackage
         (name-set! ,name)
         (nsURI-set! ,uri)
         (nsPrefix-set! ,(symbol->string package-prefix)))))

    (define (create-attribute-metaclass the-eclass list)
    (match list
      ((list 'attribute name type minoccur maxoccur)
       `(begin
          (let ((att (new ecore:EAttribute)))
            (send* att
              (name-set! ,(symbol->string name))
              (eType-set! ,type)
              (lowerBound-set! ,minoccur)
              (upperBound-set! ,maxoccur))

            (send ,the-eclass eStructuralFeatures-append! att))))))

  (define (create-reference-metaclass the-eclass list)
    (match list
      ((list ref-type name type contained? minoccur maxoccur rest ...)
       `(begin
          (let ((ref (new ecore:EReference)))
            (send* ref
              (name-set! ,(symbol->string name))
              (eType-set! ,type)
              (derived-set! ,(eq? ref-type 'ref/derived))
              (lowerBound-set! ,minoccur)
              (upperBound-set! ,maxoccur))

            (send ,the-eclass eStructuralFeatures-append! ref))))))

  (define (metaclass-body-creation the-eclass body)
    (filter-map
     (lambda (e)
       (when (pair? e)
         (cond
           ;; Attribute
           ((eq? (car e) 'attribute)
            (create-attribute-metaclass the-eclass e))
           ;; Reference
           ((memq (car e) '(reference ref/derived))
            (create-reference-metaclass the-eclass e))
           (else
            #f))))
     body))

  (define (create-metaclass n super body)
    (let ((m-name (append-id n "-eclass"))
          (m-super-name (append-id super "-eclass")))
      `(begin
         (set! ,m-name (new ecore:EClass))
         (send* ,m-name
           (name-set! ,(symbol->string n))
           (ePackage-set! the-epackage))

         ,(unless (or (memq n '(EObject ecore:EObject))
                      (memq super '(EObject ecore:EObject)))
            `(send ,m-name eSuperTypes-append! ,m-super-name))

         ,@(metaclass-body-creation m-name body)

         (send* the-epackage
           (eClassifiers-append! ,m-name))
         ;         (eClassifiers-table-add! ,n the-eclass)
         )))

  (define (create-metaclasses body)
    (filter-map
     (lambda (e)
       (and (pair? e) (eq? (car e) 'eclass)
            (match e
              ((list _ name super body ...)
               (create-metaclass name super body)))))
     body))

  )


;; The edatatype macro proper. Private version to generate Ecore itself.
(define-macro (edatatype n serializable? default-value)
  (let ((dt (gensym))
        (name-symbol (symbol->string n)))
    `(set! ,n
         (let ((,dt (new ecore:EDataType)))
           (send* ,dt
             (name-set! ,name-symbol)
             (serializable-set! ,serializable?)
             (defaultValue-set! ,default-value))

           ;; TODO
           ;;(send the-epackage eClassifiers-append! ,dt)

           ,dt))))


;(define-syntax (with-epackage stx)
;    (syntax-case stx ()
;      ((_ package body ...)
;         #`(begin
;             (define #,(datum->syntax stx 'the-epackage) package)
;             body ...))))



(define-macro (-with-epackage package name uri package-prefix . body)
  `(begin
     (define the-epackage ,package)

     ;; Predefine the classes so that they can self-reference later.
     ,@(generate-defines-for-classifiers package-prefix body)

     ,@body
     ;; We generate here a call to macros so that they are
     ;; at the same level to all the previous calls to
     ;; eclass, so it is run *after* all the classes
     ;; have been created.

     ;; Generate ~xxx methods
     ,@(generate-fast-accessors body)

     ;; Metaclasses for all the classes and datatypes in the package
     ,@(create-metaclasses body)

     ;; Generate the package itself. As all the classes have been defined above,
     ;; we can create a proper package object and populate with all the defined
     ;; classes
     ,(create-package name uri package-prefix body)

     ;; Resolve references for this model. If a symbol naming a EClassifier
     ;; is introduced, search in the package the concrete classifier and reference it
     ;,@(update-references)
     ))

(define-macro (update-references)
  ;; Update all the references in the model
  `(begin
     ;; TODO
     ))

(define-syntax (with-eclass stx)
  (syntax-case stx ()
    ((_ eclass body ...)
     (with-syntax ([the-eclass (datum->syntax stx 'the-eclass)])
       #'(begin
           (define the-eclass eclass)
           body ...)))))

(define-macro (ref/derived name type contained? minoccur maxoccur body)
  `(begin
     (field [,(append-id "-" name) null])
     (define/public (,name)
       ,body)))

(define-macro (with-epackage package name uri package-prefix . body)
  `(begin
     (define the-epackage ,package)

     ;; Predefine the classes so that they can self-reference later.
     ,@(generate-defines-for-classifiers package-prefix body)

     ,@body

     ,@(create-metaclasses body)

     ,(create-package name uri package-prefix body)))

;; The eclass macro proper. Private version to generate Ecore itself.
(define-macro (eclass n super . body)
  `(begin
     (set! ,n
       (class ,super  ;; Ifaces: TODO
         (super-new)

         (inherit-field -eClass)
         (set! -eClass ,(append-id n "-eclass"))

         ,@(expand-eclass-body body)))))

;; Macro for the only subtype of a datatype: EEnum
(define-macro (eenum name . keyval)
  (void))

;; Utility macros
(define-syntax ~eclass
  (syntax-rules ()
    ((_ c)
     (send c eClass))))


;;; Ecore classes
(define ecore-package null)
(-with-epackage
 ecore-package "ecore" "http://www.eclipse.org/emf/2002/Ecore" ecore

 (eclass
  EObject EObject-base)
 (provide ecore:EObject)

 (eclass
  EModelElement EObject)
 (provide ecore:EModelElement)

 (eclass  EAnnotation EModelElement
  (attribute source EString 0 1)
  (reference eModelElement EModelElement #f 0 1)
  (reference details EStringToStringMapEntry #t 0 -1))

 (eclass
  EStringToStringMapEntry EObject
  (attribute key EString 0 1)
  (attribute value EString 0 1))

 (eclass
  ENamedElement EModelElement
  (attribute name EString 1 1))
 (provide ecore:EModelElement)

 (eclass
  EClassifier ENamedElement
  (attribute defaultValue EObject 0 1)
  (reference ePackage EPackage #f 0 1))
 (provide ecore:EClassifier)

 (define-macro (collect-from-supers all-att-super att)
   (let ([att-value-n (gensym)])
     `(let* ([,att-value-n ,att]
             [the-package (send this ePackage)]
             [direct-superclasses -eSuperTypes]
             [all-superclasses
              (set->list
               (list->set
                (apply append
                       direct-superclasses
                       (map (lambda (c) (send c eAllSuperTypes))
                            direct-superclasses))))])
        (set->list
         (list->set
          (apply append
                 ,att-value-n
                 (map (lambda (c) (send c ,all-att-super))
                      all-superclasses)))))))

 (eclass
  EClass EClassifier
  (attribute abstract EBoolean 0 1)
  (attribute interface EBoolean 0 1)
  (reference eIDAttribute EAttribute #f 0 1)
  (reference eOperations EOperation #t 0 -1)
  (reference eSuperTypes EClass #f 0 -1)
  (reference eStructuralFeatures EStructuralFeature #t 0 -1)

  ;; Note: as eAttributes and eReferences (and all the "allXX" references) are derived,
  ;; and so frequently used, so we implement them by hand

  (ref/derived eReferences EReference #f 0 -1
              (filter (lambda (f) (is-a? f EReference)) -eStructuralFeatures))

  (ref/derived eAllReferences EReference #f 0 -1
              (collect-from-supers eAllReferences (eReferences)))

  (ref/derived eAttributes EAttribute #f 0 -1
              (filter (lambda (f) (is-a? f EAttribute)) -eStructuralFeatures))

  (ref/derived eAllAttributes EAttribute #f 0 -1
              (collect-from-supers eAllAttributes (eAttributes)))

  (ref/derived eAllOperations EOperation #f 0 -1
              (collect-from-supers eAllOperations -eOperations))

  (ref/derived eAllStructuralFeatures EStructuralFeature #f 0 -1
              (collect-from-supers eAllStructuralFeatures -eStructuralFeatures))

  (ref/derived eAllSuperTypes EClass #f 0 -1
              (collect-from-supers eAllSuperTypes -eSuperTypes)))
 (provide ecore:EClass)

 (eclass
  EPackage ENamedElement
  (attribute nsURI EString 0 1)
  (attribute nsPrefix EString 0 1)
  (reference eSuperPackage EPackage #f 0 1)
  (reference eClassifiers EClassifier #t 0 -1)
  (reference eSubpackages EPackage #t 0 -1))
 (provide ecore:EPackage)

 (eclass
  ETypedElement ENamedElement
  (attribute ordered EBoolean 0 1)
  (attribute unique EBoolean 0 1)
  (attribute lowerBound EInt 0 1)
  (attribute upperBound EInt 0 1)
  (attribute many EBoolean 0 1)
  (attribute required EBoolean 0 1)
  (reference eType EClassifier #f 0 1))
 (provide ecore:ETypedElement)

 (eclass
  EOperation ETypedElement
  (reference eContainingClass EClass #f 1 1)
  (reference eParameters EParameter #t 0 -1))
 (provide ecore:EOperation)

 (eclass
  EParameter ETypedElement
  (reference eOperation EOperation #f 1 1))
 (provide ecore:EParameter)

 (eclass
  EStructuralFeature ETypedElement
  (attribute changeable EBoolean 0 1)
  (attribute volatile EBoolean 0 1)
  (attribute transient EBoolean 0 1)
  (attribute unsettable EBoolean 0 1)
  (attribute derived EBoolean 0 1)
  (reference eContainingClass EClass #f 0 1))
 (provide ecore:EStructuralFeature)

 (eclass
  EAttribute EStructuralFeature
  (attribute iD EBoolean 0 1)
  (reference eAttributeType EDataType #f 1 1))
 (provide ecore:EAttribute)

 (eclass
  EReference EStructuralFeature
  (attribute containment EBoolean 0 1)
  (attribute container EBoolean 0 1)
  (reference eOpposite EReference #f 0 1)
  (reference eReferenceType EClass #f 1 1))
 (provide ecore:EReference)

 (eclass
  EDataType EClassifier
  (attribute serializable EBoolean 0 1))
 (provide ecore:EDataType)

 (eclass
  EEnumLiteral ENamedElement
  (attribute value EInt 0 1)
  (attribute literal EString 0 1)
  (reference eEnum EEnum #f 0 1))

 (eclass
  EEnum EDataType
  (reference eLiterals EEnumLiteral #t 0 -1))

 ;; Datatypes
 (edatatype EString #t "")
 (provide ecore:EString)
 (edatatype ELong #t 0)
 (provide ecore:ELong)
 (edatatype EInt #t 0)
 (provide ecore:EInt)
 (edatatype EShort #t 0)
 (provide ecore:EShort)
 (edatatype EChar #t #\u0)
 (provide ecore:EChar)
 (edatatype EFloat #t 0.0)
 (provide ecore:EFloat)
 (edatatype EDouble #t 0.0)
 (provide ecore:EDouble)
 (edatatype EBoolean #t #f)
 (provide ecore:EBoolean)

 )

(define (eobject->xexpr o nameattr)
  (-eobject->xexpr o null nameattr 0))


;; Keeps track of the different objects used in the serialization/deserialization
(struct xmi-pos (self parent label pos))

(define xmi-object-hash (make-hasheq))


;; Reference format:
;; URL#/<parent-spec>/<child-spec>
;; parent-spec and child-spec can be either @refname.pos or simply name, where "name" comes from
;; the property "name" of the object at that position.
;; Multiple references are stored as an attribute like this:
;; ref="ref-pointer1 refpointer2"

(define (ref-repr xmip)
  (let ((parent (xmi-pos-parent xmip)))
    (if (null? parent)
        (format "/~a" (xmi-pos-pos xmip))
        (let ((parent-string
               (ref-repr (hash-ref xmi-object-hash parent))))
          (string-append parent-string (format "/@~a.~a"
                                               (xmi-pos-label xmip)
                                               (xmi-pos-pos xmip)))))))

(define (ref->xexpr o refname r)
  (let ((refval (dynamic-send o refname)))
    ;; Mono or multi-valuated?
    (if (= (~upperBound r) 1)
        ;; Mono
        (if (not (null? refval))
            (list (-eobject->xexpr refval o refname 0))
            null)
        ;; Multi
        (let ((counter 0))
          (filter-map (lambda (ref)
                        (begin0
                          (and (not (null? ref))
                               (-eobject->xexpr ref o refname counter))
                          (set! counter (+ 1 counter))))
                      refval)))))


(define (-eobject->xexpr o parent nameattr n)

  ;; Insert this object in the hash
  (let ((xmip (xmi-pos o parent nameattr n)))
    (displayln (ref-repr xmip))
    (hash-set! xmi-object-hash o xmip))

  (apply
   append
   (list
    nameattr

    ;; Attributes (with non-default value) + non-contained references
    (append
     (filter-map
      (lambda (att)
        (and (= (~upperBound att) 1)
             (let* ((attname (string->symbol (~name att)))
                    (value (dynamic-send o attname)))
               (and (not (equal? value (~defaultValue (~eType att))))
                    (list attname value)))))
      (~eAllAttributes (~eclass o))))))

   ;; Multi-valuated attributes
   (filter-map
    (lambda (att)
      (and (not (= (~upperBound att) 1))
           (let* ((attname (string->symbol (~name att)))
                  (attvalue (dynamic-send o attname)))
             (and (not (null? attvalue))
                  (map (lambda (v)
                         (list attname '() v))
                       attvalue)))))
    (~eAllAttributes (~eclass o)))

   ;; References
   (filter-map
    (lambda (ref)
      (let ((result (ref->xexpr o (string->symbol (~name ref)) ref)))
        (and (not (null? result))
             result)))
    (~eAllReferences (~eclass o))))

;; ; Idea
;; ;(struct -ecore# (EClass))
;; ;
;; ;(define ecore (-ecore# (class object% (super-new))))
;; ;
;; ;(define-syntax (ecore# stx)
;; ;  (syntax-case stx ()
;; ;    ((_ c . body)
;; ;     (let ((base #`(#,(string->symbol (format "-ecore#-~a" (syntax->datum #'c))) ecore)))
;; ;       (syntax-case #'body (new)
;; ;         [(new)
;; ;            #`(new #,base)]
;; ;         [_ base])))))

(module+ test
  (require rackunit)

  (check-equal? (sort (map ~name (~eAllAttributes (~eclass (new EClass)))) string<?)
                (sort '("name" "interface" "abstract" "defaultValue") string<?))
)
