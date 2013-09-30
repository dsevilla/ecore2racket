#lang racket/base

; (module box racket/base

  (require racket/class racket/contract)

  ;; Support functions
  (require "../src/ecore-support.rkt")

  ;; The package
  (provide box-package)
  (provide
  		Box%
  		BoxReference%
  		WireReference%
  		LocalWireReference%
  		ExternalWireReference%
  		Connection%
  		Wire%
  		Type%
  		BasicType%
  		OtherType%
  		Method%
  		Param%
  		WireMany%
  		WireMethod%
  	)

(define box-package (new EPackage%))
(send* box-package 
	(name-set! "box")
	(nsURI-set! "http://www.catedrasaes.org/Box")
    (nsPrefix-set! "box"))
(with-epackage
 box-package
(define Box<%> 
  (interface (EObject<%>)  
	comment
	comment-set!

	name
	name-set!

	attributes
	attributes-set!
    attributes-append!

	methods
	methods-set!
    methods-append!

	wires
	wires-set!
    wires-append!

	boxes
	boxes-set!
    boxes-append!

	connection
	connection-set!
    connection-append!

))
;; Class Box
(eclass 
  Box% EObject% (Box<%>)

    ;; fields (features)
	; attribute comment
	(attribute comment string 0 1)
	
	; attribute name
	(attribute name string 0 1)
	
	
	; reference attributes
	(reference attributes 'Param% #t 0 -1)
	
	; reference methods
	(reference methods 'Method% #t 0 -1)
	
	; reference wires
	(reference wires 'Wire% #t 0 -1)
	
	; reference boxes
	(reference boxes 'BoxReference% #t 0 -1)
	
	; reference connection
	(reference connection 'Connection% #t 0 -1)
	
	

  )


(define BoxReference<%> 
  (interface (EObject<%>)  
	box
	box-set!

	isPointer
	isPointer-set!

	isReference
	isReference-set!

	name
	name-set!

))
;; Class BoxReference
(eclass 
  BoxReference% EObject% (BoxReference<%>)

    ;; fields (features)
	; attribute isPointer
	(attribute isPointer boolean 0 1)
	
	; attribute isReference
	(attribute isReference boolean 0 1)
	
	; attribute name
	(attribute name string 0 1)
	
	
	; reference box
	(reference box 'Box% #f 0 1)
	
	

  )


(define WireReference<%> 
  (interface (EObject<%>)  
	wire
	wire-set!

))
;; Class WireReference
(eclass 
  WireReference% EObject% (WireReference<%>)

    ;; fields (features)
	
	; reference wire
	(reference wire 'Wire% #f 0 1)
	
	

  )


(define LocalWireReference<%> 
  (interface (WireReference<%>)  
))
;; Class LocalWireReference
(eclass 
  LocalWireReference% WireReference% (LocalWireReference<%>)

    ;; fields (features)
	
	

  )


(define ExternalWireReference<%> 
  (interface (WireReference<%>)  
	boxReference
	boxReference-set!

))
;; Class ExternalWireReference
(eclass 
  ExternalWireReference% WireReference% (ExternalWireReference<%>)

    ;; fields (features)
	
	; reference boxReference
	(reference boxReference 'BoxReference% #f 0 1)
	
	

  )


(define Connection<%> 
  (interface (EObject<%>)  
	from
	from-set!

	to
	to-set!

))
;; Class Connection
(eclass 
  Connection% EObject% (Connection<%>)

    ;; fields (features)
	
	; reference from
	(reference from 'WireReference% #t 0 1)
	
	; reference to
	(reference to 'WireReference% #t 0 1)
	
	

  )


(define Wire<%> 
  (interface (EObject<%>)  
	isInput
	isInput-set!

	name
	name-set!

	type
	type-set!

))
;; Class Wire
(eclass 
  Wire% EObject% (Wire<%>)

    ;; fields (features)
	; attribute isInput
	(attribute isInput boolean 0 1)
	
	; attribute name
	(attribute name string 0 1)
	
	
	; reference type
	(reference type 'Type% #t 0 1)
	
	

  )


(define Type<%> 
  (interface (EObject<%>)  
	isPointer
	isPointer-set!

	isReference
	isReference-set!

))
;; Class Type
(eclass 
  Type% EObject% (Type<%>)

    ;; fields (features)
	; attribute isPointer
	(attribute isPointer boolean 0 1)
	
	; attribute isReference
	(attribute isReference boolean 0 1)
	
	
	

  )


(define BasicType<%> 
  (interface (Type<%>)  
	basicType
	basicType-set!

))
;; Class BasicType
(eclass 
  BasicType% Type% (BasicType<%>)

    ;; fields (features)
	; attribute basicType
	(attribute basicType number 0 1)
	
	
	

  )


(define OtherType<%> 
  (interface (Type<%>)  
	namespace
	namespace-set!
    namespace-append!

	name
	name-set!

))
;; Class OtherType
(eclass 
  OtherType% Type% (OtherType<%>)

    ;; fields (features)
	; attribute namespace
	(attribute namespace string 0 -1)
	
	; attribute name
	(attribute name string 0 1)
	
	
	

  )


(define Method<%> 
  (interface (EObject<%>)  
	comment
	comment-set!

	isPrivate
	isPrivate-set!

	type
	type-set!

	name
	name-set!

	parameters
	parameters-set!
    parameters-append!

))
;; Class Method
(eclass 
  Method% EObject% (Method<%>)

    ;; fields (features)
	; attribute comment
	(attribute comment string 0 1)
	
	; attribute isPrivate
	(attribute isPrivate boolean 0 1)
	
	; attribute name
	(attribute name string 0 1)
	
	
	; reference type
	(reference type 'Type% #t 0 1)
	
	; reference parameters
	(reference parameters 'Param% #t 0 -1)
	
	

  )


(define Param<%> 
  (interface (EObject<%>)  
	comment
	comment-set!

	type
	type-set!

	name
	name-set!

	defaultValue
	defaultValue-set!
    defaultValue-append!

))
;; Class Param
(eclass 
  Param% EObject% (Param<%>)

    ;; fields (features)
	; attribute comment
	(attribute comment string 0 1)
	
	; attribute name
	(attribute name string 0 1)
	
	; attribute defaultValue
	(attribute defaultValue string 0 -1)
	
	
	; reference type
	(reference type 'Type% #t 0 1)
	
	

  )


(define WireMany<%> 
  (interface (Wire<%>)  
))
;; Class WireMany
(eclass 
  WireMany% Wire% (WireMany<%>)

    ;; fields (features)
	
	

  )


(define WireMethod<%> 
  (interface (Wire<%>)  
	method
	method-set!

))
;; Class WireMethod
(eclass 
  WireMethod% Wire% (WireMethod<%>)

    ;; fields (features)
	
	; reference method
	(reference method 'Method% #f 0 1)
	
	

  )


)

;) ; end module box
