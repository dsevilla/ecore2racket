#lang racket/base

; (module box racket/base

  (require racket/class racket/contract)

  ;; Support functions
  (require "../src/ecore-support.rkt")
  (provide (all-from-out "../src/ecore-support.rkt"))

  ;; The package
  (provide box-package)
  (provide
  		Box
  		BoxReference
  		WireReference
  		LocalWireReference
  		ExternalWireReference
  		Connection
  		Wire
  		Type
  		BasicType
  		OtherType
  		Method
  		Param
  		WireMany
  		WireMethod
  	)

(define box-package null)
(with-epackage
 box-package "box" "http://www.catedrasaes.org/Box" box
(define Box<%> 
  (interface (ecore:EObject<%>)  
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
  Box ecore:EObject

    ;; fields (features)
	; attribute comment
	(attribute comment ecore:EString 0 1)
	
	; attribute name
	(attribute name ecore:EString 0 1)
	
	
	; reference attributes
	(reference attributes box:Param #t 0 -1)
	
	; reference methods
	(reference methods box:Method #t 0 -1)
	
	; reference wires
	(reference wires box:Wire #t 0 -1)
	
	; reference boxes
	(reference boxes box:BoxReference #t 0 -1)
	
	; reference connection
	(reference connection box:Connection #t 0 -1)
	
	

  )


(define BoxReference<%> 
  (interface (ecore:EObject<%>)  
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
  BoxReference ecore:EObject

    ;; fields (features)
	; attribute isPointer
	(attribute isPointer ecore:EBoolean 0 1)
	
	; attribute isReference
	(attribute isReference ecore:EBoolean 0 1)
	
	; attribute name
	(attribute name ecore:EString 0 1)
	
	
	; reference box
	(reference box box:Box #f 0 1)
	
	

  )


(define WireReference<%> 
  (interface (ecore:EObject<%>)  
	wire
	wire-set!

))
;; Class WireReference
(eclass 
  WireReference ecore:EObject

    ;; fields (features)
	
	; reference wire
	(reference wire box:Wire #f 0 1)
	
	

  )


(define LocalWireReference<%> 
  (interface (WireReference<%>)  
))
;; Class LocalWireReference
(eclass 
  LocalWireReference WireReference

    ;; fields (features)
	
	

  )


(define ExternalWireReference<%> 
  (interface (WireReference<%>)  
	boxReference
	boxReference-set!

))
;; Class ExternalWireReference
(eclass 
  ExternalWireReference WireReference

    ;; fields (features)
	
	; reference boxReference
	(reference boxReference box:BoxReference #f 0 1)
	
	

  )


(define Connection<%> 
  (interface (ecore:EObject<%>)  
	from
	from-set!

	to
	to-set!

))
;; Class Connection
(eclass 
  Connection ecore:EObject

    ;; fields (features)
	
	; reference from
	(reference from box:WireReference #t 0 1)
	
	; reference to
	(reference to box:WireReference #t 0 1)
	
	

  )


(define Wire<%> 
  (interface (ecore:EObject<%>)  
	isInput
	isInput-set!

	name
	name-set!

	type
	type-set!

))
;; Class Wire
(eclass 
  Wire ecore:EObject

    ;; fields (features)
	; attribute isInput
	(attribute isInput ecore:EBoolean 0 1)
	
	; attribute name
	(attribute name ecore:EString 0 1)
	
	
	; reference type
	(reference type box:Type #t 0 1)
	
	

  )


(define Type<%> 
  (interface (ecore:EObject<%>)  
	isPointer
	isPointer-set!

	isReference
	isReference-set!

))
;; Class Type
(eclass 
  Type ecore:EObject

    ;; fields (features)
	; attribute isPointer
	(attribute isPointer ecore:EBoolean 0 1)
	
	; attribute isReference
	(attribute isReference ecore:EBoolean 0 1)
	
	
	

  )


(define BasicType<%> 
  (interface (Type<%>)  
	basicType
	basicType-set!

))
;; Class BasicType
(eclass 
  BasicType Type

    ;; fields (features)
	; attribute basicType
	(attribute basicType box:BaseTypes 0 1)
	
	
	

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
  OtherType Type

    ;; fields (features)
	; attribute namespace
	(attribute namespace ecore:EString 0 -1)
	
	; attribute name
	(attribute name ecore:EString 0 1)
	
	
	

  )


(define Method<%> 
  (interface (ecore:EObject<%>)  
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
  Method ecore:EObject

    ;; fields (features)
	; attribute comment
	(attribute comment ecore:EString 0 1)
	
	; attribute isPrivate
	(attribute isPrivate ecore:EBoolean 0 1)
	
	; attribute name
	(attribute name ecore:EString 0 1)
	
	
	; reference type
	(reference type box:Type #t 0 1)
	
	; reference parameters
	(reference parameters box:Param #t 0 -1)
	
	

  )


(define Param<%> 
  (interface (ecore:EObject<%>)  
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
  Param ecore:EObject

    ;; fields (features)
	; attribute comment
	(attribute comment ecore:EString 0 1)
	
	; attribute name
	(attribute name ecore:EString 0 1)
	
	; attribute defaultValue
	(attribute defaultValue ecore:EString 0 -1)
	
	
	; reference type
	(reference type box:Type #t 0 1)
	
	

  )


(define WireMany<%> 
  (interface (Wire<%>)  
))
;; Class WireMany
(eclass 
  WireMany Wire

    ;; fields (features)
	
	

  )


(define WireMethod<%> 
  (interface (Wire<%>)  
	method
	method-set!

))
;; Class WireMethod
(eclass 
  WireMethod Wire

    ;; fields (features)
	
	; reference method
	(reference method box:Method #f 0 1)
	
	

  )



(edatatype BaseTypes #t 0 )
 

)

;) ; end module box
