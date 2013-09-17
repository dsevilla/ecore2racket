#lang racket/base

; (module box racket/base

  (require racket/class racket/contract)

  ;; Support functions
  (require "../src/ecore-support.rkt")

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

(define Box<%> 
  (interface ()  
	comment
	comment-set!
	name
	name-set!
	attributes
	attributes-set!
	methods
	methods-set!
	wires
	wires-set!
	boxes
	boxes-set!
	connection
	connection-set!
))
;; Class Box
(eclass 
  Box% eobject%
   (EClassifier<%> Box<%>)

    ;; fields (features)
	; attribute comment
	(attribute comment EString 0 1)
	
	; attribute name
	(attribute name EString 0 1)
	
	
	; reference attributes
	(reference attributes Param #t 0 -1)
	
	; reference methods
	(reference methods Method #t 0 -1)
	
	; reference wires
	(reference wires Wire #t 0 -1)
	
	; reference boxes
	(reference boxes BoxReference #t 0 -1)
	
	; reference connection
	(reference connection Connection #t 0 -1)
	
	

  ))


(define BoxReference<%> 
  (interface ()  
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
  BoxReference% eobject%
   (EClassifier<%> BoxReference<%>)

    ;; fields (features)
	; attribute isPointer
	(attribute isPointer EBoolean 0 1)
	
	; attribute isReference
	(attribute isReference EBoolean 0 1)
	
	; attribute name
	(attribute name EString 0 1)
	
	
	; reference box
	(reference box Box #f 0 1)
	
	

  ))


(define WireReference<%> 
  (interface ()  
	wire
	wire-set!
))
;; Class WireReference
(eclass 
  WireReference% eobject%
   (EClassifier<%> WireReference<%>)

    ;; fields (features)
	
	; reference wire
	(reference wire Wire #f 0 1)
	
	

  ))


(define LocalWireReference<%> 
  (interface (WireReference<%>
  )  
))
;; Class LocalWireReference
(eclass 
  LocalWireReference% WireReference%/b
   (EClassifier<%> LocalWireReference<%>)

    ;; fields (features)
	
	

  ))


(define ExternalWireReference<%> 
  (interface (WireReference<%>
  )  
	boxReference
	boxReference-set!
))
;; Class ExternalWireReference
(eclass 
  ExternalWireReference% WireReference%/b
   (EClassifier<%> ExternalWireReference<%>)

    ;; fields (features)
	
	; reference boxReference
	(reference boxReference BoxReference #f 0 1)
	
	

  ))


(define Connection<%> 
  (interface ()  
	from
	from-set!
	to
	to-set!
))
;; Class Connection
(eclass 
  Connection% eobject%
   (EClassifier<%> Connection<%>)

    ;; fields (features)
	
	; reference from
	(reference from WireReference #t 0 1)
	
	; reference to
	(reference to WireReference #t 0 1)
	
	

  ))


(define Wire<%> 
  (interface ()  
	isInput
	isInput-set!
	name
	name-set!
	type
	type-set!
))
;; Class Wire
(eclass 
  Wire% eobject%
   (EClassifier<%> Wire<%>)

    ;; fields (features)
	; attribute isInput
	(attribute isInput EBoolean 0 1)
	
	; attribute name
	(attribute name EString 0 1)
	
	
	; reference type
	(reference type Type #t 0 1)
	
	

  ))


(define Type<%> 
  (interface ()  
	isPointer
	isPointer-set!
	isReference
	isReference-set!
))
;; Class Type
(eclass 
  Type% eobject%
   (EClassifier<%> Type<%>)

    ;; fields (features)
	; attribute isPointer
	(attribute isPointer EBoolean 0 1)
	
	; attribute isReference
	(attribute isReference EBoolean 0 1)
	
	
	

  ))


(define BasicType<%> 
  (interface (Type<%>
  )  
	basicType
	basicType-set!
))
;; Class BasicType
(eclass 
  BasicType% Type%/b
   (EClassifier<%> BasicType<%>)

    ;; fields (features)
	; attribute basicType
	(attribute basicType BaseTypes 0 1)
	
	
	

  ))


(define OtherType<%> 
  (interface (Type<%>
  )  
	namespace
	namespace-set!
	name
	name-set!
))
;; Class OtherType
(eclass 
  OtherType% Type%/b
   (EClassifier<%> OtherType<%>)

    ;; fields (features)
	; attribute namespace
	(attribute namespace EString 0 -1)
	
	; attribute name
	(attribute name EString 0 1)
	
	
	

  ))


(define Method<%> 
  (interface ()  
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
))
;; Class Method
(eclass 
  Method% eobject%
   (EClassifier<%> Method<%>)

    ;; fields (features)
	; attribute comment
	(attribute comment EString 0 1)
	
	; attribute isPrivate
	(attribute isPrivate EBoolean 0 1)
	
	; attribute name
	(attribute name EString 0 1)
	
	
	; reference type
	(reference type Type #t 0 1)
	
	; reference parameters
	(reference parameters Param #t 0 -1)
	
	

  ))


(define Param<%> 
  (interface ()  
	comment
	comment-set!
	type
	type-set!
	name
	name-set!
	defaultValue
	defaultValue-set!
))
;; Class Param
(eclass 
  Param% eobject%
   (EClassifier<%> Param<%>)

    ;; fields (features)
	; attribute comment
	(attribute comment EString 0 1)
	
	; attribute name
	(attribute name EString 0 1)
	
	; attribute defaultValue
	(attribute defaultValue EString 0 -1)
	
	
	; reference type
	(reference type Type #t 0 1)
	
	

  ))


(define WireMany<%> 
  (interface (Wire<%>
  )  
))
;; Class WireMany
(eclass 
  WireMany% Wire%/b
   (EClassifier<%> WireMany<%>)

    ;; fields (features)
	
	

  ))


(define WireMethod<%> 
  (interface (Wire<%>
  )  
	method
	method-set!
))
;; Class WireMethod
(eclass 
  WireMethod% Wire%/b
   (EClassifier<%> WireMethod<%>)

    ;; fields (features)
	
	; reference method
	(reference method Method #f 0 1)
	
	

  ))




;) ; end module box
