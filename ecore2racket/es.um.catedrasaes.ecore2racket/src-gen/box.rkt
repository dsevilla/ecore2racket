#lang racket/base

; (module box racket/base

  (require racket/class)

  ;; Support functions
  (require "../src/ecore-support.rkt")

  (provide
  		box%
  		box-reference%
  		wire-reference%
  		local-wire-reference%
  		external-wire-reference%
  		connection%
  		wire%
  		type%
  		basic-type%
  		other-type%
  		method%
  		param%
  		wire-many%
  		wire-method%
  	)

;; Class Box (box%)
(define box%
  (class* eobject%
   (classifier<%>)
    (super-new)

	;; Make it available for introspection (TODO: see a more general method)
	(inspect #f)

	;; classifier& interface methods
    (define/override (e-name)
		"Box")
	(define/override (e-attributes)
		'( 
		comment
		name
		)	
	)
	(define/override (e-references)
		'( 
		attributes
		methods
		wires
		boxes
		connection
		)
	)


    ;; fields (features)
	; attribute comment
	(define -comment 0)
	(define/public (comment)
		-comment)
	(define/public (comment-set! val)
		(set! -comment val))
	
	; attribute name
	(define -name 0)
	(define/public (name)
		-name)
	(define/public (name-set! val)
		(set! -name val))
	
	
	; reference attributes
	(define -attributes null)
	(define/public (attributes)
		-attributes)
	(define/public (attributes-set! val)
		(set! -attributes val))
	
	; reference methods
	(define -methods null)
	(define/public (methods)
		-methods)
	(define/public (methods-set! val)
		(set! -methods val))
	
	; reference wires
	(define -wires null)
	(define/public (wires)
		-wires)
	(define/public (wires-set! val)
		(set! -wires val))
	
	; reference boxes
	(define -boxes null)
	(define/public (boxes)
		-boxes)
	(define/public (boxes-set! val)
		(set! -boxes val))
	
	; reference connection
	(define -connection null)
	(define/public (connection)
		-connection)
	(define/public (connection-set! val)
		(set! -connection val))
	
	

  ))


;; Class BoxReference (box-reference%)
(define box-reference%
  (class* eobject%
   (classifier<%>)
    (super-new)

	;; Make it available for introspection (TODO: see a more general method)
	(inspect #f)

	;; classifier& interface methods
    (define/override (e-name)
		"BoxReference")
	(define/override (e-attributes)
		'( 
		is-pointer
		is-reference
		name
		)	
	)
	(define/override (e-references)
		'( 
		box
		)
	)


    ;; fields (features)
	; attribute is-pointer
	(define -is-pointer 0)
	(define/public (is-pointer)
		-is-pointer)
	(define/public (is-pointer-set! val)
		(set! -is-pointer val))
	
	; attribute is-reference
	(define -is-reference 0)
	(define/public (is-reference)
		-is-reference)
	(define/public (is-reference-set! val)
		(set! -is-reference val))
	
	; attribute name
	(define -name 0)
	(define/public (name)
		-name)
	(define/public (name-set! val)
		(set! -name val))
	
	
	; reference box
	(define -box null)
	(define/public (box)
		-box)
	(define/public (box-set! val)
		(set! -box val))
	
	

  ))


;; Class WireReference (wire-reference%)
(define wire-reference%
  (class* eobject%
   (classifier<%>)
    (super-new)

	;; Make it available for introspection (TODO: see a more general method)
	(inspect #f)

	;; classifier& interface methods
    (define/override (e-name)
		"WireReference")
	(define/override (e-attributes)
		'( 
		)	
	)
	(define/override (e-references)
		'( 
		wire
		)
	)


    ;; fields (features)
	
	; reference wire
	(define -wire null)
	(define/public (wire)
		-wire)
	(define/public (wire-set! val)
		(set! -wire val))
	
	

  ))


;; Class LocalWireReference (local-wire-reference%)
(define local-wire-reference%
  (class* wire-reference%
   (classifier<%>)
    (super-new)

	;; Make it available for introspection (TODO: see a more general method)
	(inspect #f)

	;; classifier& interface methods
    (define/override (e-name)
		"LocalWireReference")
	(define/override (e-attributes)
		'( 
		)	
	)
	(define/override (e-references)
		'( 
		)
	)


    ;; fields (features)
	
	

  ))


;; Class ExternalWireReference (external-wire-reference%)
(define external-wire-reference%
  (class* wire-reference%
   (classifier<%>)
    (super-new)

	;; Make it available for introspection (TODO: see a more general method)
	(inspect #f)

	;; classifier& interface methods
    (define/override (e-name)
		"ExternalWireReference")
	(define/override (e-attributes)
		'( 
		)	
	)
	(define/override (e-references)
		'( 
		box-reference
		)
	)


    ;; fields (features)
	
	; reference box-reference
	(define -box-reference null)
	(define/public (box-reference)
		-box-reference)
	(define/public (box-reference-set! val)
		(set! -box-reference val))
	
	

  ))


;; Class Connection (connection%)
(define connection%
  (class* eobject%
   (classifier<%>)
    (super-new)

	;; Make it available for introspection (TODO: see a more general method)
	(inspect #f)

	;; classifier& interface methods
    (define/override (e-name)
		"Connection")
	(define/override (e-attributes)
		'( 
		)	
	)
	(define/override (e-references)
		'( 
		from
		to
		)
	)


    ;; fields (features)
	
	; reference from
	(define -from null)
	(define/public (from)
		-from)
	(define/public (from-set! val)
		(set! -from val))
	
	; reference to
	(define -to null)
	(define/public (to)
		-to)
	(define/public (to-set! val)
		(set! -to val))
	
	

  ))


;; Class Wire (wire%)
(define wire%
  (class* eobject%
   (classifier<%>)
    (super-new)

	;; Make it available for introspection (TODO: see a more general method)
	(inspect #f)

	;; classifier& interface methods
    (define/override (e-name)
		"Wire")
	(define/override (e-attributes)
		'( 
		is-input
		name
		)	
	)
	(define/override (e-references)
		'( 
		type
		)
	)


    ;; fields (features)
	; attribute is-input
	(define -is-input 0)
	(define/public (is-input)
		-is-input)
	(define/public (is-input-set! val)
		(set! -is-input val))
	
	; attribute name
	(define -name 0)
	(define/public (name)
		-name)
	(define/public (name-set! val)
		(set! -name val))
	
	
	; reference type
	(define -type null)
	(define/public (type)
		-type)
	(define/public (type-set! val)
		(set! -type val))
	
	

  ))


;; Class Type (type%)
(define type%
  (class* eobject%
   (classifier<%>)
    (super-new)

	;; Make it available for introspection (TODO: see a more general method)
	(inspect #f)

	;; classifier& interface methods
    (define/override (e-name)
		"Type")
	(define/override (e-attributes)
		'( 
		is-pointer
		is-reference
		)	
	)
	(define/override (e-references)
		'( 
		)
	)


    ;; fields (features)
	; attribute is-pointer
	(define -is-pointer 0)
	(define/public (is-pointer)
		-is-pointer)
	(define/public (is-pointer-set! val)
		(set! -is-pointer val))
	
	; attribute is-reference
	(define -is-reference 0)
	(define/public (is-reference)
		-is-reference)
	(define/public (is-reference-set! val)
		(set! -is-reference val))
	
	
	

  ))


;; Class BasicType (basic-type%)
(define basic-type%
  (class* type%
   (classifier<%>)
    (super-new)

	;; Make it available for introspection (TODO: see a more general method)
	(inspect #f)

	;; classifier& interface methods
    (define/override (e-name)
		"BasicType")
	(define/override (e-attributes)
		'( 
		basic-type
		)	
	)
	(define/override (e-references)
		'( 
		)
	)


    ;; fields (features)
	; attribute basic-type
	(define -basic-type 0)
	(define/public (basic-type)
		-basic-type)
	(define/public (basic-type-set! val)
		(set! -basic-type val))
	
	
	

  ))


;; Class OtherType (other-type%)
(define other-type%
  (class* type%
   (classifier<%>)
    (super-new)

	;; Make it available for introspection (TODO: see a more general method)
	(inspect #f)

	;; classifier& interface methods
    (define/override (e-name)
		"OtherType")
	(define/override (e-attributes)
		'( 
		namespace
		name
		)	
	)
	(define/override (e-references)
		'( 
		)
	)


    ;; fields (features)
	; attribute namespace
	(define -namespace 0)
	(define/public (namespace)
		-namespace)
	(define/public (namespace-set! val)
		(set! -namespace val))
	
	; attribute name
	(define -name 0)
	(define/public (name)
		-name)
	(define/public (name-set! val)
		(set! -name val))
	
	
	

  ))


;; Class Method (method%)
(define method%
  (class* eobject%
   (classifier<%>)
    (super-new)

	;; Make it available for introspection (TODO: see a more general method)
	(inspect #f)

	;; classifier& interface methods
    (define/override (e-name)
		"Method")
	(define/override (e-attributes)
		'( 
		comment
		is-private
		name
		)	
	)
	(define/override (e-references)
		'( 
		type
		parameters
		)
	)


    ;; fields (features)
	; attribute comment
	(define -comment 0)
	(define/public (comment)
		-comment)
	(define/public (comment-set! val)
		(set! -comment val))
	
	; attribute is-private
	(define -is-private 0)
	(define/public (is-private)
		-is-private)
	(define/public (is-private-set! val)
		(set! -is-private val))
	
	; attribute name
	(define -name 0)
	(define/public (name)
		-name)
	(define/public (name-set! val)
		(set! -name val))
	
	
	; reference type
	(define -type null)
	(define/public (type)
		-type)
	(define/public (type-set! val)
		(set! -type val))
	
	; reference parameters
	(define -parameters null)
	(define/public (parameters)
		-parameters)
	(define/public (parameters-set! val)
		(set! -parameters val))
	
	

  ))


;; Class Param (param%)
(define param%
  (class* eobject%
   (classifier<%>)
    (super-new)

	;; Make it available for introspection (TODO: see a more general method)
	(inspect #f)

	;; classifier& interface methods
    (define/override (e-name)
		"Param")
	(define/override (e-attributes)
		'( 
		comment
		name
		default-value
		)	
	)
	(define/override (e-references)
		'( 
		type
		)
	)


    ;; fields (features)
	; attribute comment
	(define -comment 0)
	(define/public (comment)
		-comment)
	(define/public (comment-set! val)
		(set! -comment val))
	
	; attribute name
	(define -name 0)
	(define/public (name)
		-name)
	(define/public (name-set! val)
		(set! -name val))
	
	; attribute default-value
	(define -default-value 0)
	(define/public (default-value)
		-default-value)
	(define/public (default-value-set! val)
		(set! -default-value val))
	
	
	; reference type
	(define -type null)
	(define/public (type)
		-type)
	(define/public (type-set! val)
		(set! -type val))
	
	

  ))


;; Class WireMany (wire-many%)
(define wire-many%
  (class* wire%
   (classifier<%>)
    (super-new)

	;; Make it available for introspection (TODO: see a more general method)
	(inspect #f)

	;; classifier& interface methods
    (define/override (e-name)
		"WireMany")
	(define/override (e-attributes)
		'( 
		)	
	)
	(define/override (e-references)
		'( 
		)
	)


    ;; fields (features)
	
	

  ))


;; Class WireMethod (wire-method%)
(define wire-method%
  (class* wire%
   (classifier<%>)
    (super-new)

	;; Make it available for introspection (TODO: see a more general method)
	(inspect #f)

	;; classifier& interface methods
    (define/override (e-name)
		"WireMethod")
	(define/override (e-attributes)
		'( 
		)	
	)
	(define/override (e-references)
		'( 
		method
		)
	)


    ;; fields (features)
	
	; reference method
	(define -method null)
	(define/public (method)
		-method)
	(define/public (method-set! val)
		(set! -method val))
	
	

  ))


;) ; end module box
