#lang racket

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

(define box<%> 
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
;; Class Box (box%)
(define box%/b
  (class* eobject%
   (classifier<%> box<%>)
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

    (define/override (e-all-attributes)
		'( 
		comment
		name
		)	
	)
	(define/override (e-all-references)
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
	(define -comment 
	)
	; attribute name
	(define -name 
	)
	
	; attribute attributes
	(define -attributes 
	)
	; attribute methods
	(define -methods 
	)
	; attribute wires
	(define -wires 
	)
	; attribute boxes
	(define -boxes 
	)
	; attribute connection
	(define -connection 
	)
	

  ))


(define box-reference<%> 
  (interface ()  
	box
	box-set!
	is-pointer
	is-pointer-set!
	is-reference
	is-reference-set!
	name
	name-set!
))
;; Class BoxReference (box-reference%)
(define box-reference%/b
  (class* eobject%
   (classifier<%> box-reference<%>)
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

    (define/override (e-all-attributes)
		'( 
		is-pointer
		is-reference
		name
		)	
	)
	(define/override (e-all-references)
		'( 
		box
		)
	)


    ;; fields (features)
	; attribute is-pointer
	(define -is-pointer 
	)
	; attribute is-reference
	(define -is-reference 
	)
	; attribute name
	(define -name 
	)
	
	; attribute box
	(define -box 
	)
	

  ))


(define wire-reference<%> 
  (interface ()  
	wire
	wire-set!
))
;; Class WireReference (wire-reference%)
(define wire-reference%/b
  (class* eobject%
   (classifier<%> wire-reference<%>)
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

    (define/override (e-all-attributes)
		'( 
		)	
	)
	(define/override (e-all-references)
		'( 
		wire
		)
	)


    ;; fields (features)
	
	; attribute wire
	(define -wire 
	)
	

  ))


(define local-wire-reference<%> 
  (interface (wire-reference<%>
  )  
))
;; Class LocalWireReference (local-wire-reference%)
(define local-wire-reference%/b
  (class* wire-reference%/b
   (classifier<%> local-wire-reference<%>)
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

    (define/override (e-all-attributes)
		'( 
		)	
	)
	(define/override (e-all-references)
		'( 
		wire
		)
	)


    ;; fields (features)
	
	

  ))


(define external-wire-reference<%> 
  (interface (wire-reference<%>
  )  
	box-reference
	box-reference-set!
))
;; Class ExternalWireReference (external-wire-reference%)
(define external-wire-reference%/b
  (class* wire-reference%/b
   (classifier<%> external-wire-reference<%>)
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

    (define/override (e-all-attributes)
		'( 
		)	
	)
	(define/override (e-all-references)
		'( 
		wire
		box-reference
		)
	)


    ;; fields (features)
	
	; attribute box-reference
	(define -box-reference 
	)
	

  ))


(define connection<%> 
  (interface ()  
	from
	from-set!
	to
	to-set!
))
;; Class Connection (connection%)
(define connection%/b
  (class* eobject%
   (classifier<%> connection<%>)
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

    (define/override (e-all-attributes)
		'( 
		)	
	)
	(define/override (e-all-references)
		'( 
		from
		to
		)
	)


    ;; fields (features)
	
	; attribute from
	(define -from 
	)
	; attribute to
	(define -to 
	)
	

  ))


(define wire<%> 
  (interface ()  
	is-input
	is-input-set!
	name
	name-set!
	type
	type-set!
))
;; Class Wire (wire%)
(define wire%/b
  (class* eobject%
   (classifier<%> wire<%>)
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

    (define/override (e-all-attributes)
		'( 
		is-input
		name
		)	
	)
	(define/override (e-all-references)
		'( 
		type
		)
	)


    ;; fields (features)
	; attribute is-input
	(define -is-input 
	)
	; attribute name
	(define -name 
	)
	
	; attribute type
	(define -type 
	)
	

  ))


(define type<%> 
  (interface ()  
	is-pointer
	is-pointer-set!
	is-reference
	is-reference-set!
))
;; Class Type (type%)
(define type%/b
  (class* eobject%
   (classifier<%> type<%>)
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

    (define/override (e-all-attributes)
		'( 
		is-pointer
		is-reference
		)	
	)
	(define/override (e-all-references)
		'( 
		)
	)


    ;; fields (features)
	; attribute is-pointer
	(define -is-pointer 
	)
	; attribute is-reference
	(define -is-reference 
	)
	
	

  ))


(define basic-type<%> 
  (interface (type<%>
  )  
	basic-type
	basic-type-set!
))
;; Class BasicType (basic-type%)
(define basic-type%/b
  (class* type%/b
   (classifier<%> basic-type<%>)
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

    (define/override (e-all-attributes)
		'( 
		is-pointer
		is-reference
		basic-type
		)	
	)
	(define/override (e-all-references)
		'( 
		)
	)


    ;; fields (features)
	; attribute basic-type
	(define -basic-type 
	)
	
	

  ))


(define other-type<%> 
  (interface (type<%>
  )  
	namespace
	namespace-set!
	name
	name-set!
))
;; Class OtherType (other-type%)
(define other-type%/b
  (class* type%/b
   (classifier<%> other-type<%>)
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

    (define/override (e-all-attributes)
		'( 
		is-pointer
		is-reference
		namespace
		name
		)	
	)
	(define/override (e-all-references)
		'( 
		)
	)


    ;; fields (features)
	; attribute namespace
	(define -namespace 
	)
	; attribute name
	(define -name 
	)
	
	

  ))


(define method<%> 
  (interface ()  
	comment
	comment-set!
	is-private
	is-private-set!
	type
	type-set!
	name
	name-set!
	parameters
	parameters-set!
))
;; Class Method (method%)
(define method%/b
  (class* eobject%
   (classifier<%> method<%>)
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

    (define/override (e-all-attributes)
		'( 
		comment
		is-private
		name
		)	
	)
	(define/override (e-all-references)
		'( 
		type
		parameters
		)
	)


    ;; fields (features)
	; attribute comment
	(define -comment 
	)
	; attribute is-private
	(define -is-private 
	)
	; attribute name
	(define -name 
	)
	
	; attribute type
	(define -type 
	)
	; attribute parameters
	(define -parameters 
	)
	

  ))


(define param<%> 
  (interface ()  
	comment
	comment-set!
	type
	type-set!
	name
	name-set!
	default-value
	default-value-set!
))
;; Class Param (param%)
(define param%/b
  (class* eobject%
   (classifier<%> param<%>)
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

    (define/override (e-all-attributes)
		'( 
		comment
		name
		default-value
		)	
	)
	(define/override (e-all-references)
		'( 
		type
		)
	)


    ;; fields (features)
	; attribute comment
	(define -comment 
	)
	; attribute name
	(define -name 
	)
	; attribute default-value
	(define -default-value 
	)
	
	; attribute type
	(define -type 
	)
	

  ))


(define wire-many<%> 
  (interface (wire<%>
  )  
))
;; Class WireMany (wire-many%)
(define wire-many%/b
  (class* wire%/b
   (classifier<%> wire-many<%>)
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

    (define/override (e-all-attributes)
		'( 
		is-input
		name
		)	
	)
	(define/override (e-all-references)
		'( 
		type
		)
	)


    ;; fields (features)
	
	

  ))


(define wire-method<%> 
  (interface (wire<%>
  )  
	method
	method-set!
))
;; Class WireMethod (wire-method%)
(define wire-method%/b
  (class* wire%/b
   (classifier<%> wire-method<%>)
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

    (define/override (e-all-attributes)
		'( 
		is-input
		name
		)	
	)
	(define/override (e-all-references)
		'( 
		type
		method
		)
	)


    ;; fields (features)
	
	; attribute method
	(define -method 
	)
	

  ))



;; Contracts
(define box<%>/c 
	(class/c ));;(amethod (->m x y))))
(define/contract box% box<%>/c box%/b)


(define box-reference<%>/c 
	(class/c ));;(amethod (->m x y))))
(define/contract box-reference% box-reference<%>/c box-reference%/b)


(define wire-reference<%>/c 
	(class/c ));;(amethod (->m x y))))
(define/contract wire-reference% wire-reference<%>/c wire-reference%/b)


(define local-wire-reference<%>/c 
	(class/c ));;(amethod (->m x y))))
(define/contract local-wire-reference% local-wire-reference<%>/c local-wire-reference%/b)


(define external-wire-reference<%>/c 
	(class/c ));;(amethod (->m x y))))
(define/contract external-wire-reference% external-wire-reference<%>/c external-wire-reference%/b)


(define connection<%>/c 
	(class/c ));;(amethod (->m x y))))
(define/contract connection% connection<%>/c connection%/b)


(define wire<%>/c 
	(class/c ));;(amethod (->m x y))))
(define/contract wire% wire<%>/c wire%/b)


(define type<%>/c 
	(class/c ));;(amethod (->m x y))))
(define/contract type% type<%>/c type%/b)


(define basic-type<%>/c 
	(class/c ));;(amethod (->m x y))))
(define/contract basic-type% basic-type<%>/c basic-type%/b)


(define other-type<%>/c 
	(class/c ));;(amethod (->m x y))))
(define/contract other-type% other-type<%>/c other-type%/b)


(define method<%>/c 
	(class/c ));;(amethod (->m x y))))
(define/contract method% method<%>/c method%/b)


(define param<%>/c 
	(class/c ));;(amethod (->m x y))))
(define/contract param% param<%>/c param%/b)


(define wire-many<%>/c 
	(class/c ));;(amethod (->m x y))))
(define/contract wire-many% wire-many<%>/c wire-many%/b)


(define wire-method<%>/c 
	(class/c ));;(amethod (->m x y))))
(define/contract wire-method% wire-method<%>/c wire-method%/b)



;) ; end module box
