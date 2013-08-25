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

(define box<%> )
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


(define box-reference<%> )
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


(define wire-reference<%> )
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


    ;; fields (features)
	
	; reference wire
	(define -wire null)
	(define/public (wire)
		-wire)
	(define/public (wire-set! val)
		(set! -wire val))
	
	

  ))


(define local-wire-reference<%> )
;; Class LocalWireReference (local-wire-reference%)
(define local-wire-reference%/b
  (class* wire-reference%
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


    ;; fields (features)
	
	

  ))


(define external-wire-reference<%> )
;; Class ExternalWireReference (external-wire-reference%)
(define external-wire-reference%/b
  (class* wire-reference%
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


    ;; fields (features)
	
	; reference box-reference
	(define -box-reference null)
	(define/public (box-reference)
		-box-reference)
	(define/public (box-reference-set! val)
		(set! -box-reference val))
	
	

  ))


(define connection<%> )
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


(define wire<%> )
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


(define type<%> )
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


(define basic-type<%> )
;; Class BasicType (basic-type%)
(define basic-type%/b
  (class* type%
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


    ;; fields (features)
	; attribute basic-type
	(define -basic-type 0)
	(define/public (basic-type)
		-basic-type)
	(define/public (basic-type-set! val)
		(set! -basic-type val))
	
	
	

  ))


(define other-type<%> )
;; Class OtherType (other-type%)
(define other-type%/b
  (class* type%
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


(define method<%> )
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


(define param<%> )
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


(define wire-many<%> )
;; Class WireMany (wire-many%)
(define wire-many%/b
  (class* wire%
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


    ;; fields (features)
	
	

  ))


(define wire-method<%> )
;; Class WireMethod (wire-method%)
(define wire-method%/b
  (class* wire%
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


    ;; fields (features)
	
	; reference method
	(define -method null)
	(define/public (method)
		-method)
	(define/public (method-set! val)
		(set! -method val))
	
	

  ))



;; Contracts
(define box<%>/c 
	(class/c (afield (->m x y))))
(define/contract box% box<%>/c box%/b)


(define box-reference<%>/c 
	(class/c (afield (->m x y))))
(define/contract box-reference% box-reference<%>/c box-reference%/b)


(define wire-reference<%>/c 
	(class/c (afield (->m x y))))
(define/contract wire-reference% wire-reference<%>/c wire-reference%/b)


(define local-wire-reference<%>/c 
	(class/c (afield (->m x y))))
(define/contract local-wire-reference% local-wire-reference<%>/c local-wire-reference%/b)


(define external-wire-reference<%>/c 
	(class/c (afield (->m x y))))
(define/contract external-wire-reference% external-wire-reference<%>/c external-wire-reference%/b)


(define connection<%>/c 
	(class/c (afield (->m x y))))
(define/contract connection% connection<%>/c connection%/b)


(define wire<%>/c 
	(class/c (afield (->m x y))))
(define/contract wire% wire<%>/c wire%/b)


(define type<%>/c 
	(class/c (afield (->m x y))))
(define/contract type% type<%>/c type%/b)


(define basic-type<%>/c 
	(class/c (afield (->m x y))))
(define/contract basic-type% basic-type<%>/c basic-type%/b)


(define other-type<%>/c 
	(class/c (afield (->m x y))))
(define/contract other-type% other-type<%>/c other-type%/b)


(define method<%>/c 
	(class/c (afield (->m x y))))
(define/contract method% method<%>/c method%/b)


(define param<%>/c 
	(class/c (afield (->m x y))))
(define/contract param% param<%>/c param%/b)


(define wire-many<%>/c 
	(class/c (afield (->m x y))))
(define/contract wire-many% wire-many<%>/c wire-many%/b)


(define wire-method<%>/c 
	(class/c (afield (->m x y))))
(define/contract wire-method% wire-method<%>/c wire-method%/b)



;) ; end module box
