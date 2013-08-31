#lang racket/base

; (module box racket/base

  (require racket/class racket/contract)

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
	; Structural Feature comment
	(define -comment 
	  ""
	  
	  
	)
	(define/public (comment)
	    -comment)
	(define/public (comment-set! val)
	    (set! -comment val))
	; Structural Feature name
	(define -name 
	  ""
	  
	  
	)
	(define/public (name)
	    -name)
	(define/public (name-set! val)
	    (set! -name val))
	
	; Structural Feature attributes
	(define -attributes 
	  (make-vector 0)
	  
	)
	(define/public (attributes (pos #f))
	   (if (not pos)
	    -attributes
	    (begin
	    (when (<= (vector-length -attributes) pos)
	     (let ((new-vector (make-vector (+ 1 pos))))
	       ;; grow the vector
	       (vector-copy! new-vector 0 -attributes)
	       (set! -attributes new-vector)))
	    (vector-ref -attributes pos))))
	(define/public (attributes-set! val (pos #f))
	  (if (not pos)
	   (set! -attributes val)
	   (begin
	    (when (<= (vector-length -attributes) pos)
	     (let ((new-vector (make-vector (+ 1 pos))))
	       ;; grow the vector
	       (vector-copy! new-vector 0 -attributes)
	       (set! -attributes new-vector)))
	    (vector-set! -attributes pos val))))
	; Structural Feature methods
	(define -methods 
	  (make-vector 0)
	  
	)
	(define/public (methods (pos #f))
	   (if (not pos)
	    -methods
	    (begin
	    (when (<= (vector-length -methods) pos)
	     (let ((new-vector (make-vector (+ 1 pos))))
	       ;; grow the vector
	       (vector-copy! new-vector 0 -methods)
	       (set! -methods new-vector)))
	    (vector-ref -methods pos))))
	(define/public (methods-set! val (pos #f))
	  (if (not pos)
	   (set! -methods val)
	   (begin
	    (when (<= (vector-length -methods) pos)
	     (let ((new-vector (make-vector (+ 1 pos))))
	       ;; grow the vector
	       (vector-copy! new-vector 0 -methods)
	       (set! -methods new-vector)))
	    (vector-set! -methods pos val))))
	; Structural Feature wires
	(define -wires 
	  (make-vector 0)
	  
	)
	(define/public (wires (pos #f))
	   (if (not pos)
	    -wires
	    (begin
	    (when (<= (vector-length -wires) pos)
	     (let ((new-vector (make-vector (+ 1 pos))))
	       ;; grow the vector
	       (vector-copy! new-vector 0 -wires)
	       (set! -wires new-vector)))
	    (vector-ref -wires pos))))
	(define/public (wires-set! val (pos #f))
	  (if (not pos)
	   (set! -wires val)
	   (begin
	    (when (<= (vector-length -wires) pos)
	     (let ((new-vector (make-vector (+ 1 pos))))
	       ;; grow the vector
	       (vector-copy! new-vector 0 -wires)
	       (set! -wires new-vector)))
	    (vector-set! -wires pos val))))
	; Structural Feature boxes
	(define -boxes 
	  (make-vector 0)
	  
	)
	(define/public (boxes (pos #f))
	   (if (not pos)
	    -boxes
	    (begin
	    (when (<= (vector-length -boxes) pos)
	     (let ((new-vector (make-vector (+ 1 pos))))
	       ;; grow the vector
	       (vector-copy! new-vector 0 -boxes)
	       (set! -boxes new-vector)))
	    (vector-ref -boxes pos))))
	(define/public (boxes-set! val (pos #f))
	  (if (not pos)
	   (set! -boxes val)
	   (begin
	    (when (<= (vector-length -boxes) pos)
	     (let ((new-vector (make-vector (+ 1 pos))))
	       ;; grow the vector
	       (vector-copy! new-vector 0 -boxes)
	       (set! -boxes new-vector)))
	    (vector-set! -boxes pos val))))
	; Structural Feature connection
	(define -connection 
	  (make-vector 0)
	  
	)
	(define/public (connection (pos #f))
	   (if (not pos)
	    -connection
	    (begin
	    (when (<= (vector-length -connection) pos)
	     (let ((new-vector (make-vector (+ 1 pos))))
	       ;; grow the vector
	       (vector-copy! new-vector 0 -connection)
	       (set! -connection new-vector)))
	    (vector-ref -connection pos))))
	(define/public (connection-set! val (pos #f))
	  (if (not pos)
	   (set! -connection val)
	   (begin
	    (when (<= (vector-length -connection) pos)
	     (let ((new-vector (make-vector (+ 1 pos))))
	       ;; grow the vector
	       (vector-copy! new-vector 0 -connection)
	       (set! -connection new-vector)))
	    (vector-set! -connection pos val))))
	

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
	; Structural Feature is-pointer
	(define -is-pointer 
	  #f
	  
	  
	)
	(define/public (is-pointer)
	    -is-pointer)
	(define/public (is-pointer-set! val)
	    (set! -is-pointer val))
	; Structural Feature is-reference
	(define -is-reference 
	  #f
	  
	  
	)
	(define/public (is-reference)
	    -is-reference)
	(define/public (is-reference-set! val)
	    (set! -is-reference val))
	; Structural Feature name
	(define -name 
	  ""
	  
	  
	)
	(define/public (name)
	    -name)
	(define/public (name-set! val)
	    (set! -name val))
	
	; Structural Feature box
	(define -box 
	  null
	  
	  
	)
	(define/public (box)
	    -box)
	(define/public (box-set! val)
	    (set! -box val))
	

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
	
	; Structural Feature wire
	(define -wire 
	  null
	  
	  
	)
	(define/public (wire)
	    -wire)
	(define/public (wire-set! val)
	    (set! -wire val))
	

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
	
	; Structural Feature box-reference
	(define -box-reference 
	  null
	  
	  
	)
	(define/public (box-reference)
	    -box-reference)
	(define/public (box-reference-set! val)
	    (set! -box-reference val))
	

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
	
	; Structural Feature from
	(define -from 
	  null
	  
	  
	)
	(define/public (from)
	    -from)
	(define/public (from-set! val)
	    (set! -from val))
	; Structural Feature to
	(define -to 
	  null
	  
	  
	)
	(define/public (to)
	    -to)
	(define/public (to-set! val)
	    (set! -to val))
	

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
	; Structural Feature is-input
	(define -is-input 
	  #f
	  
	  
	)
	(define/public (is-input)
	    -is-input)
	(define/public (is-input-set! val)
	    (set! -is-input val))
	; Structural Feature name
	(define -name 
	  ""
	  
	  
	)
	(define/public (name)
	    -name)
	(define/public (name-set! val)
	    (set! -name val))
	
	; Structural Feature type
	(define -type 
	  null
	  
	  
	)
	(define/public (type)
	    -type)
	(define/public (type-set! val)
	    (set! -type val))
	

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
	; Structural Feature is-pointer
	(define -is-pointer 
	  #f
	  
	  
	)
	(define/public (is-pointer)
	    -is-pointer)
	(define/public (is-pointer-set! val)
	    (set! -is-pointer val))
	; Structural Feature is-reference
	(define -is-reference 
	  #f
	  
	  
	)
	(define/public (is-reference)
	    -is-reference)
	(define/public (is-reference-set! val)
	    (set! -is-reference val))
	
	

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
	; Structural Feature basic-type
	(define -basic-type 
	  0
	  
	  
	)
	(define/public (basic-type)
	    -basic-type)
	(define/public (basic-type-set! val)
	    (set! -basic-type val))
	
	

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
	; Structural Feature namespace
	(define -namespace 
	  (make-vector 0)
	  
	)
	(define/public (namespace (pos #f))
	   (if (not pos)
	    -namespace
	    (begin
	    (when (<= (vector-length -namespace) pos)
	     (let ((new-vector (make-vector (+ 1 pos))))
	       ;; grow the vector
	       (vector-copy! new-vector 0 -namespace)
	       (set! -namespace new-vector)))
	    (vector-ref -namespace pos))))
	(define/public (namespace-set! val (pos #f))
	  (if (not pos)
	   (set! -namespace val)
	   (begin
	    (when (<= (vector-length -namespace) pos)
	     (let ((new-vector (make-vector (+ 1 pos))))
	       ;; grow the vector
	       (vector-copy! new-vector 0 -namespace)
	       (set! -namespace new-vector)))
	    (vector-set! -namespace pos val))))
	; Structural Feature name
	(define -name 
	  ""
	  
	  
	)
	(define/public (name)
	    -name)
	(define/public (name-set! val)
	    (set! -name val))
	
	

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
	; Structural Feature comment
	(define -comment 
	  ""
	  
	  
	)
	(define/public (comment)
	    -comment)
	(define/public (comment-set! val)
	    (set! -comment val))
	; Structural Feature is-private
	(define -is-private 
	  #f
	  
	  
	)
	(define/public (is-private)
	    -is-private)
	(define/public (is-private-set! val)
	    (set! -is-private val))
	; Structural Feature name
	(define -name 
	  ""
	  
	  
	)
	(define/public (name)
	    -name)
	(define/public (name-set! val)
	    (set! -name val))
	
	; Structural Feature type
	(define -type 
	  null
	  
	  
	)
	(define/public (type)
	    -type)
	(define/public (type-set! val)
	    (set! -type val))
	; Structural Feature parameters
	(define -parameters 
	  (make-vector 0)
	  
	)
	(define/public (parameters (pos #f))
	   (if (not pos)
	    -parameters
	    (begin
	    (when (<= (vector-length -parameters) pos)
	     (let ((new-vector (make-vector (+ 1 pos))))
	       ;; grow the vector
	       (vector-copy! new-vector 0 -parameters)
	       (set! -parameters new-vector)))
	    (vector-ref -parameters pos))))
	(define/public (parameters-set! val (pos #f))
	  (if (not pos)
	   (set! -parameters val)
	   (begin
	    (when (<= (vector-length -parameters) pos)
	     (let ((new-vector (make-vector (+ 1 pos))))
	       ;; grow the vector
	       (vector-copy! new-vector 0 -parameters)
	       (set! -parameters new-vector)))
	    (vector-set! -parameters pos val))))
	

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
	; Structural Feature comment
	(define -comment 
	  ""
	  
	  
	)
	(define/public (comment)
	    -comment)
	(define/public (comment-set! val)
	    (set! -comment val))
	; Structural Feature name
	(define -name 
	  ""
	  
	  
	)
	(define/public (name)
	    -name)
	(define/public (name-set! val)
	    (set! -name val))
	; Structural Feature default-value
	(define -default-value 
	  (make-vector 0)
	  
	)
	(define/public (default-value (pos #f))
	   (if (not pos)
	    -default-value
	    (begin
	    (when (<= (vector-length -default-value) pos)
	     (let ((new-vector (make-vector (+ 1 pos))))
	       ;; grow the vector
	       (vector-copy! new-vector 0 -default-value)
	       (set! -default-value new-vector)))
	    (vector-ref -default-value pos))))
	(define/public (default-value-set! val (pos #f))
	  (if (not pos)
	   (set! -default-value val)
	   (begin
	    (when (<= (vector-length -default-value) pos)
	     (let ((new-vector (make-vector (+ 1 pos))))
	       ;; grow the vector
	       (vector-copy! new-vector 0 -default-value)
	       (set! -default-value new-vector)))
	    (vector-set! -default-value pos val))))
	
	; Structural Feature type
	(define -type 
	  null
	  
	  
	)
	(define/public (type)
	    -type)
	(define/public (type-set! val)
	    (set! -type val))
	

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
	
	; Structural Feature method
	(define -method 
	  null
	  
	  
	)
	(define/public (method)
	    -method)
	(define/public (method-set! val)
	    (set! -method val))
	

  ))



;; Contracts
(define box<%>/c 
	(class/c 
            (comment (->m string?
            ))
            (comment-set! (->m string?
             any))
            
            (name (->m string?
            ))
            (name-set! (->m string?
             any))
            
            
            
            
            
            
            
            
            
            
            
        
    ))
(define/contract box% box<%>/c box%/b)


(define box-reference<%>/c 
	(class/c 
            
            
            (is-pointer (->m boolean?
            ))
            (is-pointer-set! (->m boolean?
             any))
            
            (is-reference (->m boolean?
            ))
            (is-reference-set! (->m boolean?
             any))
            
            (name (->m string?
            ))
            (name-set! (->m string?
             any))
            
        
    ))
(define/contract box-reference% box-reference<%>/c box-reference%/b)


(define wire-reference<%>/c 
	(class/c 
            
            
        
    ))
(define/contract wire-reference% wire-reference<%>/c wire-reference%/b)


(define local-wire-reference<%>/c 
	(class/c 
        
    ))
(define/contract local-wire-reference% local-wire-reference<%>/c local-wire-reference%/b)


(define external-wire-reference<%>/c 
	(class/c 
            
            
        
    ))
(define/contract external-wire-reference% external-wire-reference<%>/c external-wire-reference%/b)


(define connection<%>/c 
	(class/c 
            
            
            
            
        
    ))
(define/contract connection% connection<%>/c connection%/b)


(define wire<%>/c 
	(class/c 
            (is-input (->m boolean?
            ))
            (is-input-set! (->m boolean?
             any))
            
            (name (->m string?
            ))
            (name-set! (->m string?
             any))
            
            
            
        
    ))
(define/contract wire% wire<%>/c wire%/b)


(define type<%>/c 
	(class/c 
            (is-pointer (->m boolean?
            ))
            (is-pointer-set! (->m boolean?
             any))
            
            (is-reference (->m boolean?
            ))
            (is-reference-set! (->m boolean?
             any))
            
        
    ))
(define/contract type% type<%>/c type%/b)


(define basic-type<%>/c 
	(class/c 
            (basic-type (->m number?
            ))
            (basic-type-set! (->m number?
             any))
            
        
    ))
(define/contract basic-type% basic-type<%>/c basic-type%/b)


(define other-type<%>/c 
	(class/c 
            (namespace (->dm () ((pos (or/c #f natural-number/c)))
                             (_ (or/c (vectorof string?
                             )
                                      string?
                                      ))))   
            
            (name (->m string?
            ))
            (name-set! (->m string?
             any))
            
        
    ))
(define/contract other-type% other-type<%>/c other-type%/b)


(define method<%>/c 
	(class/c 
            (comment (->m string?
            ))
            (comment-set! (->m string?
             any))
            
            (is-private (->m boolean?
            ))
            (is-private-set! (->m boolean?
             any))
            
            
            
            (name (->m string?
            ))
            (name-set! (->m string?
             any))
            
            
            
        
    ))
(define/contract method% method<%>/c method%/b)


(define param<%>/c 
	(class/c 
            (comment (->m string?
            ))
            (comment-set! (->m string?
             any))
            
            
            
            (name (->m string?
            ))
            (name-set! (->m string?
             any))
            
            (default-value (->dm () ((pos (or/c #f natural-number/c)))
                             (_ (or/c (vectorof string?
                             )
                                      string?
                                      ))))   
            
        
    ))
(define/contract param% param<%>/c param%/b)


(define wire-many<%>/c 
	(class/c 
        
    ))
(define/contract wire-many% wire-many<%>/c wire-many%/b)


(define wire-method<%>/c 
	(class/c 
            
            
        
    ))
(define/contract wire-method% wire-method<%>/c wire-method%/b)



;) ; end module box
