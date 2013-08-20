#lang racket/base

; (module box racket/base

  (require racket/class)

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
  (class object%
  
    (super-new)

    ;; fields (features)
	; attribute comment
	(field (comment 0))
	
	; attribute name
	(field (name 0))
	
	
	; reference attributes
	(field (attributes null))
	
	; reference methods
	(field (methods null))
	
	; reference wires
	(field (wires null))
	
	; reference boxes
	(field (boxes null))
	
	; reference connection
	(field (connection null))
	
	

  ))


;; Class BoxReference (box-reference%)
(define box-reference%
  (class object%
  
    (super-new)

    ;; fields (features)
	; attribute is-pointer
	(field (is-pointer 0))
	
	; attribute is-reference
	(field (is-reference 0))
	
	; attribute name
	(field (name 0))
	
	
	; reference box
	(field (box null))
	
	

  ))


;; Class WireReference (wire-reference%)
(define wire-reference%
  (class object%
  
    (super-new)

    ;; fields (features)
	
	; reference wire
	(field (wire null))
	
	

  ))


;; Class LocalWireReference (local-wire-reference%)
(define local-wire-reference%
  (class wire-reference%
  
    (super-new)

    ;; fields (features)
	
	

  ))


;; Class ExternalWireReference (external-wire-reference%)
(define external-wire-reference%
  (class wire-reference%
  
    (super-new)

    ;; fields (features)
	
	; reference box-reference
	(field (box-reference null))
	
	

  ))


;; Class Connection (connection%)
(define connection%
  (class object%
  
    (super-new)

    ;; fields (features)
	
	; reference from
	(field (from null))
	
	; reference to
	(field (to null))
	
	

  ))


;; Class Wire (wire%)
(define wire%
  (class object%
  
    (super-new)

    ;; fields (features)
	; attribute is-input
	(field (is-input 0))
	
	; attribute name
	(field (name 0))
	
	
	; reference type
	(field (type null))
	
	

  ))


;; Class Type (type%)
(define type%
  (class object%
  
    (super-new)

    ;; fields (features)
	; attribute is-pointer
	(field (is-pointer 0))
	
	; attribute is-reference
	(field (is-reference 0))
	
	
	

  ))


;; Class BasicType (basic-type%)
(define basic-type%
  (class type%
  
    (super-new)

    ;; fields (features)
	; attribute basic-type
	(field (basic-type 0))
	
	
	

  ))


;; Class OtherType (other-type%)
(define other-type%
  (class type%
  
    (super-new)

    ;; fields (features)
	; attribute namespace
	(field (namespace 0))
	
	; attribute name
	(field (name 0))
	
	
	

  ))


;; Class Method (method%)
(define method%
  (class object%
  
    (super-new)

    ;; fields (features)
	; attribute comment
	(field (comment 0))
	
	; attribute is-private
	(field (is-private 0))
	
	; attribute name
	(field (name 0))
	
	
	; reference type
	(field (type null))
	
	; reference parameters
	(field (parameters null))
	
	

  ))


;; Class Param (param%)
(define param%
  (class object%
  
    (super-new)

    ;; fields (features)
	; attribute comment
	(field (comment 0))
	
	; attribute name
	(field (name 0))
	
	; attribute default-value
	(field (default-value 0))
	
	
	; reference type
	(field (type null))
	
	

  ))


;; Class WireMany (wire-many%)
(define wire-many%
  (class wire%
  
    (super-new)

    ;; fields (features)
	
	

  ))


;; Class WireMethod (wire-method%)
(define wire-method%
  (class wire%
  
    (super-new)

    ;; fields (features)
	
	; reference method
	(field (method null))
	
	

  ))


;) ; end module box
