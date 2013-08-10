#lang racket/base

;; Class Box (box)
(struct box
  
    ;; slots (features)
	(
	; attribute comment
	comment
	; attribute name
	name
	
	; reference attributes
	attributes
	; reference methods
	methods
	; reference wires
	wires
	; reference boxes
	boxes
	; reference connection
	connection
	
	)

  #:transparent)


;; Class BoxReference (box-reference)
(struct box-reference
  
    ;; slots (features)
	(
	; attribute is-pointer
	is-pointer
	; attribute is-reference
	is-reference
	; attribute name
	name
	
	; reference box
	box
	
	)

  #:transparent)


;; Class WireReference (wire-reference)
(struct wire-reference
  
    ;; slots (features)
	(
	
	; reference wire
	wire
	
	)

  #:transparent)


;; Class LocalWireReference (local-wire-reference)
(struct local-wire-reference
  wire-reference
  
    ;; slots (features)
	(
	
	
	)

  #:transparent)


;; Class ExternalWireReference (external-wire-reference)
(struct external-wire-reference
  wire-reference
  
    ;; slots (features)
	(
	
	; reference box-reference
	box-reference
	
	)

  #:transparent)


;; Class Connection (connection)
(struct connection
  
    ;; slots (features)
	(
	
	; reference from
	from
	; reference to
	to
	
	)

  #:transparent)


;; Class Wire (wire)
(struct wire
  
    ;; slots (features)
	(
	; attribute is-input
	is-input
	; attribute name
	name
	
	; reference type
	type
	
	)

  #:transparent)


;; Class Type (type)
(struct type
  
    ;; slots (features)
	(
	; attribute is-pointer
	is-pointer
	; attribute is-reference
	is-reference
	
	
	)

  #:transparent)


;; Class BasicType (basic-type)
(struct basic-type
  type
  
    ;; slots (features)
	(
	; attribute basic-type
	basic-type
	
	
	)

  #:transparent)


;; Class OtherType (other-type)
(struct other-type
  type
  
    ;; slots (features)
	(
	; attribute namespace
	namespace
	; attribute name
	name
	
	
	)

  #:transparent)


;; Class Method (method)
(struct method
  
    ;; slots (features)
	(
	; attribute comment
	comment
	; attribute is-private
	is-private
	; attribute name
	name
	
	; reference type
	type
	; reference parameters
	parameters
	
	)

  #:transparent)


;; Class Param (param)
(struct param
  
    ;; slots (features)
	(
	; attribute comment
	comment
	; attribute name
	name
	; attribute default-value
	default-value
	
	; reference type
	type
	
	)

  #:transparent)


;; Class WireMany (wire-many)
(struct wire-many
  wire
  
    ;; slots (features)
	(
	
	
	)

  #:transparent)


;; Class WireMethod (wire-method)
(struct wire-method
  wire
  
    ;; slots (features)
	(
	
	; reference method
	method
	
	)

  #:transparent)


