#lang racket

(require "./box.rkt")

(define b1 (new Box))
(send b1 name-set! "box1")
(define p1 (new Param))
(send p1 name-set! "param1")
(send b1 attributes-append! p1)
(send b1 comment-set! "comment")