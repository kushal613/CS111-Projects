;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Exercise 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; not needed to implement your functions
; but very helpful for testing
(require "./iterated_images.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 1: Ordinary Recursion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; FILL IN FUNCTIONS HERE
; function names are given but you
; MUST add signatures and purpose statements

; multiply-list : (listof number) -> number
; computes the product of a list of numbers

(define (multiply-list list)
  (cond [(empty? list)        1]
        [else                 (* (first list)
                                 (multiply-list (rest list)))]))

(check-expect (multiply-list (list 2 3 4)) 24)
(check-expect (multiply-list (list)) 1)
(check-expect (multiply-list (list 1)) 1)
(check-expect (multiply-list (list 0 3 4)) 0)
(check-expect (multiply-list (list 0)) 0)


; my-iterated-overlay : (number -> image), number -> image
; overlays pictures on top of one another, with earlier images being on top of later ones

(define (my-iterated-overlay proc n)
  (cond [(= n 0) empty-image]
        [else    (overlay (my-iterated-overlay proc (- n 1))
                          (proc (- n 1)))]))

(check-expect
 (my-iterated-overlay
  (lambda (n) (circle (* 10 n) "solid" "black")) 0) empty-image)

(check-expect (my-iterated-overlay (lambda (n) (circle (* 10 n) "solid" "blue")) 3)
 (overlay (circle 0 "solid" "blue")
          (circle 10 "solid" "blue")
          (circle 20 "solid" "blue")))

; iterated-any : (image, image -> image), (number -> image), number -> image
; Abstracted version of my-iterated-overlay that takes an arbitrary combiner, a generator,
; and a number of iterations, and combines the results using the specified combiner. 

(define (iterated-any combiner generator n)
  (if (= n 0) empty-image
      (combiner   (iterated-any combiner generator (- n 1))
                  (generator (- n 1)))))

(define (square-gen n) 
  (square (* n 10) 
          "solid" 
          (color (* n 50) 
                 0 0))) 

(check-expect (iterated-any beside square-gen 5)
              (beside (square-gen 0)
                      (square-gen 1)
                      (square-gen 2)
                      (square-gen 3)
                      (square-gen 4)))

(check-expect (iterated-any beside square-gen 0) empty-image)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 2: Iterative Recursion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; FILL IN FUNCTIONS HERE
; function names are given but you
; MUST add signatures and purpose statements

; multiply-list/iter : (listof number) -> number
; computes the product of a list of numbers

(define (multiply-list/iter lon)
  (local [(define (helper alon accum)
  (cond [(empty? alon) accum]
        [else (helper (rest alon)(* accum (first alon)))]))]
  (helper lon 1)))

(check-expect (multiply-list/iter (list 2 3 4)) 24)
(check-expect (multiply-list/iter (list)) 1)
(check-expect (multiply-list/iter (list 0)) 0)

; iterated-overlay/iter : (number -> image), number -> image
; overlays pictures on top of one another, with earlier images being on top of later ones

(define (iterated-overlay/iter proc n)
  (local [(define (help n accum)
            (cond [(= n 0)        accum]
                  [else           (help (- n 1) (overlay (proc (- n 1)) accum))]))]
         (help n empty-image)))

(check-expect (iterated-overlay/iter (lambda (n) (circle (* 10 n) "solid" "blue")) 3)
 (overlay (circle 0 "solid" "blue")
          (circle 10 "solid" "blue")
          (circle 20 "solid" "blue")))

(check-expect (iterated-overlay/iter square-gen 0)
              (iterated-overlay square-gen 0))

; iterated-any/iter : (image, image -> image), (number -> image), number -> image
; Abstracted version of iterated-overlay/iter that takes an arbitrary combiner, a generator,
; and a number of iterations, and combines the results using the specified combiner. 

(define (iterated-any/iter combiner generator n)
  (local [(define (help n combiner_h generator_h accum)
            (cond [(= n 0) accum]
                  [else    (help (- n 1) combiner_h generator_h
                                 (combiner_h (generator_h (- n 1)) accum))]))]
         (help n combiner generator empty-image)))

(check-expect (iterated-any/iter beside square-gen 5)
              (beside (square-gen 0)
                      (square-gen 1)
                      (square-gen 2)
                      (square-gen 3)
                      (square-gen 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;     We are providing fewer and fewer tests!
;;     Come up with your own tests carefully following the PDF.
;;     For the recursive iterated functions, you could refer to
;;     your Exercise 1 file and use some of its images for your testing.
;;    
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-expect (multiply-list '()) 1)

;; square-gen : number -> image
(define (square---gen n)
  (square (+ (* n 50) 50)
          "solid"
          (color (* n 50)
                 0
                 0)))
(check-expect (my-iterated-overlay square---gen 3) 
              (overlay (square---gen 0)
                       (square---gen 1)
                       (square---gen 2)))

(check-expect (my-iterated-overlay square---gen 5)
              (iterated-overlay square---gen 5))

(check-expect (procedure? multiply-list) #true)
(check-expect (procedure? my-iterated-overlay) #true)
(check-expect (procedure? iterated-any) #true)
(check-expect (procedure? multiply-list/iter) #true)
(check-expect (procedure? iterated-overlay/iter) #true)
(check-expect (procedure? iterated-any/iter) #true)
