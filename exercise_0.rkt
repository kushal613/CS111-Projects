;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname exercise_0) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Write your code here!
(require 2htdp/image)

(define a-red-square (square 100 "solid" "red"))
(define a-blue-circle (circle 50 "solid" "blue"))
(define outlined-square (square 100 "outline" "red"))
(define outlined-circle (circle 50 "outline" "blue"))

(define row-of-squares (beside (square 100 "solid" "red")
                               (square 100 "solid" "blue")
                               (square 100 "solid" "green")))

(define column-of-squares (above (square 100 "solid" "red")
                                 (square 100 "solid" "blue")
                                 (square 100 "solid" "green")))

(define nested-squares (overlay (square 40 "solid" "black")
                                (square 60 "solid" "green")
                                (square 80 "solid" "blue")
                                (square 100 "solid" "red")))

(define rotated-squares (overlay (rotate 45 (square 40 "solid" "black"))
                                 (rotate 45 (square 60 "solid" "green"))
                                 (rotate 45 (square 80 "solid" "blue"))
                                 (rotate 45 (square 100 "solid" "red"))))

(define flag-of-chicago  (overlay (overlay/xy
                                  (radial-star 6 10 40 "solid" "red")
                                  100 0
                                  (radial-star 6 10 40 "solid" "red"))
                                  
                                  (overlay/xy
                                  (radial-star 6 10 40 "solid" "red")
                                  300 0
                                  (radial-star 6 10 40 "solid" "red"))
                                  
                          (above (rectangle 500 50 "solid" (make-color 179 221 242))
                                 (rectangle 500 120 "solid" "white")
                                 (rectangle 500 50 "solid" (make-color 179 221 242)))))


;; DON'T CHANGE THESE!!!
;; These are just tests to make sure all of your images are named correctly for grading
(check-expect (image? a-red-square) #t)
(check-expect (image? a-blue-circle) #t)
(check-expect (image? outlined-square) #t)
(check-expect (image? outlined-circle) #t)
(check-expect (image? row-of-squares) #t)
(check-expect (image? column-of-squares) #t)
(check-expect (image? nested-squares) #t)
(check-expect (image? rotated-squares) #t)
(check-expect (image? flag-of-chicago) #t)