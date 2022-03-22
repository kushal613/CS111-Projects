;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname exercise_7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require "define_struct.rkt")


;Part 1
;Define the question struct and its methods here.
(define-struct question [text answer point-value]
  #:methods
  (define (display q) ;q is a question
    (printf "question: ~a~n" (question-text q)))
  (define (check-answer q input)
    (if (equal? (question-answer q) input)
        #true
        #false)))

; a question is a ....
; (make-question string symbol number)
 

;Some tests to make sure that you define the question struct properly. 
(define q1 (make-question "What is Sirius' nickname when disguised as a dog?" 'Padfoot 3))
(check-expect (procedure? display) true)
(check-expect (procedure? check-answer) true)
(check-expect (question? q1) true)
(check-expect (question-text q1) "What is Sirius' nickname when disguised as a dog?")
(check-expect (check-answer q1 'Padfoot)
              true)
(check-expect (check-answer q1 'Spot)
              false)
(check-expect (question-point-value q1) 3)


;Part 2
;Define the multiple-choice-question struct and its methods here.
(define-struct (multiple-choice-question question) [number-of-choices choices]
  #:methods
  (define (display mcq) ;mcq is a multiple-choice-question
    (printf "question: ~a~n 1. ~a~n 2. ~a~n 3. ~a~n"
            (question-text mcq)
            (first (multiple-choice-question-choices mcq))
            (second (multiple-choice-question-choices mcq))
            (third (multiple-choice-question-choices mcq))
            ))
  (define (check-answer mcq input)
    (if (equal? input (question-answer mcq))
        #true
        #false)))

;a multiple choice question is a ...
; (make-multiple-choice-question string symbol number number (listof string))

(define q2
  (make-multiple-choice-question
   "Which spell does Hermoine use to repel Nagini as she attacks her and Harry in Bathilda Bagshot's house?"
   '1 3 3
   (list "Confringo" "Bombarda Maxima" "Sectumsempra")))
(check-expect (question? q2) true)
(check-expect (multiple-choice-question? q2) true)
(check-expect (multiple-choice-question-number-of-choices q2) 3)
(check-expect (check-answer q2 '1)
              true)
(check-expect (question-point-value q2) 3)
(check-expect (length (multiple-choice-question-choices q2)) 3)

;Define the numeric-question struct and its methods here.
(define-struct (numeric-question question) [error-range]
  #:methods
  (define (display nq) ;nq is a numeric-question
    (printf "question: ~a ~n" (question-text nq)))
  (define (check-answer nq input)
    (if (and (< (- (question-answer nq) (numeric-question-error-range nq)) input)
             (< input (+ (question-answer nq) (numeric-question-error-range nq))))
        #true
        #false)))

;a numeric question is a ...
; (make-numeric-question string symbol number number)

(define q3 (make-numeric-question "When Harry is appointed Seeker, how many
 years is it since someone his age has been appointed to a house Quidditch team?" 100 3 2))
(check-expect (question? q3) true)
(check-expect (numeric-question? q3) true)
(check-expect (multiple-choice-question? q3) false)
(check-expect (check-answer q3 101)
              true)
(check-expect (check-answer q3 99)
              true)
(check-expect (check-answer q3 103)
              false)

; an example "quiz" (a list of questions)
(define myquiz (list q1 q2 q3)) 

; runquiz: (listof question) -> void
; Takes a list of questions. In order, displays the question,
; gets a response from the user and checks the answer.
; Effect: A quiz has been displayed and run.
(define (runquiz somequiz) 
  (begin
    (printf "Welcome to my quiz! ~n")
    (local [(define user-response "")
            (define points-correct 0)
            (define total-points-possible 0)]
      (begin (for-each (λ (q)
                         (begin (display q)
                                (set! user-response (read))
                                (if (check-answer q user-response)
                                    (begin (printf "Yay!! Good job! ~n~n")
                                           (set! points-correct (+ points-correct
                                                                   (question-point-value q))))
                                    (printf "Sorry - thats wrong! ~n~n"))
                                (set! total-points-possible (+ total-points-possible
                                                               (question-point-value q)))))
                         somequiz)
             (printf "Your overall score is....")
             (print points-correct)
             (printf " out of ")
             (print total-points-possible)
             (newline)))))

;run the quiz on our list of questions
;Once you've completed parts 1 and 2, uncomment the following line try to out the quiz! 
(runquiz myquiz)