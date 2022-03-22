;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname exercise_8) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require "define_struct.rkt")

;a quiz is a ...
; (make-quiz (listof fun-question) (listof String) (hash String Number))
(define-struct quiz [questions possible-outcomes scoring-of-outcomes]
  #:methods
  ;update-scoring-hash: quiz String -> void
  ;Increments the scoring-of-outcomes hash, adding one to the value
  ; associated with outcome. 
  ;Effect: scoring-of-outcomes attribute has changed, incrementing value
  ; associated with outcome by 1. 
  (define (update-scoring-hash q outcome)
    (hash-set! (quiz-scoring-of-outcomes q) outcome (+ 1 (hash-ref (quiz-scoring-of-outcomes q) outcome)))))

  ;reset-scoring-hash: quiz -> void
  ;Sets the value associated with each key in scoring-of-outcomes to 0.
  ; The possible-outcomes attribute holds of a list of each of the possible-
  ; outcomes - the keys of the hash. 
  ;Effect: scoring-of-outcomes attribute has changed, all values set to 0.
  (define (reset-scoring-hash q)
    (for-each (lambda (n)
                (hash-set! (quiz-scoring-of-outcomes q) n 0))
              (quiz-possible-outcomes q)))
  
    ;get-scoring-outcome: quiz -> String
    ;Iterates over the scoring-of-outcomes hash to find and return the
    ;  outcome with the highest associated value.
    ; The possible-outcomes attribute holds of a list of each of the possible-
    ; outcomes - the keys of the hash. 
    (define (get-scoring-outcome q)
      (local [(define max-val 0)
              (define best-outcome-str "")]
      (begin (for-each (lambda (n)
                         (when (> (hash-ref (quiz-scoring-of-outcomes q) n) max-val)
                           (begin (set! max-val (hash-ref (quiz-scoring-of-outcomes q) n))
                                  (set! best-outcome-str n))))
                       (quiz-possible-outcomes q))
             best-outcome-str)))

  ;a fun question is a ...
  ; (make-fun-question string number (listof string) (listof string))
  ; If a user answers with a particular choice, you can think of that as a vote
  ;   for a particular outcome (the result of the quiz as a whole).
  (define-struct fun-question [text number-of-choices choices corresponding-outcomes] 
    #:methods
    (define (display q)
      (begin (printf "question: ~a ~n" (fun-question-text q))
             (local [(define choice-num 1)]
               (for-each (λ (c)
                           (begin
                             (printf "~a. ~a~n" choice-num c)
                             (set! choice-num (+ 1 choice-num))))
                         (fun-question-choices q)))))
  
    ;get-choice-from-number: fun-question Number -> String
    ;Returns the choice associated with a particular number.
    ;(read about the list-ref function in the documentation)
    (define (get-choice-from-number q n)
      (list-ref (fun-question-choices q) (- n 1))) ;index starts at 0, so need to subtract 1

    ;get-corresponding-outcome-from-number: fun-question Number -> String
    ;Returns the outcome associated with a particular number.
    ;(read about the list-ref function in the documentation)
    (define (get-corresponding-outcome-from-number q n)
      (list-ref (fun-question-corresponding-outcomes q) (- n 1)))) ;index starts at 0, so need to subtract 1

  ; an example "quiz" (a list of questions)
  (define myquiz
    (make-quiz (list
                (make-fun-question
                 "How would your friends describe you?"
                 6
                 (list "Funny" "Strong" "Emotional"
                       "Kind" "Shy" "Intense")
                 (list "Dolores" "Luisa" "Pepa"
                       "Mirabel" "Bruno" "Isabella"))
                (make-fun-question
                 "Choose a gift you'd love to have."
                 6
                 (list "Invisibility" "Mind Reading"
                       "Ability to fly" "Knowing every language"
                       "No gift" "Literally any gift")
                 (list "Dolores" "Luisa" "Pepa"
                       "Bruno" "Isabella" "Mirabel"))
                (make-fun-question
                 "Pick the Encanto song you can't stop listening to."
                 6
                 (list "The Family Madrigal"
                       "Waiting on a Miracle"
                       "Surface Pressure"
                       "We Don't Talk About Bruno"
                       "What Else Can I Do?"
                       "Impossible to pick just one")
                 (list "Dolores" "Mirabel" "Luisa"
                       "Bruno" "Pepa" "Isabella")))
               (list "Dolores" "Luisa" "Pepa"
                     "Mirabel" "Bruno" "Isabella")
               (make-hash '(("Dolores" 0) ("Mirabel" 0) ("Luisa" 0) ("Bruno" 0) ("Pepa" 0) ("Isabella" 0)))))


  (check-expect (begin
                  (reset-scoring-hash myquiz)
                  (update-scoring-hash myquiz "Bruno")
                  (update-scoring-hash myquiz "Bruno")
                  (update-scoring-hash myquiz "Bruno")
                  (update-scoring-hash myquiz "Mirabel")
                  (update-scoring-hash myquiz "Isabella")
                  (get-scoring-outcome myquiz))                  "Bruno")
  (check-expect (begin
                  (reset-scoring-hash myquiz)
                  (update-scoring-hash myquiz "Pepa")
                  (hash-ref (quiz-scoring-of-outcomes myquiz) "Pepa"))    1)
  (check-expect (begin
                  (reset-scoring-hash myquiz)
                  (hash-ref (quiz-scoring-of-outcomes myquiz) "Pepa")) 0)



  ; runquiz: (listof question) -> void
  ; Takes a list of questions. In order, displays the question,
  ; gets a response from the user and checks the answer.
  ; Effect: A quiz has been displayed and run.
  (define (runquiz somequiz)
    (local [(define user-response "")
            (define user-responses (list))]
      (begin
        (printf "Welcome to my quiz! ~n")
        (reset-scoring-hash somequiz)
        (for-each (λ (q)
                    (begin (display q)
                           (set! user-response (read))
                           (set! user-responses (append user-responses
                                                        (list (get-choice-from-number q user-response))))
                           (update-scoring-hash somequiz
                                                (get-corresponding-outcome-from-number q
                                                                                       user-response))))
                  (quiz-questions somequiz))
        (newline)
        (printf "You answered...")
        (newline)
        (for-each (lambda (s)
                    (begin (printf s)
                           (newline)))
                  user-responses)
        (printf "Your result is...")
        (get-scoring-outcome somequiz))))


  ;credit buzzfeed - https://www.buzzfeed.com/jenniferabidor/encanto-character-quiz 
  ; run the quiz on our list of questions
  (runquiz myquiz)