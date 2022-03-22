;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Exercise 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "./remove_duplicates.rkt")

; This defines the basic album datatype.
(define-struct album (title artist genre))
; define-struct automatically creates the following functions for you:
;
; `make-<struct-name>` (in this case `make-album`)
;   a function to create an instance of the struct
;   this function takes arguments for each of the fields listed, so for example
;   (make-struct 'Sway' 'Tove Styrke' 'Pop') will create an album struct
;    with title 'Sway', artist 'Tove Styrke' & genre 'Pop
;
; `<struct-name>-<field-name>` (for each field)
;    functions for accessing values of each field in the struct
;    for album this would mean we'd have the following functions:
;    `album-title`, `album-artist`, `album-genre`
;    the following examples creates an album and then accesses its fields
;    ```
;    (define sway (make-album 'Sway' 'Tove Styrke' 'Pop')
;    (album-title sway) ; returns 'Sway'
;    (album-artist sway) ; returns 'Tove Styrke'
;    (album-genre sway) ; returns 'Pop'
;    ```
;
; `<struct-name>?` (in this case `album?`)
;   a predicate (function which returns a boolean) that checks a value and
;   returns true if it's an instance of the struct, false otherwise
;   using the `sway` album defined in the previous example
;   ```
;   (album? sway) ; returns true
;   (album? 1) ; returns false
;   (album? 'hi') ; returns false
;   ```

;;; Enter a list of albums below
;;; They need not be the actual albums you own.
;;; But you should include enough variety to adequately
;;; test your code.
;;;
;;; Here's what we mean.  One of the questions involves
;;; writing a procedure that finds all the albums of a
;;; given genre.  If all the albums in the library are
;;; in the rock genre, then there's only one genre and
;;; when you ask for all the rock albums and it gives
;;; back all the albums, you don't know whether that's
;;; because the code really works, or because it's
;;; not even paying attention to the genre.  So you want
;;; to make sure there are multiple artists and genres,
;;; some artists with only one album or genre, others
;;; with multiple artists or genres, etc.

(define testing-library-1
  (list (make-album "Adharam Madhuram" "Shreya" "Religious")
        (make-album "Bairi Piya" "Shreya" "classical")
        (make-album "Dola re dola" "Shreya" "Filmy")
        (make-album "Kal ho na ho" "Sonu" "Filmy")
        (make-album "Ashutosh" "Sonu" "Religious")
        (make-album "Soona" "Sonu" "classical")
        (make-album "Yeh galiyan" "Lataji" "Filmy")
        (make-album "Laga ja" "Lataji" "classical")
        (make-album "Sukhakarta" "Lataji" "Religious")
        (make-album "Ehsan tera" "Rafi" "classical")
        (make-album "Badan pe sitare" "Rafi" "Filmy")
        (make-album "Sukh ke" "Rafi" "Religious")
        (make-album "Galti se mistake" "Arjit" "Filmy")
        (make-album "Agar Tum" "Arjit" "Filmy")))

;;; Add the procedures you write (e.g. all-genres, versatile-artists)
;;; below.  Be sure to test your procedures to make sure they work.
;;; We only provide very few test cases this time, so you need
;;; to write your own test cases to make sure the code works.
;;; We will use our own test cases when grading and assign you
;;; a grade based on the number of test cases that passed.


;; all-titles : (listof album) -> (listof string)
;; Gets all titles of all albums in the library

(define all-titles
  (lambda (loa)
    (remove-duplicates
     (map album-title loa))))


(check-expect (all-titles testing-library-1)

              (list "Adharam Madhuram" "Bairi Piya" "Dola re dola" "Kal ho na ho"
                    "Ashutosh" "Soona" "Yeh galiyan" "Laga ja" "Sukhakarta"
                    "Ehsan tera" "Badan pe sitare" "Sukh ke" "Galti se mistake" "Agar Tum"))



;; all-artists: (listof album) -> (listof string)
;; Gets all artists of all albums in the library

;; be sure to use remove-duplicates

(define all-artists
  (lambda (loa)
    (remove-duplicates
     (map album-artist loa))))

(check-expect (all-artists testing-library-1)

              (list "Shreya" "Sonu" "Lataji" "Rafi" "Arjit"))


;; all-genres: (listof album) -> (listof string)
;; Gets all genres of all albums in the library

;; be sure to use remove-duplicates

(define all-genres
  (lambda (loa)
    (remove-duplicates
     (map album-genre loa))))

(check-expect (all-genres testing-library-1)

              (list "Religious" "classical" "Filmy"))



;; artist-albums : string (listof album) -> (listof album)
;; Get all albums in the library by the given artist

(define artist-albums
  (lambda (desired-artist loa)
    (filter (lambda (album)
              (equal? (album-artist album) desired-artist)) loa)))



(check-expect (artist-albums "Shreya" testing-library-1)
              (list (make-album "Adharam Madhuram" "Shreya" "Religious")
                    (make-album "Bairi Piya" "Shreya" "classical")
                    (make-album "Dola re dola" "Shreya" "Filmy")))

;; Procedure ahead of filter call needs to be a predicate

;; artist-genres: string, (listof album) -> (listof string)
;; Get all genres in the library of a given artist

(define artist-genres
  (lambda (desired-artist loa)
    (remove-duplicates
      (all-genres (artist-albums desired-artist loa)))))

(check-expect (artist-genres "Shreya" testing-library-1)
              (list "Religious" "classical" "Filmy"))

(check-expect (artist-genres "Arjit" testing-library-1)
              (list "Filmy"))


;; artist-is-versatile?: string, (listof album) -> boolean
;; takes the name of an artist and the library as
;; inputs and returns true if an artist has albums in more than one genre

;; hint: use artist-genres (if there is more than one artist in the list, then the list is versatile)

(define (artist-is-versatile? desired-artist loa)
  (> (length (artist-genres desired-artist loa)) 1))

(check-expect (artist-is-versatile? "Shreya" testing-library-1) #true)
(check-expect (artist-is-versatile? "Sonu" testing-library-1) #true)
(check-expect (artist-is-versatile? "Arjit" testing-library-1) #false)


;; versatile-artists: (listof album) -> (listof string)
;; Returns a list of the names of all artists who work in more than one genre.

(define (versatile-artists loa)
  (filter (lambda (an-artist)
            (artist-is-versatile? an-artist loa)) ;some procedure that takes an artist, and returns true if that artist is versatile
              ; cannot just use artist-is-versatile? because this takes two inputs, but filter takes only one input, so use lambda
   (all-artists loa)))

(check-expect (versatile-artists testing-library-1)
              (list "Shreya" "Sonu" "Lataji" "Rafi"))


;;;;;;;; The first two functions below, artist-album-count, and artist-album-count-list, are used to create artist-album-counts

; artist-album-count: string, album --> number
; tells how many albums an-artist has in loa (the library), this is the number that is an output in artist-album-counts

(define (artist-album-count an-artist loa)
  (length (artist-albums an-artist loa)))

(check-expect (artist-album-count "Shreya" testing-library-1) 3)

; artist-album-count-list:
; given the name of an artist, returns the two-element list, the artist’s name followed by their album count. 

(define (artist-album-count-list an-artist loa)
  (list an-artist (artist-album-count an-artist loa)))

(check-expect (artist-album-count-list "Shreya" testing-library-1)
              (list "Shreya" 3))

;; Finally, we can write the procedure artist-album-counts, number 9 in exercise 2

;; artist-album-counts: (listof album) -> (listof (list string number))
;; returns the number of albums by each artist

(define (artist-album-counts loa)
  (map (lambda (an-artist) (artist-album-count-list an-artist loa))
       (all-artists loa)))

(check-expect (artist-album-counts testing-library-1)
              (list (list "Shreya" 3) (list "Sonu" 3) (list "Lataji" 3) (list "Rafi" 3) (list "Arjit" 2)))
              

;; genre-album-counts: (listof album) -> (listof (list string number))
;; returns the number of albums in each genre


;; METHOD 1 for Number 9

(define (genre-album-counts loa)
 (map
     (lambda (genre) ; this takes in a genre and returns (genre number of albums in the genre)
       (list genre (length
                    (filter (lambda (album)
                             (equal? (album-genre album) genre)) ; lambda function returns true if album genre equals desired genre
                               loa))))
                               (all-genres loa)))

(check-expect (genre-album-counts testing-library-1)
              (list (list "Religious" 4) (list "classical" 4) (list "Filmy" 6)))


;; METHOD 2 for Number 9 --> The last procedure which is genre-album-counts requires the first three functions to be used
;; it is the same procedure as the one above, hence it has been commented out

(define genre-album-count
  (lambda (a loa)
    (length (genre-album a loa))))

(check-expect (genre-album-count "classical" testing-library-1) 4)

(define genre-album-count-list
  (lambda (genre-name loa)
    (list genre-name (genre-album-count genre-name loa))))

(check-expect (genre-album-count-list "classical" testing-library-1)
              (list "classical" 4))

(define genre-album
  (lambda (genre loa)
    (filter (lambda (albums) (equal? genre (album-genre albums))) loa)))

(check-expect (genre-album "classical" testing-library-1)
              (list (make-album "Bairi Piya" "Shreya" "classical")
                    (make-album "Soona" "Sonu" "classical")
                    (make-album "Laga ja" "Lataji" "classical")
                    (make-album "Ehsan tera" "Rafi" "classical")))

;(define genre-album-counts
;   (lambda (loa) (remove-duplicates
;    (map (lambda (a)
;           (genre-album-count-list (album-genre a) loa)) loa))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Below are two example album libraries and a few example tests
;;; for SOME functions.
;;;
;;; ALL functions in this assignment should work for ANY given
;;; library, and the test cases below illustrate how libraries
;;; are passed to album functions.
;;;
;;; These test cases are far from complete. Remember to write new
;;; album library and tests to see if your solution works as expected.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define example-testing-library-1
  (list (make-album "You're Stronger Than You Know" "James Morrison" "Pop")
        (make-album "Bootleg"                       "Kenshi Yonezu"  "J-Pop")))

(define example-testing-library-2
  (list (make-album "Arthur Rubinstein Collection" "Arthur Rubinstein" "Classic")
        (make-album "Scott Joplin Piano Rags"      "Joshua Rifkin"     "Rags")
        (make-album "The Violin Sonatas (4 CDs)"   "Lev Oborin"        "Classic")))

(check-expect (all-titles example-testing-library-1)
              (list "You're Stronger Than You Know"
                    "Bootleg"))

(check-expect (all-titles example-testing-library-2)
              (list "Arthur Rubinstein Collection"
                    "Scott Joplin Piano Rags"
                    "The Violin Sonatas (4 CDs)"))

(check-expect (all-genres example-testing-library-2)
              (list "Classic" "Rags"))

(check-expect (artist-albums "Scott Joplin" example-testing-library-2)
              (list))

(check-expect (artist-albums "Joshua Rifkin" example-testing-library-2)
              (list (make-album "Scott Joplin Piano Rags" "Joshua Rifkin" "Rags")))

(check-expect (artist-albums "Joshua Rifkin" example-testing-library-1)
              (list))

(check-expect (artist-albums "Kenshi Yonezu" example-testing-library-1)
              (list (make-album "Bootleg" "Kenshi Yonezu" "J-Pop")))

(check-expect (artist-is-versatile? "James Morrison" example-testing-library-1)
              #false)

(check-expect (artist-album-counts example-testing-library-1)
              (list (list "James Morrison" 1)
                    (list "Kenshi Yonezu" 1)))

(check-expect (procedure? all-titles) #true)
(check-expect (procedure? all-artists) #true)
(check-expect (procedure? all-genres) #true)
(check-expect (procedure? artist-albums) #true)
(check-expect (procedure? artist-genres) #true)
(check-expect (procedure? artist-is-versatile?) #true)
(check-expect (procedure? versatile-artists) #true)
(check-expect (procedure? artist-album-counts) #true)
(check-expect (procedure? genre-album-counts) #true)
