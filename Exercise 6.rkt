;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Exercise 6|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require "./file_operations.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 1: Recursion over the File System Structure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; sum : (listof number) -> number
;; returns the sum of the numbers in a list

(define (sum lst)
  (cond [(empty? lst)    0]
        [else            (+ (first lst) (sum (rest lst)))]))

(check-expect (sum (list 3 2 4)) 9)
(check-expect (sum (list 0)) 0)
(check-expect (sum (list)) 0)


(check-satisfied sum procedure?)

;; count-files : path -> number
; Takes a path to a directory, and returns the number of files within the  
; directory and all its descendants, recursively.

;The below code also works, it just does not make use of the sum procedure defined above

;(define (all-files directory)
 ;          (append (directory-files directory)
  ;                 (append
   ;                  (map all-files 
    ;                   (directory-subdirectories directory)))))

;(define (count-files dir-path)
 ;      (length (all-files dir-path)))

(define (count-files dir-path)
  (sum (list (length (directory-files dir-path))
             (length (map count-files (directory-subdirectories dir-path))))))

(check-expect (count-files (string->path "test")) 5)

(check-satisfied count-files procedure?)

;; directory-size : path -> number 
;; Returns the number of bytes of the given directory and all its contents, recursively

(define (directory-size dir-path)
  (sum (append (map file-size (directory-files dir-path))
               (map directory-size (directory-subdirectories dir-path)))))

(check-expect (directory-size (string->path "test")) 7177)

(check-satisfied directory-size procedure?)

;; concat : (listof (listof path)) -> (listof path)
;; takes a list of lists of paths and concatenate them in the given order to produce a list of all given paths.
(define (concat lst-of-lists)
  (apply append lst-of-lists))

(check-satisfied concat procedure?)
(check-expect
 (concat
  (list (list (build-path "test"))
        (list (build-path "test" "test_2")
              (build-path "test" "test_3"))))
 (list (build-path "test")
       (build-path "test" "test_2")
       (build-path "test" "test_3")))
 
;; all-directories : path -> (listof path)
(define (all-directories dir-path)
  (apply append (list dir-path)
                (map all-directories (directory-subdirectories dir-path))))

(check-expect (all-directories (string->path "test"))
              (list (string->path "test") (string->path "test/test_2") (string->path "test/test_3")))
(check-expect (all-directories (string->path "test/test_3"))
              (list (string->path "test/test_3")))

(check-satisfied all-directories procedure?)

;; search-file-name : string path -> (listof path)
; Returns a list of paths to files within the original directory, whose filenames contain the given string. 
(define (search-file-name name dir-path)
  (append (filter
            (lambda (file) (string-contains? name (path->string (path-filename file))))
            (directory-files dir-path))
  (concat (map
            (lambda (subdirectories) (search-file-name name subdirectories))
            (directory-subdirectories dir-path)))))

(check-satisfied search-file-name procedure?)
(check-expect
 (search-file-name "test" (string->path "test"))
 (list (build-path "test" "test.txt")))
(check-expect
 (search-file-name "bar" (string->path "test"))
 (list (build-path "test" "test_2" "bar.txt")))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 2: Imperative Programming and File Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; backup! : Path Path -> Void
;
; Recursively copies all the files and subdirectories in the `from`
; directory to the `to` directory. This is a modified version of the
; `copy-tree` procedure we discussed in class.
;
; EFFECT: `to` and all its contents now exist
; EFFECT: may overwrite existing files at `to`
(define (backup! from to) 
  (begin
    ; create the destination directory if it doesn't already exist
    (unless (directory-exists? to)
      (make-directory! to))

    ; for each file (leaf node) in the origin directory,
    ; copy it over to the destination directory
    (for-each (λ (file)
                ; print the name of the file being copied into the REPL
                ; for more on how `printf` works, see Appendix 1 in the pdf
        (when (or (not (file-exists? (build-path to
                                                 (path-filename file))))
                  (> (file-or-directory-modify-seconds (build-path from
                                                                   (path-filename file)))
                     (file-or-directory-modify-seconds (build-path to
                                                                   (path-filename file)))))
                (begin
                  (printf "Copying file ~A to ~A~n"
                          file
                          (build-path to (path-filename file)))
                  (copy-file! file
                              (build-path to (path-filename file))
                              #true))))
              (directory-files from))

    ; for each folder (recursive child node) in the origin directory,
    ; recursively `backup!` its contents
    (for-each (λ (subdir)
                (backup! subdir
                         ; add the subdirectory's name to the
                         ; end of the original destination path
                         (build-path to (path-filename subdir))))
              (directory-subdirectories from))))
