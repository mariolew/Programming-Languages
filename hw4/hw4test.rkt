#lang racket
;; Simple tests for hw4
;; Just put it into the same folder with hw4.rkt

(require "hw4.rkt")
(require rackunit)

(define a 2)

(define tests
  (test-suite
   "Simple tests for hw4."

   ; sequence test
   (check-equal? (sequence 3 11 2) (list 3 5 7 9 11) "sequence test")

   ; string-append-map test
   (check-equal? (string-append-map (list "Hello" "Vision" "Dan") "dog")
                 (list "Hellodog" "Visiondog" "Dandog") "string-append-map test")

   ; list-nth-mod test
   (check-equal? (list-nth-mod (list 1 2 3 4 5) 5) 1 "list-nth-mod test")

   ; stream-for-n-steps and funny-number-stream test
   (check-equal? (stream-for-n-steps funny-number-stream 10) (list 1 2 3 4 -5 6 7 8 9 -10)
                 "stream-for-n-steps and funny-number-stream test")

   ; dan-then-dog test
   (check-equal? (stream-for-n-steps dan-then-dog 4) (list "dan.jpg" "dog.jpg" "dan.jpg" "dog.jpg") "dan-then-dog test")

   ; stream-add-zero test
   (check-equal? (stream-for-n-steps (stream-add-zero dan-then-dog) 2) (list (cons 0 "dan.jpg") (cons 0 "dog.jpg"))
                 "stream-add-zero test")

   ; cycle-lists test
   (check-equal? (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b")) 3) (list (cons 1 "a") (cons 2 "b") (cons 3 "a")) 
                 "cycle-lists test")

   ; vector-assoc test
   (check-equal? (vector-assoc 4 (vector (cons 5 1) (cons 8 1) (cons 4 1) (cons 21 1))) (cons 4 1) "vector-assoc test")
   
   ; cached-assoc tests
   (check-equal? ((cached-assoc (list (cons 1 2) (cons 2 3)) 3) 1) (cons 1 2) "cached-assoc test")
   
   ; while-less test. Six 'x' should be printed.

   (check-equal? (while-less 7 do (begin (set! a (+ a 1)) (print "x") a)) #t "while-less test")
   (check-equal? (while-less 7 do (begin (set! a (+ a 1)) (print "x") a)) #t "while-less test")
   ))
(require rackunit/text-ui)

(run-tests tests)
   