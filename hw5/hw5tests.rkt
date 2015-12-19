#lang racket
; Simple tests for hw5
; 
(require "hw5.rkt")
(require rackunit)

; a test case that uses problems 1, 2, and 4
; should produce (list (int 10) (int 11) (int 16))
(define test1
  (mupllist->racketlist
   (eval-exp (call (call mupl-mapAddN (int 7))
                   (racketlist->mupllist 
                    (list (int 3) (int 4) (int 9)))))))
                    


(define tests
  (test-suite
   "Simple tests for hw5."

   ;; mupllist->racketlist and mupl-mapAddN test
   (check-equal? (mupllist->racketlist
                  (eval-exp (call (call mupl-mapAddN (int 7))
                                  (racketlist->mupllist 
                                   (list (int 3) (int 4) (int 9))))))
                 (list (int 10) (int 11) (int 16)) "test1")

   ;; check racketlist to mupllist
   (check-equal? (racketlist->mupllist (list (int 3) (int 4))) (apair (int 3) (apair (int 4) (aunit))) "racketlist->mupllist test")

   ;; check ifgreater
   (check-equal? (eval-exp (ifgreater (int 1) (int 0) (int 1) (int 2))) (int 1) "ifgreater test")

   ;; check mlet
   (check-equal? (eval-exp (mlet "x" (int 8) (add (int 9) (var "x")))) (int 17) "mlet test")

   ;; call test
   (check-equal? (eval-exp (call (closure '() (fun #f "x" (add (var "x") (int 7)))) (int 1))) (int 8) "call test")
   
   ;; snd test
   (check-equal? (eval-exp (snd (apair (int 1) (int 2)))) (int 2) "snd test")
   
   ;; isaunit test
   (check-equal? (eval-exp (isaunit (closure '() (fun #f "x" (aunit))))) (int 0) "isaunit test")

   (check-equal? (eval-exp (isaunit (aunit))) (int 1) "isaunit test2")
   
   ;; ifaunit test
   (check-equal? (eval-exp (ifaunit (int 1) (int 2) (int 3))) (int 3) "ifaunit test")
   
   ;; mlet* test
   (check-equal? (eval-exp (mlet* (list (cons "x" (int 10))) (add (var "x") (int 10))))  (int 20) "mlet* test2")
   
   ;; ifeq test
   (check-equal? (eval-exp (ifeq (int 1) (int 1) (int 3) (int 4))) (int 3) "ifeq test")
   
   
   ))
(require rackunit/text-ui)

(run-tests tests)
   