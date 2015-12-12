
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; warm-up part
(define (sequence low high stride)
  (if (> low high)
     null
     (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (str) (string-append str suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
             [(null? xs) (error "list-nth-mod: empty list")]
             [#t (car (list-tail xs (remainder n (length xs))))]))

;; function 4
(define (stream-for-n-steps s n)
  (if (= n 0)
     null
     (let ([pr (s)])
           (cons (car pr) (stream-for-n-steps (cdr pr) (- n 1))))))

;; function 5, produce a stream
(define funny-number-stream
  (letrec ([f (lambda (x)
                (if (= (remainder x 5) 0)
                   (cons (- 0 x) (lambda () (f (+ x 1))))
                   (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

;; function 6
(define dan-then-dog
  (letrec ([dan (lambda () (cons "dan.jpg" dog))]
           [dog (lambda () (cons "dog.jpg" dan))])
    dan))

;; function 7
(define (stream-add-zero s)
  (letrec ([f (lambda (x)
                (cons (cons 0 (car x)) (lambda ()(f ((cdr x))))))])
    (lambda () (f (s)))))

;; function 8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
              (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
                          (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

;; function 9
(define (vector-assoc v vec)
  (letrec ([len (vector-length vec)]
           [f (lambda (n)
                (if (= len n) #f
                    (let ([nv (vector-ref vec n)])
                      (if (and (pair? nv) (= (car nv) v))
                          nv
                          (f (+ n 1))))))])
    (f 0)))

;; function 10
(define (cached-assoc xs n)
  (letrec([memo (make-vector n #f)]
          [position 0]
          [f (lambda (v)
               (let ([ans (vector-assoc v memo)])
                 (if ans
                     ans
                     (let ([new-ans (assoc v xs)])
                       (if new-ans
                           (begin (vector-set! memo position new-ans)
                                  (set! position (remainder (+ position 1) n))
                                  new-ans) #f)))))])
    f))


;; macro 11
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (begin (letrec ([e e1]
                  [f (lambda()
                     (if (> e e2) (f)
                         #t))])
              (f)))]))

;; challenges. Although, silly way.
(define (cycle-lists-challenge xs ys)
  (letrec ([memo1 null]
           [memo2 null]
           [f (lambda ()
              (cond [(and (null? memo1) (null? memo2))
                     (begin
                       (set! memo1 (cdr xs))
                       (set! memo2 (cdr ys))
                       (cons (cons (car xs) (car ys)) f))]
                    [(null? memo1)
                     (begin
                       (set! memo1 (cdr xs))
                       (let ([m (car memo2)])
                         (begin (set! memo2 (cdr memo2))
                                (cons (cons (car xs) m) f))))]
                    [(null? memo2)
                     (begin
                       (set! memo2 (cdr ys))
                       (let ([m (car memo1)])
                         (begin (set! memo1 (cdr memo1))
                                (cons (cons m (car ys)) f))))]
                    [#t (begin
                          (let ([m1 (car memo1)]
                                [m2 (car memo2)])
                                (begin
                                  (set! memo1 (cdr memo1))
                                  (set! memo2 (cdr memo2))
                                  (cons (cons m1 m2) f))))]))])
    f))
                          
    
                  
                  
                    


                

