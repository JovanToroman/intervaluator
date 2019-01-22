#lang racket
(require racket/hash)

(struct const (n) #:transparent)
(struct bool (b) #:transparent)
(struct interval (a b) #:transparent)
(struct pair (e1 e2) #:transparent)
(struct nil () #:transparent)

(struct if-then-else (b e1 e2) #:transparent)
(struct is-const? (e) #:transparent)
(struct is-bool? (e) #:transparent)
(struct is-interval? (e) #:transparent)
(struct is-nil? (e) #:transparent)
(struct negate (e) #:transparent)
(struct add (e1 e2) #:transparent)
(struct subtract (e1 e2) #:transparent)
(struct multiply (e1 e2) #:transparent)
(struct divide (e1 e2) #:transparent)
(struct exponentiate (e) #:transparent)
(struct left (e) #:transparent)
(struct right (e) #:transparent)
(struct greater (e1 e2) #:transparent)
(struct lower (e1 e2) #:transparent)
(struct equal (e1 e2) #:transparent)
(struct intersect (e1 e2) #:transparent)
(struct with (vars e) #:transparent)
(struct valof (s) #:transparent)
(struct closure (env f) #:transparent)
(struct function (name farg body) #:transparent)
(struct script (name body) #:transparent)
(struct call (e arg) #:transparent)

(define (my-exp n) (exp n))

(define (iv exp env)
  (cond [(is-const?? exp)
         (let ([val (is-const?-e exp)])
           (if (const? val)
               (if (or (integer? (const-n val)) (real? (const-n val)))
                   (bool #t)
                   (error "Constant must contain real or integer value"))
               (bool #f)))]
        [(not (hash? env))
         (error "Environment must be a hash")]
        [(is-bool?? exp)
         (let ([val (is-bool?-e exp)])
           (if (bool? val)
               (if (boolean? (bool-b val))
                   (bool #t)
                   (error "Bool must contain boolean value"))
               (bool #f)))]
        [(is-interval?? exp)
         (let ([val (is-interval?-e exp)])
           (if (interval? val)
               (if (and (or (integer? (interval-a val)) (real? (interval-a val)))
                        (or (integer? (interval-b val)) (real? (interval-b val)))
                          (>= (interval-b val) (interval-a val)))
                   (bool #t)
                   (error "Interval must contain real or integer boundaries where b >= a"))
               (bool #f)))]
        [(is-nil?? exp)
         (let ([val (is-nil?-e exp)])
           (if (nil? val) (bool #t) (bool #f)))]
        [(const? exp) exp]
        [(bool? exp) exp]
        [(interval? exp) exp]
        [(pair? exp) exp]
        [(nil? exp) exp]
        [(negate? exp)
         (let ([v (iv (negate-e exp) env)])
           (cond [(bool-b (iv (is-const? v) env)) (const (- (const-n v)))]
                 [(bool-b (iv (is-bool? v) env)) (bool (not (bool-b v)))]
                 [(bool-b (iv (is-interval? v) env))
                  (interval (- (interval-b v))
                            (- (interval-a v)))]
                 [#t (error "negation of unexpected expression")]))]
        [(add? exp)
         (let ([v1 (iv (add-e1 exp) env)]
               [v2 (iv (add-e2 exp) env)])
           (cond [(and (bool-b (iv (is-const? v1) env)) (bool-b (iv (is-const? v2) env)))
                  (const (+ (const-n v1) (const-n v2)))]
                 [(and (bool-b (iv (is-interval? v1) env)) (bool-b (iv (is-interval? v2) env)))
                  (interval (+ (interval-a v1) (interval-a v2))
                            (+ (interval-b v1) (interval-b v2)))]
                 [(and (bool-b (iv (is-interval? v1) env)) (bool-b (iv (is-const? v2) env)))
                  (interval (+ (interval-a v1) (const-n v2))
                            (+ (interval-b v1) (const-n v2)))]
                 [(and (bool-b (iv (is-const? v1) env)) (bool-b (iv (is-interval? v2) env)))
                  (interval (+ (interval-a v2) (const-n v1))
                            (+ (interval-b v2) (const-n v1)))]
                 [#t (error "not a number or interval in additi")]))]
        [(subtract? exp)
         (let ([v1 (iv (add-e1 exp) env)]
               [v2 (iv (add-e2 exp) env)])
           (cond [(and (bool-b (iv (is-const? v1) env)) (bool-b (iv (is-const? v2) env)))
                  (const (- (const-n v1) (const-n v2)))]
                 [(and (bool-b (iv (is-interval? v1) env)) (bool-b (iv (is-interval? v2) env)))
                  (interval (- (interval-a v1) (interval-a v2))
                            (- (interval-b v1) (interval-b v2)))]
                 [(and (bool-b (iv (is-interval? v1) env)) (bool-b (iv (is-const? v2) env)))
                  (interval (- (interval-a v1) (const-n v2))
                            (- (interval-b v1) (const-n v2)))]
                 [(and (bool-b (iv (is-const? v1) env)) (bool-b (iv (is-interval? v2) env)))
                  (interval (- (interval-a v2) (const-n v1))
                            (- (interval-b v2) (const-n v1)))]
                 [#t (error "not a number or interval in subtraction")]))]
        [(multiply? exp)
         (let ([v1 (iv (multiply-e1 exp) env)]
               [v2 (iv (multiply-e2 exp) env)])
           (cond [(and (bool-b (iv (is-const? v1) env)) (bool-b (iv (is-const? v2) env)))
                  (const (* (const-n v1) (const-n v2)))]
                 [(and (bool-b (iv (is-interval? v1) env)) (bool-b (iv (is-interval? v2) env)))
                  (interval (min (* (interval-a v1) (interval-a v2))
                                 (* (interval-a v1) (interval-b v2))
                                 (* (interval-b v1) (interval-a v2))
                                 (* (interval-b v1) (interval-b v2)))
                            (max (* (interval-a v1) (interval-a v2))
                                 (* (interval-a v1) (interval-b v2))
                                 (* (interval-b v1) (interval-a v2))
                                 (* (interval-b v1) (interval-b v2))))]
                 [#t (error "Multiply error")]))]
        [(divide? exp)
         (let ([v1 (iv (divide-e1 exp) env)]
               [v2 (iv (divide-e2 exp) env)])
           (cond [(and (bool-b (iv (is-const? v1) env)) (bool-b (iv (is-const? v2) env)))
                  (if (= 0 (const-n v2)) (error "Cannot divide by zero.")
                  (const (/ (const-n v1) (const-n v2))))]
                 [(and (bool-b (iv (is-interval? v1) env)) (bool-b (iv (is-interval? v2) env)))
                  (if (or (= 0 (interval-a v2)) (= 0 (interval-b v2))) (error "Cannot divide by zero.")
                  (interval (min (/ (interval-a v1) (interval-a v2))
                                 (/ (interval-a v1) (interval-b v2))
                                 (/ (interval-b v1) (interval-a v2))
                                 (/ (interval-b v1) (interval-b v2)))
                            (max (/ (interval-a v1) (interval-a v2))
                                 (/ (interval-a v1) (interval-b v2))
                                 (/ (interval-b v1) (interval-a v2))
                                 (/ (interval-b v1) (interval-b v2)))))]
                 [#t (error "Divide error")]))]
        [(exponentiate? exp)
         (let ([v (iv (exponentiate-e exp) env)])
           (cond [(and (bool-b (iv (is-const? v) env)) (const (my-exp (const-n v))))]
               [(bool-b (iv (is-interval? v) env))
                (interval (my-exp (interval-a v))
                          (my-exp (interval-b v)))]
               [#t (error "Exponentiate error")]))]
        [(left? exp)
         (let ([v (iv (left-e exp) env)])
           (cond [(pair? v) (pair-e1 v)]
               [(bool-b (iv (is-interval? v) env)) (interval-a v)]
               [#t (error "Extraction error left")]))]
        [(right? exp)
         (let ([v (iv (right-e exp) env)])
           (cond [(pair? v) (pair-e2 v)]
               [(bool-b (iv (is-interval? v) env)) (interval-b v)]
               [#t (error "Extraction error right")]))]
        [(greater? exp)
         (let ([v1 (iv (greater-e1 exp) env)]
               [v2 (iv (greater-e2 exp) env)])
           (cond [(and (bool-b (iv (is-const? v1) env)) (bool-b (iv (is-const? v2) env)))
                  (bool (> (const-n v1) (const-n v2)))]
                 [(and (bool-b (iv (is-interval? v1) env)) (bool-b (iv (is-interval? v2) env)))
                  (bool (> (- (interval-b v1) (interval-a v1))
                           (- (interval-b v2) (interval-a v2))))]
                 [#t (error "Comparison error")]))]
        [(lower? exp)
         (let ([v1 (iv (lower-e1 exp) env)]
               [v2 (iv (lower-e2 exp) env)])
           (cond [(and (bool-b (iv (is-const? v1) env)) (bool-b (iv (is-const? v2) env)))
                  (bool (< (const-n v1) (const-n v2)))]
                 [(and (bool-b (iv (is-interval? v1) env)) (bool-b (iv (is-interval? v2) env)))
                  (bool (< (- (interval-b v1) (interval-a v1))
                           (- (interval-b v2) (interval-a v2))))]
                 [#t (error "Comparison error")]))]
        [(equal? exp)
         (let ([v1 (iv (equal-e1 exp) env)]
               [v2 (iv (equal-e2 exp) env)])
           (cond [(and (bool-b (iv (is-const? v1) env)) (bool-b (iv (is-const? v2) env)))
                  (bool (= (const-n v1) (const-n v2)))]
                 [(and (bool-b (iv (is-interval? v1) env)) (bool-b (iv (is-interval? v2) env)))
                  (bool (= (- (interval-b v1) (interval-a v1))
                           (- (interval-b v2) (interval-a v2))))]
                 [#t (error "Comparison error")]))]
        [(intersect? exp)
         (let ([v1 (iv (intersect-e1 exp) env)]
                [v2 (iv (intersect-e2 exp) env)])
           (cond [(and (bool-b (iv (is-interval? v1) env)) (bool-b (iv (is-interval? v2) env)))
                  (let ([a1 (interval-a v1)]
                        [b1 (interval-b v1)]
                        [a2 (interval-a v2)]
                        [b2 (interval-b v2)])
                  (cond [(or (< b1 a2) (< b2 a1)) (nil)]
                        [(= b1 a2) (interval b1 b1)]
                        [(= b2 a1) (interval b2 b2)]
                        [(and (= a1 a2) (= b1 b2)) v1]
                        [(and (< a1 a2) (> b1 a2) (< b1 b2))
                         (interval a2 b1)]
                        [(and (< a2 a1) (> b2 a1) (< b2 b1))
                         (interval a1 b2)]
                        [(and (< a1 a2) (> b1 b2) (> b2 a2))
                         (interval a2 b2)]
                        [(and (< a2 a1) (> b2 b1) (> b1 a1))
                         (interval a1 b1)]))]
                 [#t (error "Intersection error")]))]
        [(if-then-else? exp)
         (let ([v-test (iv (if-then-else-b exp) env)])
           (if (bool-b (iv (is-bool? v-test) env))
               (if (bool-b v-test)
                   (iv ( if-then-else-e1 exp) env)
                   (iv ( if-then-else-e2 exp) env))
               (error "condition not a boolean value")))]
        [(with? exp)
         (let ([e (with-e exp)]
               [vars (with-vars exp)])
           (if (hash? vars) (iv e (hash-union vars env))
               (error "Passed environment is not a hash!")))]
        [(valof? exp)
         (let ([s (valof-s exp)])
           (if (string? s) (iv (hash-ref env s (nil)) env) 
               (error "Key value is not a string")))]
        [(function? exp)
         (closure env exp)]
        [(script? exp) exp]
        [(call? exp)
         (let ([e (iv (call-e exp) env)])
           (cond [(closure? e)
                  (let ([body (function-body (closure-f e))]
                        [name (function-name (closure-f e))]
                        [farg (function-farg (closure-f e))]
                        [arg (call-arg exp)]
                        [cenv (closure-env e)]
                        [helphash (make-hash)])
                    (for/list ([(k) (map cons farg arg)]) (hash-set! helphash (car k) (cdr k)))
                    (print "helphash/n")
                    (print helphash)
                    (print "cenv/n")
                    (print cenv)
                    (iv body (hash-set (hash-union! cenv helphash) name (closure-f e))))]
                 [(script? e)
                  (if (bool-b (iv (is-nil? (call-arg exp)) env))
                  (let ([name (script-name e)]
                        [body (script-body e)])
                    (iv body (hash-set env name e)))
                  (error "Script cant take arguments"))]))]
        [#t (error "Intervaluator syntax error")]))
