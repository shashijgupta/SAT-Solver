#lang racket

(require "utilities.rkt")

(define assign #hash())

(define unitexp 0)

  (define (unitfinder exp)
    (cond[(And? exp) (or( unitfinder (And-x exp)) ( unitfinder (And-y exp)))]
         [(Or? exp) #f]
         [(Const? exp) #f]
         [else  (begin (set! unitexp exp) #t)]))
  
  (define (removeunit exp unit)
    (define (helper1 rem ex)
      (cond[(And? ex) (if (or (equal? rem (And-x ex)) (equal? rem (And-y ex))) (Const #f) 
                         (And (helper1 rem (And-x ex)) (helper1 rem (And-y ex))))]
           [(Or? ex) (cond 
                          [(equal? rem (Or-x ex)) (helper1 rem (Or-y ex))]
                          [(equal? rem (Or-y ex)) (Or-x ex)]
                          [else (Or (Or-x ex) (helper1 rem (Or-y ex)))])]
           [(equal? rem ex) (Const #f)]
           [else ex]))
    (define (helper2 rem ex)
      (cond[(Const? ex) ex]
           [(And? ex) (cond[(found? rem (And-x ex)) (helper2 rem (And-y ex))]
                           [(found? rem (And-y ex)) (helper2 rem (And-x ex))]
                           [else (And (And-x ex) (helper2 rem (And-y ex)))])]
           [(found? rem ex) (Const #t)]
           [else ex])) 


                       
    (cond[(Not? unit) (helper2 unit (helper1 (Not-e unit) exp) )]
         [else (helper2 unit (helper1 (Not unit) exp) )]))

  
  (define (found? rem ex)
    (cond [(Or? ex) (if (equal? rem (Or-x ex)) #t (found? rem (Or-y ex)))]
          [(equal? ex rem)  #t]
          [else #f]))
  
  (define alpha #t)

  (define find-var 0)

  (define (parse-finder ex var)
    (cond[(And? ex) (or(parse-finder (And-x ex) var) (parse-finder (And-y ex) var))]
         [(Or? ex) (or(parse-finder (Or-x ex) var) (parse-finder (Or-y ex) var))]
         [(Var? ex) (if (equal? ex var) #t #f)]
         [(Not? ex) (if (equal? ex var) #t #f)]
         [else #f]))
  
 (define (linear-removal lis ex)
    (define (finder e)
      (cond[(And? e) (or (finder (And-x e)) (finder (And-y e)))]
           [(Or? e) (or (finder (Or-x e)) (finder (Or-y e)))]
           [(Var? e) (if (list-finder lis e) #f
                         (if (parse-finder ex (Not e)) (begin (set! lis (append (list e (Not e)) lis)) #f)
                              (begin (set! find-var e) (set! assign (dict-set assign (Var-lit e) #t)) (set! lis (append (list e (Not e)) lis)) #t)))]
           [(Not? e) (if (list-finder lis e) #f
                      (if (parse-finder ex (Not-e e)) (begin (set! lis (append (list e (Not-e e)) lis)) #f) 
                         (begin (set! find-var e) (set! assign (dict-set assign (Var-lit (Not-e e)) #f)) (set! lis (append (list e (Not-e e)) lis)) #t)))]
           [else #f]))
   

                      
    (define (helper e)
             (cond[(And? e) (cond [(found? find-var (And-x e)) (helper (And-y e))]
                                  [(found? find-var (And-y e)) (helper (And-x e))] 
                                  [else(And (And-x e) (helper (And-y e)))])]
                  [(equal? e find-var) (Const #t)]
                  [else e]))
                  
      
    (if (finder ex) (linear-removal lis (helper ex)) ex))
  (define (list-finder l a)
    (if (null? l) #f (if (equal? (car l) a) #t (list-finder (cdr l) a))))

(define last-var 0)

(define (last-finder exp)
  (cond[(And? exp) (or (last-finder (And-x exp)) (last-finder (And-y exp)))]
       [(Or? exp) (or (last-finder (Or-x exp)) (last-finder (Or-y exp)))]
       [(or (Var? exp) (Not? exp)) (begin (set! last-var exp) #t)]
       [else #f]))

(define (remove-const exp)
  (cond[(And? exp) (if (Const? (And-x exp)) (if (equal? (And-x exp) (Const #f)) (Const #f)
                                                                                (remove-const (And-y exp)))
                                            (And (remove-const (And-x exp)) (remove-const (And-y exp))))]
       [(Or? exp) (if (found? (Const #t) exp) (Const #t) (const #f))]
       [else exp]))

  (define (dpll parseexp)
   (define (dpll1 parseexp) (cond [(Const? parseexp) (if (Const-bool parseexp) (Const-bool parseexp)
                                                       #f)]
        [(Var? parseexp) (begin (set! assign (dict-set assign (Var-lit parseexp) #t)) (set! alpha #t) #t)]
        [(Not? parseexp) (begin (set! assign (dict-set assign (Var-lit (Not-e parseexp)) #f)) (set! alpha #t) #t)]
        [(unitfinder parseexp)  (let*[(unit unitexp)] (cond[ (Var? unit) (begin (set! assign (dict-set assign (Var-lit unit) #t)) (set! alpha #t) (dpll1 (removeunit parseexp unit)))]
                                                           [else (begin (set! assign (dict-set assign (Var-lit (Not-e unit)) #f)) (set! alpha #t) (dpll1 (removeunit parseexp unit)))]))]
       [alpha (begin (set! alpha #f) (dpll1 (linear-removal '() parseexp)))]
       [(last-finder parseexp) (begin (set! alpha #t) (let*[(a last-var)]
                                 (cond[ (dpll1 (removeunit parseexp a)) (if (Var? a) (begin (set! assign (dict-set assign (Var-lit a) #t)) #t)
                                                           (begin (set! assign (dict-set assign (Var-lit (Not-e a)) #f)) #t))]
                                      [(Var? a) (if (dpll1 (removeunit parseexp (Not a))) (begin (set! assign (dict-set assign (Var-lit a) #f)) #t)
                                                            #f)]
                                      [else  (if (dpll1 (removeunit parseexp (Not-e a))) (begin (set! assign (dict-set assign (Var-lit (Not-e a)) #t)) #t)
                                                             #f)]
                                      )))]
       [else (dpll1 (remove-const parseexp))]))
    (begin (set! assign #hash()) (let*[(x(dpll1 parseexp))]
      (if x #t (begin (set! assign #hash()) #f)))))
                                                                 
        
        