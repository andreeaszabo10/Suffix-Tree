#lang racket
(require "suffix-tree.rkt")

(provide (all-defined-out))

; funcție care primește două cuvinte (liste
; de caractere) w1 și w2 și calculează cel mai lung prefix
; comun al acestora, împreună cu restul celor două cuvinte
; după eliminarea prefixului comun.
; ex:
; (longest-common-prefix '(#\w #\h #\y) '(#\w #\h #\e #\n))
; => '((#\w #\h) (#\y) (#\e #\n))
; cu recursivitate pe coada

(define (longest-common-prefix w1 w2)
  (define (common w1 w2 prefix)
    (cond
      ((or (null? w1) (null? w2)) (list prefix w1 w2))
      ((or (not (list? w1)) (not (list? w2))) (list prefix w1 w2))
      ((equal? (car w1) (car w2)) (common (cdr w1) (cdr w2) (append prefix (list (car w1)))))
      (else (list prefix w1 w2))
      )
    )
  (common w1 w2 '()))


; funcție recursiva care primește o listă nevidă 
; de cuvinte care încep cu același caracter și calculează cel 
; mai lung prefix comun al acestora.
; căutarea e oprita (parcurgerea) în momentul în care aveți garanția 
; că prefixul comun curent este prefixul comun final.

(define (longest-common-prefix-of-list words)
  (cond
    ((equal? (length words) 1) (car words))
  (else
   (define prefix (car (longest-common-prefix (car words) (cadr words))))
   (define (helper words prefix)
     (cond
       ((null? words) prefix)
       (else (helper (cdr words) (car (longest-common-prefix prefix (car words)))))
      )
    )
   (helper (cddr words) prefix))
  )
 )


;; Următoarele două funcții sunt utile căutării unui șablon
;; (pattern) într-un text cu ajutorul arborelui de sufixe.
;; Ideea de căutare este următoarea:
;; - dacă șablonul există în text, atunci există un sufix care
;;   începe cu acest șablon, deci există o cale care începe din
;;   rădăcina arborelui care se potrivește cu șablonul
;; - vom căuta ramura a cărei etichetă începe cu prima literă
;;   din șablon
;; - dacă nu găsim această ramură, șablonul nu apare în text
;; - dacă șablonul este conținut integral în eticheta ramurii,
;;   atunci el apare în text
;; - dacă șablonul se potrivește cu eticheta dar nu este conținut
;;   în ea (de exemplu șablonul "nana$" se potrivește cu eticheta
;;   "na"), atunci continuăm căutarea în subarborele ramurii
;; - dacă șablonul nu se potrivește cu eticheta (de exemplu
;;   șablonul "numai" nu se potrivește cu eticheta "na"), atunci
;;   el nu apare în text (altfel, eticheta ar fi fost "n", nu
;;   "na", pentru că eticheta este cel mai lung prefix comun al
;;   sufixelor din subarborele său)


; funcția match-pattern-with-label care primește un
; arbore de sufixe și un șablon nevid și realizează un singur pas 
; din procesul prezentat mai sus - identifică ramura arborelui a
; cărei etichetă începe cu prima literă din șablon, apoi
; determină cât de bine se potrivește șablonul cu eticheta,
; întorcând ca rezultat:
; - true, dacă șablonul este conținut integral în etichetă
; - lista (etichetă, nou pattern, subarbore), dacă șablonul se
;   potrivește cu eticheta dar nu este conținut în ea
;   (ex: ("na", "na$", subarborele de sub eticheta "na")
;   pentru șablonul inițial "nana$" și eticheta "na")
; - lista (false, cel mai lung prefix comun între etichetă și
;   șablon), dacă șablonul nu s-a potrivit cu eticheta sau nu
;   s-a găsit din start o etichetă care începe cu litera dorită
;   (ex1: (false, "n") pentru șablonul "numai" și eticheta "na")
;   (ex2: (false, "") pentru etichetă negăsită)

(define (match-pattern-with-label st pattern)
  (cond
    ((false? (get-ch-branch st (car pattern))) (list #f '()))
    ((equal? (car (longest-common-prefix (car (get-ch-branch st (car pattern))) pattern)) pattern) #t)
    ((equal? (car (longest-common-prefix (car (get-ch-branch st (car pattern))) pattern)) (car (get-ch-branch st (car pattern))))
     (list (car (get-ch-branch st (car pattern))) (caddr (longest-common-prefix (get-branch-label (get-ch-branch st (car pattern))) pattern)) (cdr (get-ch-branch st (car pattern)))))
    (else (list #f (car (longest-common-prefix (car (get-ch-branch st (car pattern))) pattern))))
    )
  )

; funcția st-has-pattern? care primește un
; arbore de sufixe și un șablon și întoarce true dacă șablonul
; apare în arbore, respectiv false în caz contrar.

(define (st-has-pattern? st pattern)
    (cond
      ((equal? (match-pattern-with-label st pattern) #t) #t)
      ((equal? (car (match-pattern-with-label st pattern)) #f) #f)
      (else (st-has-pattern? (caddr (match-pattern-with-label st pattern)) (cadr (match-pattern-with-label st pattern))))))
