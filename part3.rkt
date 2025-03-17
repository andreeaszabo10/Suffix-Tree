#lang racket
(require "suffix-tree.rkt")
(require "part1.rkt")
(require "part2.rkt")

(provide (all-defined-out))

;; Această etapă este dedicată aplicațiilor 
;; arborelui de sufixe:
;; - găsirea unui șablon într-un text
;; - cel mai lung subșir comun a două texte
;; - găsirea unui subșir de lungime dată care se
;;   repetă în text
;; Conform convenției din etapele anterioare, un text
;; este întotdeauna reprezentat ca listă de caractere.
;; Rezultatele funcțiilor de mai jos sunt de asemenea
;; reprezentate ca liste de caractere.


; funcția substring? care primește un text și
; un șablon nevid și întoarce true dacă șablonul apare în 
; text, respectiv false în caz contrar.

(define (substring? text pattern)
  (st-has-pattern? (text->ast text) pattern)
  )


; funcția longest-common-substring care primește
; două texte și determină cel mai lung subșir comun al
; acestora, folosind algoritmul următor:
; 1. Construiește arborele de sufixe ST1 pentru primul text.
; 2. Pentru fiecare sufix din al doilea text (de la cel mai
;    lung la cel mai scurt), găsește cea mai lungă potrivire 
;    cu sufixele din primul text, urmând căile relevante în ST1.
; 3. Rezultatul final este cel mai lung rezultat identificat
;    la pasul 2 (în caz de egalitate a lungimii, păstrăm
;    primul șir găsit).

(define (longest-common-substring text1 text2)
  (define st1 (text->cst text1))
  (let loop ((suffixes (get-suffixes (append text2 (list #\$)))) (longest '()) (res '()) (st st1) (len 0))
    (cond
      ((null? suffixes) longest)
      (else
       (define match (match-pattern-with-label st (car suffixes)))
         (cond
           ((equal? (append text1 (list #\$)) (car suffixes)) text1)
           ((boolean? match) (append longest (take (car suffixes) (- (length (car suffixes)) 1))))
           ((false? (car match)) (if (< len (length (append res (car (cdr match)))))
                                     (loop (cdr suffixes) (append res (car (cdr match))) '() st1 (length (append res (car (cdr match)))))
                                    (loop (cdr suffixes) longest '() st1 len)))
           (else (loop (cons (cadr match) (cdr suffixes)) longest (append res (car match)) (caddr match) len))))
      )
    )
  )


; funcția repeated-substring-of-given-length
; care primește un text și un număr natural len și
; parcurge arborele de sufixe al textului până găsește un
; subșir de lungime len care se repetă în text.
; Dacă acest subșir nu există, funcția întoarce false.
; Obs: din felul în care este construit arborele de sufixe
; (pe baza alfabetului sortat), rezultatul va fi primul 
; asemenea subșir din punct de vedere alfabetic.
; Ideea este următoarea: orice cale în arborele de sufixe
; compact care se termină cu un nod intern (un nod care 
; are copii, nu este o frunză) reprezintă un subșir care
; se repetă, pentru că orice asemenea cale reprezintă un
; prefix comun pentru două sau mai multe sufixe ale textului.

(define (repeated-substring-of-given-length text len)
  (define (get-elements list len)
  (cond
    ((= (length list) len) list)
    (else (get-elements (cdr list) len))))
  (get-elements (longest-common-substring text text) len)
  )
