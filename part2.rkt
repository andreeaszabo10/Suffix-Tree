#lang racket
(require "suffix-tree.rkt")
(require "part.rkt")

(provide (all-defined-out))

;; e definit algoritmul de construcție a unui
;; arbore de sufixe (atât cel compact, cât și cel atomic) pe
;; baza unui text și a unui alfabet dat. Se știe că textul
;; utilizează doar simboluri prezente în alfabet.
;; Abordarea este următoarea:
;; 1. obțin toate sufixele textului
;; 2. pentru fiecare caracter din alfabet, determin sufixele
;;    care încep cu acel caracter: ele vor fi grupate în câte
;;    o ramură a arborelui de sufixe (exceptând caracterele
;;    care nu apar in text - acestea nu generează ramuri)
;; 3. pentru fiecare listă de sufixe care încep cu un același
;;    caracter, determin eticheta ramurii, respectiv noile
;;    sufixe care vor genera subarborele de sub etichetă
;;    - în cazul unui AST (atomic suffix tree), eticheta este
;;      chiar primul caracter, iar noile sufixe sunt vechile
;;      sufixe fără primul caracter
;;    - în cazul unui CST (compact suffix tree), eticheta este
;;      cel mai lung prefix comun al sufixelor, iar sufixele
;;      noi se obțin din cele vechi prin îndepărtarea acestui
;;      prefix
;; 4. transform fiecare rezultat de la pasul 3 într-o ramură
;;    - eticheta este deja calculată
;;    - calculul subarborilor se realizează repetând pașii 2-4
;;      pentru noile sufixe


; funcție recursiva care primește un text (listă 
; de caractere) și determină lista tuturor sufixelor acestuia
; (de la cel mai lung la cel mai scurt).
; Se știe că textul se va termina cu caracterul special "$", 
; și cu acest caracter trebuie să se termine și toate sufixele
; din lista rezultat (de la întreg textul urmat de "$" până la
; sufixul vid - reprezentat de un singur "$").
; ex:
; (get-suffixes '(#\w #\h #\y #\$))
; => '((#\w #\h #\y #\$) (#\h #\y #\$) (#\y #\$) (#\$))
; recursivitate pe stivă

(define (get-suffixes text)
  (define lista '())
  (cond
    ((null? text) lista)
    (else (append lista (append (list text) (get-suffixes (cdr text)))))
    )
  )


; funcție care primește o listă de cuvinte 
; și un caracter ch și întoarce acele cuvinte din listă care 
; încep cu caracterul ch.
; cu functionale

(define (get-ch-words words ch)
  (filter (lambda (a) (and (not(null? a)) (equal? (car a) ch))) words)
  )


; funcție care primește o listă nevidă de sufixe 
; care încep cu același caracter și calculează perechea
; (etichetă AST pentru aceste sufixe, lista noilor sufixe).

(define (ast-func suffixes)
  (cond
    ((null? suffixes) '())
    (else (cons (list (caar suffixes)) (map (lambda (a) (cdr a)) suffixes)))))


; funcție care primește o listă nevidă de sufixe 
; care încep cu același caracter și calculează perechea
; (etichetă CST pentru aceste sufixe, lista noilor sufixe).

(define (cst-func suffixes)
  (cond
    ((null? suffixes) '())
    (else  (define ch (longest-common-prefix-of-list suffixes))
           (define len (length ch))
           (define (function len suffixes)
             (drop suffixes len)
             )
           (cons ch (map (lambda(a) (function len a)) suffixes)))))



; funcția suffixes->st construiește un
; arbore de sufixe pe baza unei liste de sufixe, a unui 
; alfabet (listă de caractere care include toate caracterele
; din sufixe), și a unei funcții care indică modul de
; etichetare (atomic sau compact).
; Când argumentul funcție va fi ast-func, se va obține un AST.
; Când argumentul funcție va fi cst-func, se va obține un CST.

(define (suffixes->st labeling-func suffixes alphabet)
  (define (not-null? x)
    (not (null? x)))

  (define (remove-null list)
    (filter (lambda (a) (not-null? a)) list))

  (define (helper suffixes)
    (remove-null (map (lambda (x) (remove-null (labeling-func (get-ch-words suffixes x)))) alphabet)))

  (define (for-each-list-of-suffixes aux acc)
    (cons (append '() (list (car aux)) (create (cdr aux))) acc)
    )

  (define (create suffixes)
    (foldr (lambda (aux acc) (for-each-list-of-suffixes aux acc)) '() (helper suffixes)))

  (create suffixes)
  )



; funcția text->st primește un text
; (listă de caractere) și o funcție de etichetare și întoarce
; arborele de sufixe corespunzând acestui text cu această
; metodă de etichetare.
; Pași:
; - obținem sufixele textului la care adăugăm marcajul final $
; - obținem alfabetul sortat asociat textului prin utilizarea
;   funcțiilor de bibliotecă sort, remove-duplicates, char<?
;   (inclusiv caracterul $)
; - apelez corespunzător funcția suffixes->st

(define (text->st text)
  (lambda (labeling-func) ((lambda (text) (suffixes->st labeling-func (get-suffixes (append text '(#\$))) (sort (remove-duplicates (append text '(#\$))) char<?))) text)))

; din funcția text->st deriva funcția text->ast care
; primește un text (listă de caractere) și întoarce AST-ul
; asociat textului.

(define (text->ast text)
  ((text->st text) ast-func))

; din funcția text->st derivați funcția text->cst care
; primește un text (listă de caractere) și întoarce CST-ul
; asociat textului.

(define (text->cst text)
  ((text->st text) cst-func))
