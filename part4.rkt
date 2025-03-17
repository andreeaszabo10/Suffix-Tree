#lang racket
(require "suffix-tree-stream.rkt")
(require "collection.rkt")

(provide (all-defined-out))

;; Pentru că un ST este construit pornind de la o colecție
;; de sufixe și pentru că ne dorim să nu calculăm toate
;; sufixele decât dacă este nevoie, vom modifica toate
;; funcțiile care prelucrau liste de sufixe pentru a
;; prelucra fluxuri de sufixe.
;;
;; Obs: fără această modificare a listelor de sufixe în
;; fluxuri de sufixe, și presupunând că am manipulat
;; arborii de sufixe doar prin interfața definită în
;; fișierul suffix-tree (respectând astfel bariera de 
;; abstractizare), ar trebui să alterăm doar funcția 
;; suffixes->st care este practic un constructor pentru
;; tipul ST.
;; Din cauza transformării listelor de sufixe în fluxuri,
;; avem mult mai multe implementări de modificat.
;; Puteam evita acest lucru? Da, utilizând conceptul de
;; colecție de sufixe de la început (în loc să presupunem
;; că ele vor fi prelucrate ca liste). În loc de cons,
;; car, cdr, map, filter, etc. am fi folosit de fiecare
;; dată collection-cons, collection-first, ... etc. -
;; aceste funcții fiind definite într-o bibliotecă
;; inițială ca fiind echivalentele lor pe liste, și
;; redefinite ulterior în stream-cons, stream-first,
;; ... etc. Operatorii pe colecții de sufixe ar fi 
;; folosit, desigur, doar funcții de tip collection-.
;;
;; Am ales să nu procedăm astfel pentru că ar fi provocat
;; confuzie la momentul respectiv (când chiar operatorii
;; pe liste erau o noutate) și pentru a vă da ocazia să
;; faceți singuri acest "re-design".


; Copiați din etapele anterioare implementările funcțiilor
; de mai jos și modificați-le astfel:
; - Toate funcțiile care lucrează cu liste de sufixe vor
;   lucra cu un nou tip de date Collection, ai cărui
;   constructori și operatori vor fi definiți de voi
;   în fișierul collection.rkt.
; - Pentru toate funcțiile, trebuie să vă asigurați că
;   este respectată bariera de abstractizare (atât în 
;   cazul tipului ST cât și în cazul tipului Collection).
; Obs: cu cât mai multe funcții rămân nemodificate, cu atât
; este mai bine (înseamnă că design-ul inițial a fost bun).

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


; am schimbat, în numele funcției, cuvântul list în
; cuvântul collection
(define (longest-common-prefix-of-collection words)
  (cond
    ((collection-empty? (collection-rest words)) (collection-first words))
    (else
     (define prefix (car (longest-common-prefix (collection-first words) (collection-first (collection-rest words)))))
     (define (helper words prefix)
     (cond
       ((collection-null? words) prefix)
       (else (helper (collection-rest words) (car (longest-common-prefix prefix (collection-first words)))))))
     (helper (collection-rest (collection-rest words)) prefix))))


(define (match-pattern-with-label st pattern)
  (cond
    ((false? (get-ch-branch st (car pattern))) (list #f '()))
    ((equal? (car (longest-common-prefix (car (get-ch-branch st (car pattern))) pattern)) pattern) #t)
    ((equal?
      (car (longest-common-prefix (car (get-ch-branch st (car pattern))) pattern))
      (car (get-ch-branch st (car pattern))))
     (list (car (get-ch-branch st (car pattern)))
           (caddr (longest-common-prefix (get-branch-label (get-ch-branch st (car pattern))) pattern))
           (cdr (get-ch-branch st (car pattern)))))
    (else (list #f (car (longest-common-prefix (car (get-ch-branch st (car pattern))) pattern))))
    )
  )


(define (st-has-pattern? st pattern)
    (cond
      ((equal? (match-pattern-with-label st pattern) #t) #t)
      ((equal? (car (match-pattern-with-label st pattern)) #f) #f)
      (else (st-has-pattern? (caddr (match-pattern-with-label st pattern)) (cadr (match-pattern-with-label st pattern))))))


(define (get-suffixes text)
  (define lista empty-collection)
  (cond
    ((null? text) lista)
    (else (collection-cons text (get-suffixes (cdr text))))))


(define (get-ch-words words ch)
  (collection-filter (lambda (a) (and (not(null? a)) (equal? (car a) ch))) words)
  )

(define (function len suffixes)
             (drop suffixes len)
             )

(define (ast-func suffixes)
  (cond
    ((collection-empty? suffixes) empty-collection)
    (else (cons (list (car (collection-first suffixes))) (collection-map (lambda (a) (cdr a)) suffixes)))))


(define (cst-func suffixes)
  (cond
    ((collection-empty? suffixes) empty-collection)
    (else  (define ch (longest-common-prefix-of-collection suffixes))
           (define len (length ch))
           (define (function len suffixes)
             (drop suffixes len)
             )
           (cons ch (collection-map (lambda(a) (function len a)) suffixes)))))


; considerați că și parametrul alphabet este un flux
; (desigur, și suffixes este un flux, fiind o colecție
; de sufixe)
(define (suffixes->st labeling-func suffixes alphabet)
  (define (not-null? x)
    (not (collection-empty? x)))

  (define (remove-null list)
    (collection-filter (lambda (a) (not-null? a)) list))

  (define (helper suffixes)
    (stream-map (lambda (a) (labeling-func a)) (remove-null (stream-map (lambda (x) (get-ch-words suffixes x)) alphabet))))

  (define (for-each-list-of-suffixes aux)
    (cons (get-branch-label aux) (create (get-branch-subtree aux)))
    )

  (define (create suffixes)
    (collection-map (lambda (aux) (for-each-list-of-suffixes aux)) (helper suffixes)))

  (create suffixes)
  )

(define (list->collection L)
  (if (null? L)
      empty-collection
      (collection-cons (car L) (list->collection (cdr L)))))


; nu uitați să convertiți alfabetul într-un flux
(define (text->st text)
  (lambda (labeling-func) ((lambda (text)
                             (suffixes->st labeling-func (get-suffixes (append text '(#\$)))
                             (list->collection (sort (remove-duplicates (append text '(#\$))) char<?)))) text)))


(define (text->ast text)
  ((text->st text) ast-func))

(define (text->cst text)
  ((text->st text) cst-func))


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (substring? text pattern)
  (st-has-pattern? (text->ast text) pattern))


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (repeated-substring-of-given-length text len)
  (let loop ((st (text->cst text)) (need-len len) (result '()))
    (cond ((st-empty? st) #f)
          ((<= need-len 0) (take result len))
          (else
           (let* ((branch (first-branch st)) (label (get-branch-label branch)) (subtree (get-branch-subtree branch)))
             (or (loop subtree (- need-len (length label)) (append result label))
                 (loop (other-branches st) need-len result)))))))
