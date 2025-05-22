#lang racket/base
(require racket/contract)
         
(struct question
  (id
   points
   history
   learning-goals
   stem
   explanation)
  #:transparent #:mutable)

(struct pick-question question
  (pick-options)
  #:transparent #:mutable)

(struct category-question question
  (categories statements)
  #:transparent #:mutable)

(struct history-item
  (date text)
  #:transparent #:mutable)

(struct learning-goal-reference
  (curriculum-version
   lg-number)
  #:transparent #:mutable)

(struct curriculum-version
  (year number)
  #:transparent #:mutable)

(struct lg-number
  (section index)
  #:transparent #:mutable)

(struct localized-text
  (langs+texts) ; assoc list
  #:transparent #:mutable)

(define lang/c
  (or/c "en" "de")) ; at the moment

(struct pick-option
  (validity ; 'distractor, or 'correct
   identifier
   text)
  #:transparent #:mutable)

(struct category
  (label text)
  #:transparent #:mutable)

(struct statement
  (correct-category identifier text)
  #:transparent #:mutable)

(provide
 (contract-out
  (struct question
    ((id string?)
     (points (integer-in 1 #f))
     (history (listof history-item?))
     (learning-goals (listof learning-goal-reference?))
     (stem localized-text?)
     (explanation (or/c localized-text? #f))))

  (struct pick-question
    ((id string?)
     (points (integer-in 1 #f))
     (history (listof history-item?))
     (learning-goals (listof learning-goal-reference?))
     (stem localized-text?)
     (explanation (or/c localized-text? #f))
     (pick-options (listof pick-option?))))

  (struct category-question
    ((id string?)
     (points (integer-in 1 #f))
     (history (listof history-item?))
     (learning-goals (listof learning-goal-reference?))
     (stem localized-text?)
     (explanation (or/c localized-text? #f))
     (categories (listof category?))
     (statements (listof statement?))))

  (struct history-item
    ((date date?)
     (text string?)))

  (struct learning-goal-reference
    ((curriculum-version curriculum-version?)
     (lg-number (or/c lg-number? 'prerequisite))))

  (struct curriculum-version
    ((year integer?)
     (number integer?)))

  (struct lg-number
    ((section integer?)
     (index integer?)))

  (struct localized-text
    ((langs+texts (listof (cons/c lang/c string?)))))

  (lang/c contract?)

  (struct pick-option
    ((validity (or/c 'distractor 'correct))
     (identifier string?)
     (text localized-text?)))

  (struct category
    ((label string?)
     (text localized-text?)))

  (struct statement
    ((correct-category category?)
     (identifier string?)
     (text localized-text?)))

  ))

  
  
  
