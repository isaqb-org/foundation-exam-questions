#lang racket/base
(require xml)
(require "question.rkt"
         "parse.rkt"
         "print.rkt")

; Check: 02-04, 03-01

(define lg-mappings
  (list
   (cons (lg-number 1 1) (lg-number 01 01))
   (cons (lg-number 1 2) (lg-number 01 02))
   (cons (lg-number 1 3) (lg-number 01 03))
   (cons (lg-number 1 4) (lg-number 01 04))
   (cons (lg-number 1 9) (lg-number 01 05))
   (cons (lg-number 1 5) (lg-number 01 06))
   (cons (lg-number 1 7) (lg-number 02 01))
   (cons (lg-number 1 8) (lg-number 02 05))
   (cons (lg-number 2 3) (lg-number 02 02))
   (cons (lg-number 4 1) (lg-number 02 03))
   (cons (lg-number 4 2) (lg-number 02 04))
   (cons (lg-number 4 3) (lg-number 03 01))
   (cons (lg-number 4 4) (lg-number 03 01))
   (cons (lg-number 2 8) (lg-number 03 01))
   (cons (lg-number 2 2) (lg-number 03 02))
   (cons (lg-number 2 1) (lg-number 03 03))
   (cons (lg-number 1 6) (lg-number 03 05))
   (cons (lg-number 2 6) (lg-number 03 04))
   (cons (lg-number 2 7) (lg-number 03 05))
   (cons (lg-number 2 9) (lg-number 03 07))
   (cons (lg-number 2 5) (lg-number 03 08))
   (cons (lg-number 2 6) (lg-number 03 09))
   (cons (lg-number 2 4) (lg-number 03 10))
   (cons (lg-number 2 10)  (lg-number 03 11))
   (cons (lg-number 1 11)  (lg-number 03 12))
   (cons (lg-number 3 1)  (lg-number 04 01))
   (cons (lg-number 3 2)  (lg-number 04 02))
   (cons (lg-number 3 3)  (lg-number 04 02))
   (cons (lg-number 3 3)  (lg-number 04 03))
   (cons (lg-number 3 4)  (lg-number 04 05))
   (cons (lg-number 3 5)  (lg-number 04 05))
   (cons (lg-number 3 7)  (lg-number 04 06))
   (cons (lg-number 3 6)  (lg-number 04 07))
   (cons (lg-number 3 8)  (lg-number 04 08))
   (cons (lg-number 3 9)  (lg-number 04 09))
   (cons (lg-number 4 3)  (lg-number 05 02))
   (cons (lg-number 4 4)  (lg-number 05 02))
   (cons (lg-number 5 1)  (lg-number 06 01))
   (cons (lg-number 5 2)  (lg-number 06 02))))

(define (update-questions directory-path)
  (for-each update-question
            (directory-list directory-path #:build? #t)))

(define (update-question question-filename)
  (define question (parse-question-file question-filename))
  (define lgs (question-learning-goals question))
  (set-question-learning-goals!
   question
   (append lgs
           (filter values (map update-lg lgs))))
   
  (call-with-output-file
    question-filename
    (lambda (port)
      (display "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>" port)
      (display-xml (question->document question)
                   port))
    #:exists 'replace))


(define (update-lg lg)
  (cond
   ((and (equal? (learning-goal-reference-curriculum-version lg)
                 (curriculum-version 2023 1))
         (assoc (learning-goal-reference-lg-number lg)
                lg-mappings))
    => (lambda (pair)
         (learning-goal-reference (curriculum-version 2025 1)
                                  (cdr pair))))
   (else #f)))
