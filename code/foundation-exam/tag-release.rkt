#lang racket/base

; racket --require tag-release.rkt -- 2025.1 <filename> ...

(require racket/cmdline
         racket/match)

(require "question.rkt"
         "parse.rkt"
         "print.rkt")

(define (parse-release text)
  (match (regexp-match #rx"(20[0-9][0-9])\\.([0-9])" text)
    ((list _ year point)
     (exam-release (string->number year) (string->number point)))))

(define (tag-release release filename)
  (define question (parse-question-file filename))
  (set-question-release! question release)
  (write-question-to-file question filename))

(define (main release-text . filenames)
  (define release (parse-release release-text))
  (for-each (lambda (filename)
              (tag-release release filename))
            filenames))

(apply main (vector->list (current-command-line-arguments)))
