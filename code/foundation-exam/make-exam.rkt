#lang racket/base

(require "print-latex.rkt")

(apply main (vector->list (current-command-line-arguments)))
