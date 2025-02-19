#lang racket/base

(require racket/match
         racket/contract
         (only-in srfi/19 date->string)
         xml)
(require "question.rkt")

(define (element* name attributes content)
  (element #f #f name attributes content))

(define (attribute* name value)
  (attribute #f #f name value))

(define (pcdata* string)
  (list (pcdata #f #f string)))

; inverse of parse-question-document
(define (question->document question)
  (document (prolog '() #f '())
            (question->element question)
            '()))

(define (question->element question)
  (match question
    ((pick-question id points history learning-goals stem explanation pick-options)
     (element* 'pickQuestion
               (list (attribute* 'id id)
                     (attribute* 'points (number->string points)))
               (list
                (history->element history)
                (learning-goals->element learning-goals)
                (stem->element stem)
                (explanation->element explanation)
                (pick-options->element pick-options))))
    ((category-question id points history learning-goals stem explanation categories statements)
     (element* 'categoryQuestion
               (list (attribute* 'id id)
                     (attribute* 'points (number->string points)))
               (list
                (history->element history)
                (learning-goals->element learning-goals)
                (stem->element stem)
                (explanation->element explanation)
                (element* 'categoryStatements
                          '()
                          (list (categories->element categories)
                                (statements->element statements))))))))


(define (history->element history)
  (element* 'history '()
            (map history-item->element history)))

(define (history-item->element item)
  (match item
    ((history-item date text)
     (element* 'item
               (list (attribute* 'date (date->string date "~Y-~m-~d")))
               (pcdata* text)))))

(define (learning-goals->element lgs)
  (element* 'refersToLgs
            '()
            (map learning-goal-reference->element lgs)))
            
(define (learning-goal-reference->element lg-ref)
  (match lg-ref
    ((learning-goal-reference curriculum-version lg-number)
     (element* 'lg
               (list (attribute* 'curriculumVersion (curriculum-version->string curriculum-version))
                     (attribute* 'lg (lg-number->string lg-number)))
               '()))))

(define (curriculum-version->string curriculum-version)
  (string-append (number->string (curriculum-version-year curriculum-version))
                 "."
                 (number->string (curriculum-version-number curriculum-version))))

(define (lg-number->string lg-number)
  (string-append (number->string (lg-number-section lg-number))
                 "-"
                 (number->string (lg-number-index lg-number))))
                     

(define (stem->element stem)
  (element* 'stem
            '()
            (localized-text->elements stem)))

(define (localized-text->elements text)
  (map (lambda (lang+text)
         (element* 'text
                   (list (attribute* 'xml:lang (car lang+text)))
                   (pcdata* (cdr lang+text))))
       (localized-text-langs+texts text)))


(define (explanation->element explanation)
  (element* 'explanation
            '()
            (localized-text->elements explanation)))

(define (pick-options->element pick-options)
  (element* 'pickOptions
            '()
            (map pick-option->element pick-options)))

(define (pick-option->element o)
  (match o
    ((pick-option validity identifier texts)
     (element* 'option
               (list (attribute* validity (symbol->string validity))
                     (attribute* 'identifier identifier))
               (localized-text->elements texts)))))


(define (categories->element categories)
  (element* 'categories
            '()
            (map category->element categories)))

(define (category->element c)
  (match c
    ((category label text)
     (element* 'category
               (list (attribute* 'label label))
               (localized-text->elements text)))))

(define (statements->element statements)
  (element* 'statements
            '()
            (map statement->element statements)))

(define (statement->element s)
  (match s
    ((statement correct-category identifier text)
     (element* 'statement
               (list
                (attribute* 'correctCategory (category-label correct-category))
                (attribute* 'identifier identifier))
               (localized-text->elements text)))))

(module+ test
  (require rackunit)
  (require "parse.rkt")

  (define q1
    (pick-question
     "Q-20-04-01"
     1
     (list (history-item (date* 0 0 0 8 8 2024 4 220 #f 7200 0 "") "content identical to mock exam 2024.1-rev0-DE-20240202"))
     (list (learning-goal-reference (curriculum-version 2025 1) (lg-number 1 1)) (learning-goal-reference (curriculum-version 2023 1) (lg-number 1 1)))
     (localized-text
      '(("de" . "\n        Wie viele Definitionen des Begriffes \"Softwarearchitektur\" gibt es?\n    ")
        ("en" . "\n        How many definitions of “software architecture” exist?\n    ")))
     (localized-text
      '(("de"
         .
         "\n      Die Vielfalt der Definitionen von Softwarearchitektur resultiert unter anderem aus unterschiedlichen Perspektiven, Zielgruppen und Entwicklungsmethoden.\n    ")
        ("en"
         .
         "\n      The variety of definitions of software architecture results, among other things, from different perspectives, target groups and development methods.\n    ")))
     (list
      (pick-option
       'false
       "A"
       (localized-text
        '(("de" . "\n        Genau eine für alle Arten von Systemen.\n      ") ("en" . "\n        Exactly one for all kinds of systems.\n      "))))
      (pick-option
       'false
       "B"
       (localized-text
        '(("de"
           .
           "\n        Eine für jede Art von Softwaresystem (z. B. \"eingebettet\",\"Echtzeit\", \"Entscheidungsunterstützung\", \"Web\", \"Batch\", …).\n      ")
          ("en" . "\n        One for every kind of software system (e.g. “embedded”, “real-time”, “decision support”, “web”, “batch”, …).\n      "))))
      (pick-option
       'correct
       "C"
       (localized-text
        '(("de" . "\n        Ein Dutzend oder mehr unterschiedliche Definitionen.\n      ")
          ("en" . "\n        A dozen ore more different definitions.\n      ")))))))

  (define q4
    (category-question
     "Q-17-13-02"
     2
     (list (history-item (date* 0 0 0 8 8 2024 4 220 #f 7200 0 "") "content identical to mock exam 2024.1-rev0-DE-20240202"))
     (list (learning-goal-reference (curriculum-version 2023 1) (lg-number 3 2)))
     (localized-text
      '(("de"
         .
         "\n      Bei Ihrem Projekt arbeiten drei Architekt:innen und sieben\n      Entwickler:innen an der Dokumentation der\n      Softwarearchitektur. Welche Methoden eignen sich zur\n      Gewährleistung einer konsistenten und zweckmäßigen Dokumentation\n      und welche nicht?\n    ")
        ("en"
         .
         "\n      In your project, three architects and seven developers are working on the\n      documentation of the software architecture. Which methods are\n      appropriate in order to achieve a consistent and adequate\n      documentation, and which are not?\n    ")))
     (localized-text
      '(("de"
         .
         "\n      Um eine konsistente und zweckmäßige Dokumentation sicherzustellen, eignet sich die Koordination durch die leitende Architekt:in sowie die Verwendung identischer Vorlagen. Die automatische Extraktion aller Teile der Dokumentation aus dem Quellcode ist weniger empfehlenswert, da dies möglicherweise nicht alle relevanten Informationen oder Kontexte berücksichtigt und die Qualität der Dokumentation beeinträchtigen kann.\n      Aspekte wie Begründungen oder Alternativen sind nicht im Code enthalten, sondern müssen in die Dokumentation aufgenommen werden, daher können nicht alle Teile der Dokumentation aus dem Quellcode extrahiert werden. \n    ")
        ("en"
         .
         "\n      To ensure consistent and useful documentation, coordination by the lead architect and the use of identical templates are recommended. Automatic extraction of all parts of the documentation from the source code is less recommended, as this may not include all relevant information or contexts and may affect the quality of the documentation.\n      Things like reasoning or alternatives won’t be contained in code, but need to be included in documentation, therefore not all parts of documentation can be extracted from source code. \n    ")))
     (list
      (category "a" (localized-text '(("de" . "Geeignet") ("en" . "appropriate"))))
      (category "b" (localized-text '(("de" . "Nicht geeignet") ("en" . "not appropriate")))))
     (list
      (statement
       (category "a" (localized-text '(("de" . "Geeignet") ("en" . "appropriate"))))
       "A"
       (localized-text
        '(("de" . "\n          Die/der leitende Architekt:in koordiniert die Erstellung der Dokumentation.\n        ")
          ("en" . "\n          The lead architect coordinates the creation of the documentation.\n        "))))
      (statement
       (category "a" (localized-text '(("de" . "Geeignet") ("en" . "appropriate"))))
       "B"
       (localized-text
        '(("de" . "\n          Für die Dokumentation werden identische Vorlagen verwendet.\n        ")
          ("en" . "\n          Identical templates are used for the documentation.\n        "))))
      (statement
       (category "b" (localized-text '(("de" . "Nicht geeignet") ("en" . "not appropriate"))))
       "C"
       (localized-text
        '(("de" . "\n          Alle Teile der Dokumentation werden automatisch aus dem Quellcode extrahiert.\n        ")
          ("en" . "\n          All parts of the documentation are automatically extracted from the source code.\n        ")))))))

  (check-equal? (parse-question-document (question->document q1))
                q1)

  (check-equal? (parse-question-document (question->document q4))
                q4))
                                      

(provide (contract-out
          (question->document (question? . -> . document?))))
