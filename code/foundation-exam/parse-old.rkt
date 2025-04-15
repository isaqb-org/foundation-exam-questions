#lang racket/base

; Parse exam questions from pre-2025 XML format

(require racket/match racket/date racket/set
         (only-in racket/string string-split string-join)
         xml
         (only-in srfi/19 string->date time<? date->time-utc)
         racket/contract
         (only-in racket/list filter-map))
(require "question.rkt"
         "match-xml.rkt"
         "print.rkt")

(struct old-question
  (id refers-to history points stem-paragraphs)
  #:transparent)

(struct old-p-question old-question
  (options)
  #:transparent)

(struct old-k-question old-question
  (categories options)
  #:transparent)

(struct old-a-question old-question
  (options)
  #:transparent)

(struct old-category
  (label text)
  #:transparent)

(struct k-option
  (label ; "a" or "b"
   text)
  #:transparent)

(struct ap-option
  (kind ; 'distractor, 'correct
   text)
  #:transparent)

(struct exn:parse-error exn:fail
  (base-exn filename))

(define (assert-equal? a b)
  (unless (equal? a b)
    (error 'assert-equal? "not equal but should be: ~a and ~a" a b)))

(define (list-destructurer f)
  (lambda (list)
    (apply f list)))

(define (split-out-date text)
  (cond
    ((regexp-match #rx"^([0-9][0-9]-[0-9][0-9]-[0-9][0-9])[ :.,-]+(.*)$"
                   text)
     => (list-destructurer (lambda (_all date payload)
                             (values (string->date date "~y-~m-~d")
                                     payload))))
    ((regexp-match #rx"^([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9])[ :.,-]+(.*)$"
                   text)
     => (list-destructurer (lambda (_all date payload)
                             (values (string->date date "~Y-~m-~d")
                                     payload))))
    (else (error 'split-out-date "history item without date ~a" text))))

(define (skip-history? text)
  (or (regexp-match? #rx"^Old-ID:" text)
      (regexp-match? #rx"^New-ID:" text)))

; prefer en version
(define (merge-history en-history de-history)
  (define (pairs history)
    (filter-map (lambda (text)
                  (if (skip-history? text)
                      #f
                      (call-with-values (lambda () (split-out-date text)) cons)))
                history))
  (let ((en-pairs (pairs en-history))
        (de-pairs (pairs de-history)))
    (let* ((en-dates (list->set (map car en-pairs)))
           (de-unique-pairs
            (filter (lambda (pair)
                      (not (set-member? en-dates (car pair))))
                    de-pairs))
           (all-pairs
            (sort
             (append en-pairs de-unique-pairs)
             #:key car
             (lambda (date1 date2)
               (time<? (date->time-utc date1)
                       (date->time-utc date2))))))
      (map (lambda (pair)
             (history-item (car pair) (cdr pair)))
           all-pairs))))

(define *reference-curriculum-version*
  (curriculum-version 2023 1))

(define (parse-refers-to all-lgs-text)
  (map (lambda (text)
         (cond
           ((regexp-match #rx"^L[ZG][ -]([0-9]+)-([0-9]+) *$" text)
            => (list-destructurer
                (lambda (_all section index)
                  (learning-goal-reference *reference-curriculum-version*
                                           (lg-number (string->number section)
                                                      (string->number index))))))
           ((regexp-match? "Prerequisites|Voraussetzung" text)
            (learning-goal-reference *reference-curriculum-version*
                                     'prerequisite))
           (else
            (error 'parse-lg-numebr "not a valid lg numbers sequence ~a" all-lgs-text))))
       (string-split all-lgs-text #rx", *")))

(define (stem-text en-stem-paragraphs de-stem-paragraphs)
  (localized-text (list (cons "en"
                              (string-join en-stem-paragraphs "\n"))
                        (cons "de"
                              (string-join de-stem-paragraphs "\n")))))

(define (old-question->new en-question de-question)
  (assert-equal? (parse-refers-to (old-question-refers-to en-question))
                 (parse-refers-to (old-question-refers-to de-question)))
  (assert-equal? (old-question-points en-question)
                 (old-question-points de-question))
  (match (cons en-question de-question)
    ((cons (old-p-question id refers-to en-history en-points en-stem-paragraphs en-options)
           (old-p-question _  _         de-history _         de-stem-paragraphs de-options))
     (pick-question id
                    en-points
                    (merge-history en-history de-history)
                    (parse-refers-to refers-to)
                    (stem-text en-stem-paragraphs de-stem-paragraphs)
                    #f
                    (map (lambda (identifier en-option de-option) ; hoping they'll align
                           (assert-equal? (ap-option-kind en-option) (ap-option-kind de-option))
                           (pick-option
                            (ap-option-kind en-option)
                            identifier
                            (localized-text
                             (list (cons "en" (ap-option-text en-option))
                                   (cons "de" (ap-option-text de-option))))))
                         (build-list (length en-options)
                                     (lambda (i)
                                       (string (integer->char (+ (char->integer #\A) i)))))
                         en-options de-options)))
    ((cons (old-k-question id refers-to en-history en-points en-stem-paragraphs en-categories en-options)
           (old-k-question _ _ de-history _ de-stem-paragraphs de-categories de-options))
     (let ((categories
            (map (lambda (en-category de-category) ; hoping they'll align
                   (assert-equal? (old-category-label en-category)
                                  (old-category-label de-category))
                   (category (old-category-label en-category)
                             (localized-text
                              (list (cons "en" (old-category-text en-category))
                                    (cons "de" (old-category-text de-category))))))
                 en-categories de-categories)))

       (define (find-category label)
         (or (findf (lambda (category)
                      (string=? label (category-label category)))
                    categories)
             (error 'find-category "unknown category label ~a" label)))
       (category-question
        id
        en-points
        (merge-history en-history de-history)
        (parse-refers-to refers-to)
        (stem-text en-stem-paragraphs de-stem-paragraphs)
        #f
        categories
        (map (lambda (identifier en-option de-option) ; hoping they'll align
               (assert-equal? (k-option-label en-option) (k-option-label de-option))
               (statement
                (find-category (k-option-label en-option))
                identifier
                (localized-text
                 (list (cons "en" (k-option-text en-option))
                       (cons "de" (k-option-text de-option))))))
             (build-list (length en-options)
                         (lambda (i)
                           (string (integer->char (+ (char->integer #\A) i)))))
             en-options de-options))))
    ((cons (old-a-question id refers-to en-history en-points en-stem-paragraphs en-options)
           (old-a-question _ _          de-history _         de-stem-paragraphs de-options))
     (pick-question id
                    en-points
                    (merge-history en-history de-history)
                    (parse-refers-to refers-to)
                    (stem-text en-stem-paragraphs de-stem-paragraphs)
                    #f
                    (map (lambda (identifier en-option de-option) ; hoping they'll align
                           (assert-equal? (ap-option-kind en-option) (ap-option-kind de-option))
                           (pick-option
                            (ap-option-kind en-option)
                            identifier
                            (localized-text
                             (list (cons "en" (ap-option-text en-option))
                                   (cons "de" (ap-option-text de-option))))))
                         (build-list (length en-options)
                                     (lambda (i)
                                       (string (integer->char (+ (char->integer #\A) i)))))
                         en-options de-options)))))

(define structure-tags
  '(t:question
    t:questions
    t:meta
    t:history
    t:options))

(define cleanup
  (eliminate-whitespace structure-tags values))

(define (translate-old-questions en-filename de-filename exclude-list
                                 target-directory)
  (for-each (lambda (question)
              (call-with-output-file 
                (build-path target-directory
                            (string-append (question-id question) ".xml"))
                (lambda (port)
                  (display-xml (question->document question)
                               port))
                #:exists 'replace))
            (old-questions->new en-filename de-filename exclude-list)))

(define (old-questions->new en-filename de-filename exclude-list)
  (let ((en-questions (parse-old-question-file en-filename))
        (de-questions (parse-old-question-file de-filename)))
    (filter-map (lambda (en-question)
                  (and (not (member (old-question-id en-question) exclude-list))
                       (let ((de-question
                              (or (findf (lambda (de-question)
                                           (string=? (old-question-id en-question)
                                                     (old-question-id de-question)))
                                         de-questions)
                                  (error 'translate-old-questions "couldn't find de version of id ~a"
                                         (old-question-id en-question)))))
                         (with-handlers
                             ((exn:fail? (lambda (exn)
                                           (raise (exn:fail (string-append "translation error in question " (old-question-id en-question) ": " (exn-message exn))
                                                                   (exn-continuation-marks exn))))))
                             
                           (old-question->new en-question de-question)))))
         en-questions)))

'(translate-old-questions "/Users/sperber/data/active-group/schulung/isaqb/foundation/examination-questions/Workspace/questions/official_questions_en.xml" "/Users/sperber/data/active-group/schulung/isaqb/foundation/examination-questions/Workspace/questions/official_questions_de.xml"
                          '("Q-15-01-07" "Q-15-01-20" "Q-15-01-30" "Q-15-01-33" "Q-15-01-39" "Q-15-01-50" "Q-15-01-53" "Q-20-05-01")
                          "/Users/sperber/data/active-group/schulung/isaqb/foundation/examination-questions/Workspace/questions/official")

(define (parse-old-question-file filename)
  (with-handlers
      ((exn:fail? (lambda (exn)
                    (raise (exn:parse-error (string-append "parse error in file " (path->string (build-path filename)) ": " (exn-message exn))
                                            (exn-continuation-marks exn)
                                            exn filename)))))
    (parse-old-question-document (call-with-input-file filename read-xml))))

(define (parse-old-question-document xml)
  (match xml
    ((document _ questions-element _)
     (filter-map (lambda (xml)
                   (match xml
                     ((element* t:build-id () ())
                      #f)
                     ((element* t:question () ())
                      (parse-question xml))))
                 (element-content (cleanup questions-element))))))

(define (parse-question/cleanup element)
  (parse-question (cleanup element)))

(define (parse-question xml)
  (match xml
    ((element* t:question
               ((kind "P"))
               ((t:meta meta)
                ((t:p) (list (element* t:p () stem-paragraphs) ...))
                (t:points (element* t:points () points))
                (t:options options)))
     (let-values (((id refers-to history)
                   (parse-meta meta)))
       (old-p-question id refers-to
                       history
                       (string->number points)
                       stem-paragraphs
                       (parse-ap-options options))))

    ((element* t:question
               ((kind "K"))
               ((t:meta meta)
                ((t:p) (list (element* t:p () stem-paragraphs) ...))
                (t:points (element* t:points () points))
                (t:categories (element* t:categories
                                        ()
                                        (((t:category) categories))))
                (t:options options)))

     (let-values (((id refers-to history)
                   (parse-meta meta)))
       (old-k-question id refers-to
                       history
                       (string->number points)
                       stem-paragraphs
                       (map parse-category categories)
                       (parse-k-options options))))

    ((element* t:question
               ((kind "A"))
               ((t:meta meta)
                ((t:p) (list (element* t:p () stem-paragraphs) ...))
                (t:points (element* t:points () points))
                (t:options options)))
     (let-values (((id refers-to history)
                   (parse-meta meta)))
       (old-a-question id refers-to
                       history
                       (string->number points)
                       stem-paragraphs
                       (parse-ap-options options))))))

                
                
(define (parse-meta xml)
  (match xml
    ((element* t:meta
               ()
               ((t:id (element* t:id () id) )
                (t:refers-to (element* t:refers-to () refers-to)) ; can this be several?
                (t:history history)))
     (values id refers-to (parse-history history)))))
         
(define (parse-category xml)
  (match xml
    ((element* t:category
               ((label label))
               text)
     (old-category label text))))

(define (parse-k-options options)
  (map parse-k-option
       (element-content options)))

(define (parse-k-option xml)
  (match xml
    ((element* t:a () text)
     (k-option "a" text))
    ((element* t:b () text)
     (k-option "b" text))))

(define (parse-ap-options options)
  (map parse-ap-option
       (element-content options)))

(define (parse-ap-option xml)
  (match xml
    ((element* t:distractor () text)
     (ap-option 'distractor text))
    ((element* t:correct () text)
     (ap-option 'correct text))))

(define (parse-history xml)
  (match xml
    ((element* t:history
               ()
               ((ul (element* ul
                              ()
                              (((li) (list (element* li () text) ...)))))))
     text)))
                      
               

(define a-en
  (old-a-question
   "Q-20-05-02"
   "LZ-3-6"
   '("2020-08-22 GS: accepted (by email)"
     "2020-08-19 PGR+ALZ: Adds another distractor. grade: 2"
     "2020-07-20: review by pghadir, see German version of this question"
     "2020-06-23 review by UBR for german version, selectivity might not be optimal, anyway +1"
     "2020-04-25 proposal by ALZ")
   1
   '("Which of the following statements on the documentation of cross-cutting concerns is correct?")
   (list
    (ap-option
     'distractor
     "Cross-cutting concerns affect a large number of building blocks, so they should be documented for each affected building block.")
    (ap-option
     'correct
     "Cross-cutting concerns should be dealt with in one single section of the documentation, as they affect a large number of building blocks.")
    (ap-option 'distractor "Only those cross-cutting concerns that are used in the implementation have to be documented."))))

(define a-de
  (old-a-question
   "Q-20-05-02"
   "LZ-3-6"
   '("2020-08-22 GS: accepted (by email)"
     "2020-08-19 PGR+ALZ: Adds another distractor. note: 2"
     "2020-07-20: review by pghadir: typo fixed, too few choices, imho"
     "2020-06-23 review by UBR for german version, selectivity might not be optimal, anyway +1"
     "2020-04-25 proposal by ALZ")
   1
   '("Welche der folgenden Aussagen zur Dokumentation von Querschnittskonzepten ist zutreffend?")
   (list
    (ap-option
     'distractor
     "Querschnittskonzepte betreffen eine Vielzahl von Bausteinen, daher sollten sie bei jedem betroffenen Baustein dokumentiert werden.")
    (ap-option
     'correct
     "Querschnittskonzepte sollten an einer zentralen Stelle der Dokumentation behandelt werden, da sie eine Vielzahl von Bausteinen betreffen.")
    (ap-option 'distractor "Nur solche Querschnittskonzepte, die in der Implementierung verwendet werden, sollten dokumentiert werden."))))

(define q1
  (element
   (location 7 2 462)
   (location 31 15 1369)
   't:question
   (list (attribute (location 7 14 474) (location 7 22 482) 'kind "P"))
   (list
    (pcdata (location 7 23 483) (location 8 4 488) "\n    ")
    (element
     (location 8 4 488)
     (location 19 13 885)
     't:meta
     '()
     (list
      (pcdata (location 8 12 496) (location 9 6 503) "\n      ")
      (element (location 9 6 503) (location 9 29 526) 't:id '() (list (pcdata (location 9 12 509) (location 9 22 519) "Q-15-01-01")))
      (pcdata (location 9 29 526) (location 10 6 533) "\n      ")
      (element (location 10 6 533) (location 10 39 566) 't:refers-to '() (list (pcdata (location 10 19 546) (location 10 25 552) "LZ-1-1")))
      (pcdata (location 10 39 566) (location 11 6 573) "\n      ")
      (element
       (location 11 6 573)
       (location 18 18 871)
       't:history
       '()
       (list
        (pcdata (location 11 17 584) (location 12 8 593) "\n        ")
        (element
         (location 12 8 593)
         (location 17 13 852)
         'ul
         '()
         (list
          (pcdata (location 12 12 597) (location 13 10 608) "\n          ")
          (element (location 13 10 608) (location 13 66 664) 'li '() (list (pcdata (location 13 14 612) (location 13 61 659) "2020-07-20 Minor fix: consistent capitalisation")))
          (pcdata (location 13 66 664) (location 14 10 675) "\n          ")
          (element
           (location 14 10 675)
           (location 14 77 742)
           'li
           '()
           (list (pcdata (location 14 14 679) (location 14 72 737) "2019-01-24 Fixed. RER. In der Foundation-Gruppe angenommen")))
          (pcdata (location 14 77 742) (location 15 10 753) "\n          ")
          (element (location 15 10 753) (location 15 62 805) 'li '() (list (pcdata (location 15 14 757) (location 15 57 800) "2018-12-01 Fixed. RER. Minor English change")))
          (pcdata (location 15 62 805) (location 16 10 816) "\n          ")
          (element (location 16 10 816) (location 16 32 838) 'li '() (list (pcdata (location 16 14 820) (location 16 27 833) "Old-ID:AK0001")))
          (pcdata (location 16 32 838) (location 17 8 847) "\n        ")))
        (pcdata (location 17 13 852) (location 18 6 859) "\n      ")))
      (pcdata (location 18 18 871) (location 19 4 876) "\n    ")))
    (pcdata (location 19 13 885) (location 20 4 890) "\n    ")
    (element
     (location 20 4 890)
     (location 20 95 981)
     't:p
     '()
     (list (pcdata (location 20 9 895) (location 20 89 975) "What are the four key terms used in common definitions of software architecture?")))
    (pcdata (location 20 95 981) (location 21 4 986) "\n    ")
    (element (location 21 4 986) (location 21 26 1008) 't:points '() (list (pcdata (location 21 14 996) (location 21 15 997) "2")))
    (pcdata (location 21 26 1008) (location 22 4 1013) "\n    ")
    (element
     (location 22 4 1013)
     (location 30 16 1353)
     't:options
     '()
     (list
      (pcdata (location 22 15 1024) (location 23 6 1031) "\n      ")
      (element (location 23 6 1031) (location 23 46 1071) 't:distractor '() (list (pcdata (location 23 20 1045) (location 23 31 1056) "Source code")))
      (pcdata (location 23 46 1071) (location 24 6 1078) "\n      ")
      (element (location 24 6 1078) (location 24 44 1116) 't:correct '() (list (pcdata (location 24 17 1089) (location 24 32 1104) "Building blocks")))
      (pcdata (location 24 44 1116) (location 25 6 1123) "\n      ")
      (element (location 25 6 1123) (location 25 48 1165) 't:distractor '() (list (pcdata (location 25 20 1137) (location 25 33 1150) "Functionality")))
      (pcdata (location 25 48 1165) (location 26 6 1172) "\n      ")
      (element (location 26 6 1172) (location 26 42 1208) 't:correct '() (list (pcdata (location 26 17 1183) (location 26 30 1196) "Relationships")))
      (pcdata (location 26 42 1208) (location 27 6 1215) "\n      ")
      (element (location 27 6 1215) (location 27 39 1248) 't:correct '() (list (pcdata (location 27 17 1226) (location 27 27 1236) "Components")))
      (pcdata (location 27 39 1248) (location 28 6 1255) "\n      ")
      (element (location 28 6 1255) (location 28 47 1296) 't:distractor '() (list (pcdata (location 28 20 1269) (location 28 32 1281) "Requirements")))
      (pcdata (location 28 47 1296) (location 29 6 1303) "\n      ")
      (element (location 29 6 1303) (location 29 39 1336) 't:correct '() (list (pcdata (location 29 17 1314) (location 29 27 1324) "Interfaces")))
      (pcdata (location 29 39 1336) (location 30 4 1341) "\n    ")))
    (pcdata (location 30 16 1353) (location 31 2 1356) "\n  "))))

(define q2
  (element
   (location 92 2 4733)
   (location 121 15 5992)
   't:question
   (list (attribute (location 92 14 4745) (location 92 22 4753) 'kind "K"))
   (list
    (pcdata (location 92 23 4754) (location 93 4 4759) "\n    ")
    (element
     (location 93 4 4759)
     (location 106 13 5299)
     't:meta
     '()
     (list
      (pcdata (location 93 12 4767) (location 94 6 4774) "\n      ")
      (element (location 94 6 4774) (location 94 29 4797) 't:id '() (list (pcdata (location 94 12 4780) (location 94 22 4790) "Q-15-01-04")))
      (pcdata (location 94 29 4797) (location 95 6 4804) "\n      ")
      (element (location 95 6 4804) (location 95 39 4837) 't:refers-to '() (list (pcdata (location 95 19 4817) (location 95 25 4823) "LZ-2-1")))
      (pcdata (location 95 39 4837) (location 96 6 4844) "\n      ")
      (element
       (location 96 6 4844)
       (location 105 18 5285)
       't:history
       '()
       (list
        (pcdata (location 96 17 4855) (location 97 8 4864) "\n        ")
        (element
         (location 97 8 4864)
         (location 104 13 5266)
         'ul
         '()
         (list
          (pcdata (location 97 12 4868) (location 98 10 4879) "\n          ")
          (element
           (location 98 10 4879)
           (location 98 74 4943)
           'li
           '()
           (list (pcdata (location 98 14 4883) (location 98 69 4938) "21-02-08, CL,RR,AL,PH+GS: removed last answer, Jira #49")))
          (pcdata (location 98 74 4943) (location 100 10 4955) "\n\n          ")
          (element
           (location 100 10 4955)
           (location 100 99 5044)
           'li
           '()
           (list (pcdata (location 100 14 4959) (location 100 94 5039) "2020-08-03 Fixed. GS+AL, consistent wording \"which are true and which are false\"")))
          (pcdata (location 100 99 5044) (location 101 10 5055) "\n          ")
          (element
           (location 101 10 5055)
           (location 101 79 5124)
           'li
           '()
           (list (pcdata (location 101 14 5059) (location 101 74 5119) "2019-01-24 Fixed. RER. Von der Foundation-Gruppe angenommen.")))
          (pcdata (location 101 79 5124) (location 102 10 5135) "\n          ")
          (element
           (location 102 10 5135)
           (location 102 94 5219)
           'li
           '()
           (list (pcdata (location 102 14 5139) (location 102 89 5214) "2018-12-01 Fixed. RER. Reworded possible ambiguity and simplified language.")))
          (pcdata (location 102 94 5219) (location 103 10 5230) "\n          ")
          (element (location 103 10 5230) (location 103 32 5252) 'li '() (list (pcdata (location 103 14 5234) (location 103 27 5247) "Old-ID:AK0004")))
          (pcdata (location 103 32 5252) (location 104 8 5261) "\n        ")))
        (pcdata (location 104 13 5266) (location 105 6 5273) "\n      ")))
      (pcdata (location 105 18 5285) (location 106 4 5290) "\n    ")))
    (pcdata (location 106 13 5299) (location 107 4 5304) "\n    ")
    (element
     (location 107 4 5304)
     (location 107 115 5415)
     't:p
     '()
     (list (pcdata (location 107 9 5309) (location 107 109 5409) "Which statements regarding top-down and bottom-up design are true and which are and which are false?")))
    (pcdata (location 107 115 5415) (location 108 4 5420) "\n    ")
    (element (location 108 4 5420) (location 108 26 5442) 't:points '() (list (pcdata (location 108 14 5430) (location 108 15 5431) "2")))
    (pcdata (location 108 26 5442) (location 109 4 5447) "\n    ")
    (element
     (location 109 4 5447)
     (location 112 19 5574)
     't:categories
     '()
     (list
      (pcdata (location 109 18 5461) (location 110 6 5468) "\n      ")
      (element
       (location 110 6 5468)
       (location 110 45 5507)
       't:category
       (list (attribute (location 110 18 5480) (location 110 27 5489) 'label "a"))
       (list (pcdata (location 110 28 5490) (location 110 32 5494) "true")))
      (pcdata (location 110 45 5507) (location 111 6 5514) "\n      ")
      (element
       (location 111 6 5514)
       (location 111 46 5554)
       't:category
       (list (attribute (location 111 18 5526) (location 111 27 5535) 'label "b"))
       (list (pcdata (location 111 28 5536) (location 111 33 5541) "false")))
      (pcdata (location 111 46 5554) (location 112 4 5559) "\n    ")))
    (pcdata (location 112 19 5574) (location 113 4 5579) "\n    ")
    (element
     (location 113 4 5579)
     (location 120 16 5976)
     't:options
     '()
     (list
      (pcdata (location 113 15 5590) (location 114 6 5597) "\n      ")
      (element
       (location 114 6 5597)
       (location 114 74 5665)
       't:a
       '()
       (list (pcdata (location 114 11 5602) (location 114 68 5659) "Top-down and bottom-up design may be used simultaneously.")))
      (pcdata (location 114 74 5665) (location 115 6 5672) "\n      ")
      (element
       (location 115 6 5672)
       (location 115 69 5735)
       't:a
       '()
       (list (pcdata (location 115 11 5677) (location 115 63 5729) "Top-down requires that details be ignored initially.")))
      (pcdata (location 115 69 5735) (location 116 6 5742) "\n      ")
      (element
       (location 116 6 5742)
       (location 116 69 5805)
       't:b
       '()
       (list (pcdata (location 116 11 5747) (location 116 63 5799) "Architects leave the bottom-up design to developers.")))
      (pcdata (location 116 69 5805) (location 117 6 5812) "\n      ")
      (element (location 117 6 5812) (location 117 60 5866) 't:b '() (list (pcdata (location 117 11 5817) (location 117 54 5860) "Generally, architects should work top-down.")))
      (pcdata (location 117 60 5866) (location 118 6 5873) "\n      ")
      (element
       (location 118 6 5873)
       (location 118 85 5952)
       't:b
       '()
       (list (pcdata (location 118 11 5878) (location 118 79 5946) "Bottom-up design means to proceed from the abstract to the concrete.")))
      (pcdata (location 118 85 5952) (location 120 4 5964) "\n      \n    ")))
    (pcdata (location 120 16 5976) (location 121 2 5979) "\n  "))))

(define q3
  (element
   (location 1490 2 76710)
   (location 1512 15 77954)
   't:question
   (list (attribute (location 1490 14 76722) (location 1490 22 76730) 'kind "A"))
   (list
    (pcdata (location 1490 23 76731) (location 1491 4 76736) "\n    ")
    (element
     (location 1491 4 76736)
     (location 1504 13 77312)
     't:meta
     '()
     (list
      (pcdata (location 1491 12 76744) (location 1492 6 76751) "\n      ")
      (element (location 1492 6 76751) (location 1492 29 76774) 't:id '() (list (pcdata (location 1492 12 76757) (location 1492 22 76767) "Q-20-05-02")))
      (pcdata (location 1492 29 76774) (location 1493 6 76781) "\n      ")
      (pcdata (location 1493 52 76827) (location 1494 6 76834) "\n      ")
      (element (location 1494 6 76834) (location 1494 39 76867) 't:refers-to '() (list (pcdata (location 1494 19 76847) (location 1494 25 76853) "LZ-3-6")))
      (pcdata (location 1494 39 76867) (location 1495 6 76874) "\n      ")
      (element
       (location 1495 6 76874)
       (location 1503 18 77298)
       't:history
       '()
       (list
        (pcdata (location 1495 17 76885) (location 1496 8 76894) "\n        ")
        (element
         (location 1496 8 76894)
         (location 1502 13 77279)
         'ul
         '()
         (list
          (pcdata (location 1496 12 76898) (location 1497 10 76909) "\n          ")
          (element (location 1497 10 76909) (location 1497 53 76952) 'li '() (list (pcdata (location 1497 14 76913) (location 1497 48 76947) "2020-08-22 GS: accepted (by email)")))
          (pcdata (location 1497 53 76952) (location 1498 10 76963) "\n          ")
          (element
           (location 1498 10 76963)
           (location 1498 72 77025)
           'li
           '()
           (list (pcdata (location 1498 14 76967) (location 1498 67 77020) "2020-08-19 PGR+ALZ: Adds another distractor. grade: 2")))
          (pcdata (location 1498 72 77025) (location 1499 10 77036) "\n          ")
          (element
           (location 1499 10 77036)
           (location 1499 85 77111)
           'li
           '()
           (list (pcdata (location 1499 14 77040) (location 1499 80 77106) "2020-07-20: review by pghadir, see German version of this question")))
          (pcdata (location 1499 85 77111) (location 1500 10 77122) "\n          ")
          (element
           (location 1500 10 77122)
           (location 1500 107 77219)
           'li
           '()
           (list (pcdata (location 1500 14 77126) (location 1500 102 77214) "2020-06-23 review by UBR for german version, selectivity might not be optimal, anyway +1")))
          (pcdata (location 1500 107 77219) (location 1501 10 77230) "\n          ")
          (element (location 1501 10 77230) (location 1501 45 77265) 'li '() (list (pcdata (location 1501 14 77234) (location 1501 40 77260) "2020-04-25 proposal by ALZ")))
          (pcdata (location 1501 45 77265) (location 1502 8 77274) "\n        ")))
        (pcdata (location 1502 13 77279) (location 1503 6 77286) "\n      ")))
      (pcdata (location 1503 18 77298) (location 1504 4 77303) "\n    ")))
    (pcdata (location 1504 13 77312) (location 1505 4 77317) "\n    ")
    (element
     (location 1505 4 77317)
     (location 1505 107 77420)
     't:p
     '()
     (list (pcdata (location 1505 9 77322) (location 1505 101 77414) "Which of the following statements on the documentation of cross-cutting concerns is correct?")))
    (pcdata (location 1505 107 77420) (location 1506 4 77425) "\n    ")
    (element (location 1506 4 77425) (location 1506 26 77447) 't:points '() (list (pcdata (location 1506 14 77435) (location 1506 15 77436) "1")))
    (pcdata (location 1506 26 77447) (location 1507 4 77452) "\n    ")
    (element
     (location 1507 4 77452)
     (location 1511 16 77938)
     't:options
     '()
     (list
      (pcdata (location 1507 15 77463) (location 1508 6 77470) "\n      ")
      (element
       (location 1508 6 77470)
       (location 1508 162 77626)
       't:distractor
       '()
       (list
        (pcdata
         (location 1508 20 77484)
         (location 1508 147 77611)
         "Cross-cutting concerns affect a large number of building blocks, so they should be documented for each affected building block.")))
      (pcdata (location 1508 162 77626) (location 1509 6 77633) "\n      ")
      (element
       (location 1509 6 77633)
       (location 1509 166 77793)
       't:correct
       '()
       (list
        (pcdata
         (location 1509 17 77644)
         (location 1509 154 77781)
         "Cross-cutting concerns should be dealt with in one single section of the documentation, as they affect a large number of building blocks.")))
      (pcdata (location 1509 166 77793) (location 1510 6 77800) "\n      ")
      (element
       (location 1510 6 77800)
       (location 1510 127 77921)
       't:distractor
       '()
       (list (pcdata (location 1510 20 77814) (location 1510 112 77906) "Only those cross-cutting concerns that are used in the implementation have to be documented.")))
      (pcdata (location 1510 127 77921) (location 1511 4 77926) "\n    ")))
    (pcdata (location 1511 16 77938) (location 1512 2 77941) "\n  "))))

(module+ test
  (require rackunit)

  (check-equal?
   'foo
   (element
    (location 7 2 462)
    (location 31 15 1369)
    't:question
    (list (attribute (location 7 14 474) (location 7 22 482) 'kind "P"))
    (list
     (pcdata (location 7 23 483) (location 8 4 488) "\n    ")
     (element
      (location 8 4 488)
      (location 19 13 885)
      't:meta
      '()
      (list
       (pcdata (location 8 12 496) (location 9 6 503) "\n      ")
       (element (location 9 6 503) (location 9 29 526) 't:id '() (list (pcdata (location 9 12 509) (location 9 22 519) "Q-15-01-01")))
       (pcdata (location 9 29 526) (location 10 6 533) "\n      ")
       (element (location 10 6 533) (location 10 39 566) 't:refers-to '() (list (pcdata (location 10 19 546) (location 10 25 552) "LZ-1-1")))
       (pcdata (location 10 39 566) (location 11 6 573) "\n      ")
       (element
        (location 11 6 573)
        (location 18 18 871)
        't:history
        '()
        (list
         (pcdata (location 11 17 584) (location 12 8 593) "\n        ")
         (element
          (location 12 8 593)
          (location 17 13 852)
          'ul
          '()
          (list
           (pcdata (location 12 12 597) (location 13 10 608) "\n          ")
           (element (location 13 10 608) (location 13 66 664) 'li '() (list (pcdata (location 13 14 612) (location 13 61 659) "2020-07-20 Minor fix: consistent capitalisation")))
           (pcdata (location 13 66 664) (location 14 10 675) "\n          ")
           (element
            (location 14 10 675)
            (location 14 77 742)
            'li
            '()
            (list (pcdata (location 14 14 679) (location 14 72 737) "2019-01-24 Fixed. RER. In der Foundation-Gruppe angenommen")))
           (pcdata (location 14 77 742) (location 15 10 753) "\n          ")
           (element (location 15 10 753) (location 15 62 805) 'li '() (list (pcdata (location 15 14 757) (location 15 57 800) "2018-12-01 Fixed. RER. Minor English change")))
           (pcdata (location 15 62 805) (location 16 10 816) "\n          ")
           (element (location 16 10 816) (location 16 32 838) 'li '() (list (pcdata (location 16 14 820) (location 16 27 833) "Old-ID:AK0001")))
           (pcdata (location 16 32 838) (location 17 8 847) "\n        ")))
         (pcdata (location 17 13 852) (location 18 6 859) "\n      ")))
       (pcdata (location 18 18 871) (location 19 4 876) "\n    ")))
     (pcdata (location 19 13 885) (location 20 4 890) "\n    ")
     (element
      (location 20 4 890)
      (location 20 95 981)
      't:p
      '()
      (list (pcdata (location 20 9 895) (location 20 89 975) "What are the four key terms used in common definitions of software architecture?")))
     (pcdata (location 20 95 981) (location 21 4 986) "\n    ")
     (element (location 21 4 986) (location 21 26 1008) 't:points '() (list (pcdata (location 21 14 996) (location 21 15 997) "2")))
     (pcdata (location 21 26 1008) (location 22 4 1013) "\n    ")
     (element
      (location 22 4 1013)
      (location 30 16 1353)
      't:options
      '()
      (list
       (pcdata (location 22 15 1024) (location 23 6 1031) "\n      ")
       (element (location 23 6 1031) (location 23 46 1071) 't:distractor '() (list (pcdata (location 23 20 1045) (location 23 31 1056) "Source code")))
       (pcdata (location 23 46 1071) (location 24 6 1078) "\n      ")
       (element (location 24 6 1078) (location 24 44 1116) 't:correct '() (list (pcdata (location 24 17 1089) (location 24 32 1104) "Building blocks")))
       (pcdata (location 24 44 1116) (location 25 6 1123) "\n      ")
       (element (location 25 6 1123) (location 25 48 1165) 't:distractor '() (list (pcdata (location 25 20 1137) (location 25 33 1150) "Functionality")))
       (pcdata (location 25 48 1165) (location 26 6 1172) "\n      ")
       (element (location 26 6 1172) (location 26 42 1208) 't:correct '() (list (pcdata (location 26 17 1183) (location 26 30 1196) "Relationships")))
       (pcdata (location 26 42 1208) (location 27 6 1215) "\n      ")
       (element (location 27 6 1215) (location 27 39 1248) 't:correct '() (list (pcdata (location 27 17 1226) (location 27 27 1236) "Components")))
       (pcdata (location 27 39 1248) (location 28 6 1255) "\n      ")
       (element (location 28 6 1255) (location 28 47 1296) 't:distractor '() (list (pcdata (location 28 20 1269) (location 28 32 1281) "Requirements")))
       (pcdata (location 28 47 1296) (location 29 6 1303) "\n      ")
       (element (location 29 6 1303) (location 29 39 1336) 't:correct '() (list (pcdata (location 29 17 1314) (location 29 27 1324) "Interfaces")))
       (pcdata (location 29 39 1336) (location 30 4 1341) "\n    ")))
     (pcdata (location 30 16 1353) (location 31 2 1356) "\n  ")))))
   
  
