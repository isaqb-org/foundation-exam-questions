#lang racket/base

; Generate XLIFF templates for translations

(require racket/match
         xml
         (only-in racket/list partition range)
         (only-in racket/string string-trim)
         file/glob
         racket/path)
(require "match-xml.rkt")

(struct translations
  (localizeds)
  #:transparent)

(struct localized
  (text language outdated?)
  #:transparent)

(define (text-element? content)
  (match content
    ((element _start _stop 'text _attributes _contents) #t)
    (else #f)))

(define (collect-translations content)
  (match content
    ((element _start _stop name attributes contents)
     (let-values (((texts non-texts)
                   (partition text-element? contents)))
       (if (null? texts)
           (apply append (map collect-translations contents))
           (cons (translations (map text->localized texts))
                 (apply append (map collect-translations contents))))))
    (else '())))
              
; returns lang or #f
(define (text->localized xml)
  (match xml
    ((element* text
               ((xml:lang lang)
                (outdated outdated-value))
               text)
     (localized (string-trim text)
                lang
                (equal? outdated-value "outdated")))))


(define structure-tags
  '(pickQuestion
    categoryQuestion
    history lg stem pickOptions option explanation
    categoryStatements categories category statements statement))

(define cleanup
  (eliminate-whitespace structure-tags values)) 

(define (question-id element)
  (match element
    ((element* pickQuestion ((id id)) ()) id)
    ((element* categoryQuestion ((id id)) ()) id)))

; returns id, translations
(define (document-collect-translations xml)
  (match xml
    ((document _ element _)
     (let ((question (cleanup element)))
       (values (question-id question)
               (collect-translations question))))))

(define (file-collect-translations filename)
  (let ((document (call-with-input-file filename read-xml)))
    (document-collect-translations document)))

(define (element% name attributes contents)
  (element #f #f name attributes contents))

(define (attribute% name value)
  (attribute #f #f name value))

(define (pcdata% text)
  (pcdata #f #f text))

(define (partition-translation source-language target-language translation)
  (let ((localizeds (translations-localizeds translation)))
    (values (findf (lambda (localized) (equal? (localized-language localized) source-language)) localizeds)
            (findf (lambda (localized) (equal? (localized-language localized) target-language)) localizeds)
            (filter (lambda (localized)
                      (let ((lang (localized-language localized)))
                        (and (not (equal? lang source-language))
                             (not (equal? lang target-language)))))
                    localizeds))))

(define (text-body localized)
  (list (pcdata% (localized-text localized))))

(struct missing-translation
  (source-localized
   target-language
   additional-localizeds)
  #:transparent)

(define (translations-missing source-language target-language translations)
  (filter
   values
   (map (lambda (translation)
          (let-values (((source-localized target-localized rest-localizeds)
                        (partition-translation source-language target-language translation)))
            (if (and target-localized
                     (not (equal? "" (localized-text target-localized)))
                     (not (localized-outdated? target-localized)))
                #f ; nothing to do
                (missing-translation source-localized
                                     target-language
                                     rest-localizeds))))
        translations)))

(define (translations->xliff-file source-language target-language filename id translations)
  (let* ((missing-translations
          (translations-missing source-language target-language translations))
         (trans-units
          (map (lambda (index missing)
                 (element% 'unit
                           (list (attribute% 'id (string-append id "_" (number->string index))))
                           (list
                            (element% 'notes '()
                                      (map (lambda (localized)
                                             (element% 'note
                                                       (list
                                                        (attribute% 'appliesTo "target")
                                                        (attribute% 'category "translation")
                                                        (attribute% 'xml:lang (localized-language localized)))
                                                       (text-body localized)))
                                           (missing-translation-additional-localizeds missing)))
                            (element% 'segment '()
                                      (list
                                       (element% 'source '() (text-body (missing-translation-source-localized missing)))
                                       (element% 'target '()
                                                 (list (comment "translation goes here"))))))))
               (range (length missing-translations))
               missing-translations)))
    (and (pair? trans-units)
         (element% 'file
                   (list (attribute% 'id filename))
                   trans-units))))

(define (file->xliff source-language target-language filename)
  (let-values (((id translations) (file-collect-translations filename))
               ((base name must-be-dir?) (split-path filename)))
    (translations->xliff-file source-language target-language (path->string name) id translations)))

(define (files->xliff-document source-language target-language filenames)
  (document (prolog (list (p-i #f #f 'xml "version=\"1.0\" encoding=\"UTF-8\""))
                    #f
                    '())
            (element% 'xliff
                      (list
                       (attribute% 'version "2.2")
                       (attribute% 'xmlns "urn:oasis:names:tc:xliff:document:2.2")
                       (attribute% 'srcLang source-language)
                       (attribute% 'trgLang target-language))
                      (filter values
                              (map (lambda (filename)
                                     (file->xliff source-language target-language filename))
                                   filenames)))
            '()))

(define (generate-xliff source-language target-languages filenames output-path-prefix)
  (for-each (lambda (target-language)
              (let ((document (files->xliff-document source-language target-language filenames)))
                (call-with-output-file
                  (let-values (((name base must-be-dir?) (split-path output-path-prefix)))
                    (build-path name (string-append (path->string base) "-" target-language ".xml")))
                  (lambda (port)
                    (display-xml document port))
                  #:exists 'replace)))
            target-languages))
