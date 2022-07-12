;;; tissue --- Text based issue tracker
;;; Copyright © 2022 Arun Isaac <arunisaac@systemreboot.net>
;;;
;;; This file is part of tissue.
;;;
;;; tissue is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; tissue is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with tissue.  If not, see <https://www.gnu.org/licenses/>.

(define-module (tissue document)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (htmlprag)
  #:use-module (oop goops)
  #:use-module (term ansi-color)
  #:use-module (xapian xapian)
  #:use-module (tissue git)
  #:use-module (tissue utils)
  #:export (slot-set
            object->scm
            scm->object
            <document>
            document-title
            document-web-uri
            document-type
            document-id-term
            document-text
            document-term-generator
            document-snippet-source-text
            document-snippet
            print
            document-sxml-snippet
            document->sxml))

(define (slot-set object slot-name value)
  "Set SLOT-NAME in OBJECT to VALUE. This is a purely functional setter
that operates on a copy of OBJECT. It does not mutate OBJECT."
  (let ((clone (shallow-clone object)))
    (slot-set! clone slot-name value)
    clone))

;; We override the default write to print object slots and values.
(define-method (write (object <object>) port)
  "Write OBJECT to PORT."
  (format port "#<~a ~a>"
          (class-name (class-of object))
          (string-join (filter-map (lambda (slot)
                                     (let ((slot-name (slot-definition-name slot)))
                                       (and (slot-bound? object slot-name)
                                            (call-with-output-string
                                              (cut format <> "~s: ~s"
                                                   slot-name
                                                   (slot-ref object slot-name))))))
                                   (class-slots (class-of object))))))

(define (date->iso-8601 date)
  "Convert DATE, an SRFI-19 date object, to an ISO-8601 date string."
  (date->string date "~4"))

(define (iso-8601->date str)
  "Convert STR, an ISO-8601 date string, to an SRFI-19 date object."
  (string->date str "~Y-~m-~dT~H:~M:~S~z"))

(define (date->alist date)
  "Convert DATE, an SRFI-19 date object, to an association list."
  `((type . <date>)
    (iso-8601 . ,(date->iso-8601 date))))

(define (alist->date alist)
  "Convert an association list to an SRFI-19 date object."
  (iso-8601->date (assq-ref alist 'iso-8601)))

(define (object->scm object)
  "Convert GOOPS OBJECT to a serializable object."
  (cond
   ((or (string? object)
        (number? object)
        (boolean? object))
    object)
   ((date? object)
    (date->alist object))
   ((list? object)
    (list->vector (map object->scm object)))
   (else
    (cons (cons 'type (class-name (class-of object)))
          (filter-map (lambda (slot)
                        (if (slot-bound? object (slot-definition-name slot))
                            (cons (slot-definition-name slot)
                                  (object->scm (slot-ref object (slot-definition-name slot))))
                            #f))
                      (class-slots (class-of object)))))))

(define (scm->object scm)
  "Convert serializable object SCM to a GOOPS object."
  (cond
   ((or (string? scm)
        (number? scm)
        (boolean? scm))
    scm)
   ((vector? scm)
    (map scm->object (vector->list scm)))
   ;; Association list encoding date
   ((eq? (assq-ref scm 'type)
         '<date>)
    (alist->date scm))
   ;; Association list encoding arbitrary object
   (else
    (let* ((class (module-ref (current-module)
                              (assq-ref scm 'type)))
           (object (make class)))
      (for-each (match-lambda
                  ((slot-name . value)
                   (unless (eq? slot-name 'type)
                     (slot-set! object slot-name (scm->object value)))))
                scm)
      object))))

(define-class <document> ()
  (title #:accessor document-title #:init-keyword #:title)
  (web-uri #:accessor document-web-uri #:init-keyword #:web-uri))

(define-generic document-id-term)
(define-generic document-text)
(define-generic print)
(define-generic document->sxml)

(define-method (document-type (document <document>))
  (string-trim-both (symbol->string (class-name (class-of document)))
                    (char-set #\< #\>)))

(define-method (document-term-generator (document <document>))
  "Return a term generator for DOCUMENT. The returned term generator has
indexed the type and text of the document. If further free text is to
be indexed, to prevent phrase searches from spanning between this text
and further text, increase-termpos! must be called before indexing."
  (let ((term-generator
         (make-term-generator
          #:stem (make-stem "en")
          #:document (make-document
                      #:data (call-with-output-string
                               (cut write (object->scm document) <>))
                      #:terms `((,(document-id-term document) . 0))))))
    (index-text! term-generator (document-type document) #:prefix "XT")
    (index-text! term-generator (document-title document) #:prefix "S")
    (index-text! term-generator (document-text document))
    term-generator))

(define-method (document-snippet-source-text (document <document>))
  "Return the source text for DOCUMENT from which to extract a search
result snippet."
  ;; Remove blank lines from document text.
  (string-join
   (remove (cut string-every char-set:whitespace <>)
           (string-split (document-text document)
                         #\newline))
   "\n"))

(define (document-html-snippet document mset)
  "Return snippet for DOCUMENT. MSET is the xapian MSet object
representing a list of search results."
  (mset-snippet mset
                (document-snippet-source-text document)
                #:length 200
                #:highlight-start "<b>"
                #:highlight-end "</b>"
                #:stemmer (make-stem "en")))

(define (document-snippet document mset)
  "Return snippet for DOCUMENT. MSET is the xapian MSet object
representing a list of search results."
  ;; mset-snippet, and thus document-html-snippet, returns serialized
  ;; HTML. So, we reverse it with html->sxml.
  (match (html->sxml (document-html-snippet document mset))
    (('*TOP* children ...)
     (string-join
      (map (match-lambda
             ;; Colorize string instead of HTML bold.
             (('b str) (colorize-string str 'BOLD 'ON-RED))
             ;; Else, return verbatim.
             (str str))
           children)
      ""))))

(define (document-sxml-snippet document mset)
  "Return snippet in SXML form for DOCUMENT. MSET is the xapian MSet
object representing a list of search results."
  ;; mset-snippet, and thus document-html-snippet, returns serialized
  ;; HTML. So, we reverse it with html->sxml.
  (match (html->sxml (document-html-snippet document mset))
    (('*TOP* children ...)
     (append-map (lambda (child)
                   (cond
                    ;; Add (br) if end of line.
                    ((and (string? child)
                          (string-suffix? "\n" child))
                     (list (string-trim-right child #\newline)
                           '(br)))
                    ;; Else, return verbatim.
                    (else
                     (list child))))
                 children))))
