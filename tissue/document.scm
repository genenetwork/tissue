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
  #:use-module (rnrs hashtables)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-171)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:use-module (term ansi-color)
  #:use-module (xapian xapian)
  #:use-module (tissue utils)
  #:export (object->scm
            scm->object
            <document>
            document-title
            document-type
            document-id-term
            document-text
            document-term-generator
            print
            <file-document>
            file-document-path
            read-gemtext-document))

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
          (map (lambda (slot)
                 (let* ((slot-name (slot-definition-name slot))
                        (value (if (slot-bound? object slot-name)
                                   (slot-ref object slot-name)
                                   (goops-error "Unbound slot ~s in ~s" slot-name object))))
                   (cons slot-name (object->scm value))))
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
  (title #:accessor document-title #:init-keyword #:title))

(define-method (document-type (document <document>))
  "document")

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
    (index-text! term-generator (document-title document) #:prefix "S")
    (index-text! term-generator (document-text document))
    term-generator))

(define-class <file-document> (<document>)
  (path #:accessor file-document-path #:init-keyword #:path))

(define-method (document-id-term (document <file-document>))
  "Return the ID term for DOCUMENT."
  (string-append "Q" (file-document-path document)))

(define-method (document-text (document <file-document>))
  "Return the full text of DOCUMENT."
  (call-with-input-file (file-document-path document)
    get-string-all))

(define-method (document-term-generator (document <file-document>))
  "Return a term generator indexing DOCUMENT."
  (let ((term-generator (next-method)))
    (increase-termpos! term-generator)
    (index-text! term-generator (file-document-path document))
    term-generator))

(define-method (print (document <file-document>) mset)
  "Print DOCUMENT in command-line search results. MSET is the xapian
MSet object representing a list of search results."
  (display (colorize-string (document-title document) 'MAGENTA 'UNDERLINE))
  (newline)
  (display (colorize-string (file-document-path document) 'YELLOW))
  (newline)
  (newline)
  (let ((snippet (mset-snippet mset
                               (document-text document)
                               #:length 200
                               #:highlight-start (color 'BOLD 'ON-RED)
                               #:highlight-end (color 'RESET)
                               #:stemmer (make-stem "en"))))
    (unless (string-null? snippet)
      (display snippet)
      (newline)
      (newline))))

(define (read-gemtext-document file)
  "Reade gemtext document from FILE. Return a <file-document> object."
  (make <file-document>
    #:title (or (call-with-input-file file
                  (lambda (port)
                    (port-transduce (tfilter-map (lambda (line)
                                                   ;; The first level one
                                                   ;; heading is the title.
                                                   (and (string-prefix? "# " line)
                                                        (string-remove-prefix "# " line))))
                                    (rany identity)
                                    get-line-dos-or-unix
                                    port)))
                ;; Fallback to filename if document has no title.
                file)
    #:path file))
