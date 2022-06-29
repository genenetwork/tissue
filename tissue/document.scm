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
  #:use-module (htmlprag)
  #:use-module (oop goops)
  #:use-module (term ansi-color)
  #:use-module (xapian xapian)
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
            document-snippet
            print
            document-sxml-snippet
            document->sxml
            <file-document>
            file-document-path
            read-gemtext-document))

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
  (title #:accessor document-title #:init-keyword #:title)
  (web-uri #:accessor document-web-uri #:init-keyword #:web-uri))

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

(define (document-html-snippet document mset)
  "Return snippet for DOCUMENT. MSET is the xapian MSet object
representing a list of search results."
  (mset-snippet mset
                (string-join
                 (remove (cut string-every char-set:whitespace <>)
                         (string-split (document-text document)
                                       #\newline))
                 "\n")
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

(define-method (print (document <file-document>) mset)
  "Print DOCUMENT in command-line search results. MSET is the xapian
MSet object representing a list of search results."
  (display (colorize-string (document-title document) 'MAGENTA 'UNDERLINE))
  (newline)
  (display (colorize-string (file-document-path document) 'YELLOW))
  (newline)
  (newline)
  (let ((snippet (document-snippet document mset)))
    (unless (string-null? snippet)
      (display snippet)
      (newline)
      (newline))))

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

(define-method (document->sxml (document <file-document>) mset)
  "Render DOCUMENT to SXML. MSET is the xapian MSet object representing
a list of search results."
  `(li (@ (class "search-result"))
       (a (@ (href ,(document-web-uri document)))
          ,(document-title document))
       ,@(let ((snippet (document-sxml-snippet document mset)))
           (if snippet
               (list `(div (@ (class "search-result-snippet"))
                           ,@snippet))
               (list)))))

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
