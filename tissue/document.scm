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
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-171)
  #:use-module (term ansi-color)
  #:use-module (xapian xapian)
  #:use-module (tissue utils)
  #:export (document
            document?
            document-file
            document-title
            document->alist
            alist->document
            print-document
            read-gemtext-document
            index-document))

(define-record-type <document>
  (document file title)
  document?
  (file document-file)
  (title document-title))

(define (document->alist document)
  "Convert DOCUMENT, a <document> object, to an association list that
can be serialized."
  `((type . document)
    (file . ,(document-file document))
    (title . ,(document-title document))))

(define (alist->document alist)
  "Convert ALIST to a <document> object."
  (document (assq-ref alist 'file)
            (assq-ref alist 'title)))

(define (print-document document)
  "Print DOCUMENT, a <document> object, in search results."
  (display (colorize-string (document-title document) 'MAGENTA 'UNDERLINE))
  (newline)
  (display (colorize-string (document-file document) 'YELLOW))
  (newline)
  (newline))

(define (read-gemtext-document file)
  "Reade gemtext document from FILE. Return a <document> object."
  (document file
            (or (call-with-input-file file
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
                file)))

(define (index-document db document)
  "Index DOCUMENT, a <document> object, in writable xapian DB."
  (let* ((idterm (string-append "Q" (document-file document)))
         (body (call-with-input-file (document-file document)
                 get-string-all))
         (doc (make-document #:data (call-with-output-string
                                      (cut write (document->alist document) <>))
                             #:terms `((,idterm . 0))))
         (term-generator (make-term-generator #:stem (make-stem "en")
                                              #:document doc)))
    (index-text! term-generator "document" #:prefix "XT")
    (index-text! term-generator body)
    (replace-document! db idterm doc)))
