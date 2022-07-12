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

(define-module (tissue file-document)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-171)
  #:use-module (oop goops)
  #:use-module (term ansi-color)
  #:use-module (tissue document)
  #:use-module (tissue git)
  #:use-module (tissue utils)
  #:use-module (xapian xapian)
  #:export (<file-document>
            file-document-path
            read-gemtext-document))

(define-class <file-document> (<document>)
  (path #:accessor file-document-path #:init-keyword #:path))

(define-method (document-type (document <file-document>))
  (next-method))

(define-method (document-id-term (document <file-document>))
  "Return the ID term for DOCUMENT."
  (string-append "Qfile." (file-document-path document)))

(define-method (document-text (document <file-document>))
  "Return the full text of DOCUMENT."
  (call-with-file-in-git (current-git-repository) (file-document-path document)
    get-string-all))

(define-method (document-term-generator (document <file-document>))
  "Return a term generator indexing DOCUMENT."
  (let ((term-generator (next-method)))
    (increase-termpos! term-generator)
    (index-text! term-generator (file-document-path document))
    term-generator))

(define-method (print (document <file-document>) mset port)
  "Print DOCUMENT in command-line search results. MSET is the xapian
MSet object representing a list of search results."
  (display (colorize-string (document-title document) 'MAGENTA 'UNDERLINE)
           port)
  (newline port)
  (display (colorize-string "DOCUMENT" 'BOLD 'YELLOW) port)
  (display " " port)
  (display (colorize-string (file-document-path document) 'YELLOW)
           port)
  (newline port)
  (let ((snippet (document-snippet document mset)))
    (unless (string-null? snippet)
      (display snippet port)
      (newline port)
      (newline port))))

(define-method (document->sxml (document <file-document>) mset)
  "Render DOCUMENT to SXML. MSET is the xapian MSet object representing
a list of search results."
  `(li (@ (class "search-result search-result-document"))
       (a (@ (href ,(document-web-uri document))
             (class "search-result-title"))
          ,(document-title document))
       ,@(let ((snippet (document-sxml-snippet document mset)))
           (if snippet
               (list `(div (@ (class "search-result-snippet"))
                           ,@snippet))
               (list)))))

(define (read-gemtext-document file)
  "Read gemtext document from FILE. Return a <file-document> object."
  (make <file-document>
    #:title (or (call-with-file-in-git (current-git-repository) file
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
