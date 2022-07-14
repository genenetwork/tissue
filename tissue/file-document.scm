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
  #:use-module (rnrs hashtables)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-171)
  #:use-module (git)
  #:use-module (oop goops)
  #:use-module (term ansi-color)
  #:use-module (tissue commit)
  #:use-module (tissue document)
  #:use-module (tissue git)
  #:use-module (tissue person)
  #:use-module (tissue utils)
  #:use-module (xapian xapian)
  #:export (<file-document>
            file-document-path
            file-document-commits
            file-document-creator
            file-document-created-date
            file-document-last-updater
            file-document-last-updated-date
            read-gemtext-document))

(define-class <file-document> (<document>)
  (path #:accessor file-document-path #:init-keyword #:path)
  ;; List of <commit> objects, oldest first.
  (commits #:accessor file-document-commits #:init-keyword #:commits))

(define file-document-creator
  (compose doc:commit-author first file-document-commits))

(define file-document-created-date
  (compose doc:commit-author-date first file-document-commits))

(define file-document-last-updater
  (compose doc:commit-author last file-document-commits))

(define file-document-last-updated-date
  (compose doc:commit-author-date last file-document-commits))

(define-method (document-type (document <file-document>))
  (next-method))

(define-method (document-id-term (document <file-document>))
  "Return the ID term for DOCUMENT."
  (string-append "Qfile." (file-document-path document)))

(define-method (document-recency-date (document <file-document>))
  "Return a date representing the recency of DOCUMENT."
  (file-document-last-updated-date document))

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
  (display (string-append
            "created "
            (colorize-string (human-date-string
                              (file-document-created-date document))
                             'CYAN)
            " by "
            (colorize-string (file-document-creator document) 'CYAN))
           port)
  (when (> (length (file-document-commits document)) 1)
    (display (string-append (colorize-string "," 'CYAN)
                            " last updated "
                            (colorize-string (human-date-string
                                              (file-document-last-updated-date document))
                                             'CYAN)
                            " by "
                            (colorize-string (file-document-last-updater document)
                                             'CYAN))
             port))
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
       (div (@ (class "search-result-metadata"))
            ,(string-append
              (format #f "created ~a by ~a"
                      (human-date-string (file-document-created-date document))
                      (file-document-creator document))
              (if (> (length (file-document-commits document))
                     1)
                  (format #f ", last updated ~a by ~a"
                          (human-date-string (file-document-last-updated-date document))
                          (file-document-last-updater document))
                  "")))
       ,@(let ((snippet (document-sxml-snippet document mset)))
           (if snippet
               (list `(div (@ (class "search-result-snippet"))
                           ,@snippet))
               (list)))))

(define file-modification-table-for-current-repository
  (memoize-thunk
   (cut file-modification-table (current-git-repository))))

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
    #:path file
    #:commits (map (lambda (commit)
                     (make <commit>
                       #:author (resolve-alias (signature-name (commit-author commit))
                                               (%aliases))
                       #:author-date (commit-author-date commit)))
                   (hashtable-ref (file-modification-table-for-current-repository)
                                  file #f))))
