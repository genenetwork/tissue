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

(define-module (tissue commit)
  #:use-module (oop goops)
  #:use-module (term ansi-color)
  #:use-module (git)
  #:use-module (tissue document)
  #:use-module (tissue git)
  #:use-module (tissue person)
  #:use-module (tissue utils)
  #:export (<commit>
            commit-hash
            doc:commit-author
            doc:commit-author-date
            commits-in-current-repository))

(define-class <commit> (<document>)
  (hash #:getter commit-hash #:init-keyword #:hash)
  ;; We prefix commit-author and commit-author-date with doc: in order
  ;; to not conflict with similarly named functions from (git) and
  ;; (tissue git).
  (author #:getter doc:commit-author #:init-keyword #:author)
  (author-date #:getter doc:commit-author-date #:init-keyword #:author-date))

(define-method (document-id-term (commit <commit>))
  "Return the ID term for DOCUMENT."
  (string-append "Qcommit." (commit-hash commit)))

(define-method (document-boolean-terms (commit <commit>))
  "Return the boolean terms in COMMIT."
  (list (string-append "Qcommit." (commit-hash commit))
        (string-append "A" (doc:commit-author commit))))

(define-method (document-recency-date (commit <commit>))
  "Return a date representing the recency of DOCUMENT"
  (doc:commit-author-date commit))

(define-method (document-snippet-source-text (commit <commit>))
  "Return the source text for COMMIT from which to extract a search
result snippet."
  (commit-body
   (commit-lookup (current-git-repository)
                  (string->oid (commit-hash commit)))))

(define-method (document-text (commit <commit>))
  "Return the full text of COMMIT."
  (commit-message
   (commit-lookup (current-git-repository)
                  (string->oid (commit-hash commit)))))

(define-method (print (commit <commit>) mset port)
  "Print COMMIT, a <commit> object, to PORT as part of command-line
search results. MSET is the xapian MSet object representing a list of
search results."
  (display (colorize-string (document-title commit) 'MAGENTA 'UNDERLINE)
           port)
  (newline port)
  (display (colorize-string "COMMIT" 'BOLD 'YELLOW)
           port)
  (display " " port)
  (display (colorize-string (commit-hash commit) 'YELLOW)
           port)
  (newline port)
  (display (string-append
            "authored "
            (colorize-string (human-date-string (doc:commit-author-date commit)) 'CYAN)
            " by "
            (colorize-string (doc:commit-author commit) 'CYAN))
           port)
  (newline port)
  (let ((snippet (document-snippet commit mset)))
    (unless (string-null? snippet)
      (display snippet port)
      (newline port)
      (newline port))))

(define-method (document->sxml (commit <commit>) mset)
  "Render COMMIT, a <commit> object, to SXML. MSET is the xapian MSet
object representing a list of search results."
  `(li (@ (class ,(string-append "search-result search-result-commit")))
       (a (@ (href ,(document-web-uri commit))
             (class "search-result-title"))
          ,(document-title commit))
       (div (@ (class "search-result-metadata"))
            ,(string-append
              (format #f "authored ~a by ~a"
                      (human-date-string (doc:commit-author-date commit))
                      (doc:commit-author commit))))
       ,@(let ((snippet (document-sxml-snippet commit mset)))
           (if snippet
               (list `(div (@ (class "search-result-snippet"))
                           ,@snippet))
               (list)))))

(define (repository-commits repository)
  "Return a list of <commit> objects representing commits in
REPOSITORY."
  (fold-commits (lambda (commit result)
                  (cons (make <commit>
                          #:title (commit-summary commit)
                          #:hash (oid->string (commit-id commit))
                          #:author (resolve-alias (signature-name (commit-author commit))
                                                  (%aliases))
                          #:author-date (commit-author-date commit))
                        result))
                (list)
                repository))

(define (commits-in-current-repository)
  "Return a list of <commit> objects representing commits in current
repository."
  (repository-commits (current-git-repository)))
