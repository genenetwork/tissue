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

(define-module (tissue search)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (tissue document)
  #:use-module (tissue issue)
  #:use-module (tissue utils)
  #:use-module (xapian wrap)
  #:use-module ((xapian xapian) #:renamer (lambda (symbol)
                                            (case symbol
                                              ((parse-query) 'xapian:parse-query)
                                              (else symbol))))
  #:export (parse-query
            boolean-query?
            search-fold
            search-map))

(define (make-query-parser stemmer prefixes boolean-prefixes)
  "Return a query parser with STEMMER, PREFIXES and
BOOLEAN-PREFIXES. PREFIXES and BOOLEAN-PREFIXES are association lists
mapping field names to prefixes."
  (let ((query-parser (new-QueryParser)))
    (QueryParser-set-stemmer query-parser stemmer)
    (for-each (match-lambda
                ((field . prefix)
                 (QueryParser-add-prefix query-parser field prefix)))
              prefixes)
    (for-each (match-lambda
                ((field . prefix)
                 (QueryParser-add-boolean-prefix query-parser field prefix)))
              boolean-prefixes)
    query-parser))

(define %prefixes
  '(("title" . "S")))

(define %boolean-prefixes
  '(("type" . "XT")
    ("creator" . "A")
    ("lastupdater" . "XA")
    ("assigned" . "XI")
    ("keyword" . "K")
    ("tag" . "K")
    ("is" . "XS")))

(define query-parser
  (make-query-parser (make-stem "en") %prefixes %boolean-prefixes))

(define (parse-query search-query)
  "Parse SEARCH-QUERY and return a xapian Query object."
  (if (string-blank? search-query)
      (Query-MatchAll)
      (QueryParser-parse-query query-parser search-query)))

(define term-ref TermIterator-get-term)

(define (query-terms-every pred query)
  "Test whether every term in QUERY satisfies PRED. If so, return the
result of the last PRED call. If not, return #f. The calls to PRED are
made successively on the first, second, third, etc. term, and stopped
when PRED returns #f."
  (let loop ((head (Query-get-terms-begin query))
             (result #t))
    (cond
     ((TermIterator-equals head (Query-get-terms-end query))
      result)
     ((pred head)
      => (lambda (result)
           (TermIterator-next head)
           (loop head result)))
     (else #f))))

(define (boolean-query? query)
  "Return #t if QUERY contains only boolean terms. Else, return #f."
  (query-terms-every (lambda (term)
                       (any (match-lambda
                              ((field . prefix)
                               (string-contains? (term-ref term) prefix)))
                            %boolean-prefixes))
                     query))

(define* (search-fold proc initial db search-query
                      #:key (offset 0) (maximum-items (database-document-count db)))
  "Search xapian database DB using SEARCH-QUERY and fold over the
results using PROC and INITIAL.

PROC is invoked as (PROC DOCUMENT MSET PREVIOUS). DOCUMENT is an
instance of <document> or one of its subclasses. MSET is the xapian
MSet object representing the search results. PREVIOUS is the return
from the previous invocation of PROC, or the given INITIAL for the
first call.

OFFSET specifies the number of items to ignore at the beginning of the
result set. MAXIMUM-ITEMS specifies the maximum number of items to
return."
  (mset-fold (lambda (item result)
               (proc (call-with-input-string (document-data (mset-item-document item))
                       (compose scm->object read))
                     (MSetIterator-mset-get item)
                     result))
             initial
             (enquire-mset (let* ((query (parse-query search-query))
                                  (enquire (enquire db query)))
                             ;; Sort by recency date (slot 0) when
                             ;; query is strictly boolean.
                             (when (boolean-query? query)
                               (Enquire-set-sort-by-value enquire 0 #t))
                             enquire)
                           #:maximum-items maximum-items)))

(define* (search-map proc db search-query
                     #:key (offset 0) (maximum-items (database-document-count db)))
  "Search xapian database DB using SEARCH-QUERY and map over the results
using PROC.

PROC is invoked as (PROC DOCUMENT MSET). DOCUMENT is an instance of
<document> or one of its subclasses. MSET is the xapian MSet object
representing the search results.

OFFSET specifies the number of items to ignore at the beginning of the
result set. MAXIMUM-ITEMS specifies the maximum number of items to
return."
  (reverse
   (search-fold (lambda (document mset result)
                  (cons (proc document mset)
                        result))
                '()
                db
                search-query
                #:maximum-items maximum-items)))
