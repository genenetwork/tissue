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
  #:use-module (tissue document)
  #:use-module (tissue issue)
  #:use-module (xapian wrap)
  #:use-module (xapian xapian)
  #:export (search-fold
            search-map))

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
  (let ((query (parse-query
                ;; When query does not mention type or state,
                ;; assume is:open. Assuming is:open is
                ;; implicitly assuming type:issue since only
                ;; issues can have is:open.
                (if (string-null? search-query)
                    "is:open"
                    (if (or (string-contains-ci search-query "type:")
                            (string-contains-ci search-query "is:"))
                        search-query
                        (string-append "is:open AND (" search-query ")")))
                #:stemmer (make-stem "en")
                #:prefixes '(("type" . "XT")
                             ("title" . "S")
                             ("creator" . "A")
                             ("last-updater" . "XA")
                             ("assigned" . "XI")
                             ("keyword" . "K")
                             ("tag" . "K")
                             ("is" . "XS")))))
    (mset-fold (lambda (item result)
                 (proc (call-with-input-string (document-data (mset-item-document item))
                         (compose scm->object read))
                       (MSetIterator-mset-get item)
                       result))
               initial
               (enquire-mset (enquire db query)
                             #:maximum-items maximum-items))))

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
