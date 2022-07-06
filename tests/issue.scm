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

(import (rnrs hashtables)
        (srfi srfi-64)
        (srfi srfi-71)
        (tissue issue))

(define hashtable-prepend!
  (@@ (tissue issue) hashtable-prepend!))

(define list-line->alist
  (@@ (tissue issue) list-line->alist))

(define file-details
  (@@ (tissue issue) file-details))

(define (hashtable->alist hashtable)
  (let ((keys values (hashtable-entries hashtable)))
    (map cons
         (vector->list keys)
         (vector->list values))))

(test-begin "issue")

(test-equal "Parse list line with multiple key-value pairs"
  '((status "unclear")
    (keywords "dangerous feature" "ui"))
  (list-line->alist "* keywords: ui, dangerous feature, status: unclear"))

(test-equal "Parse list line with repeated keys"
  '((keywords "foo")
    (status "unclear")
    (keywords "dangerous feature" "ui"))
  (list-line->alist "* keywords: ui, dangerous feature, status: unclear, keywords: foo"))

(test-equal "Parse key-value list lines case insensitively"
  '((keywords "dangerous feature" "ui"))
  (list-line->alist "* Keywords: ui, dangerous feature"))

(test-equal "Return keywords in order"
  '((keywords "foo" "bar" "fubar"))
  (call-with-input-string "* keywords: foo, bar, fubar"
    (compose hashtable->alist file-details)))

(let ((hashtable (make-eq-hashtable)))
  (hashtable-set! hashtable 'keywords (list "foo" "bar"))
  (hashtable-prepend! hashtable 'keywords (list "foobar" "fubar"))
  (test-equal "hashtable-prepend! should prepend, not append"
    '((keywords "fubar" "foobar" "foo" "bar"))
    (hashtable->alist hashtable)))

(test-equal "Do not parse definition-like lines as key-value pairs"
  '()
  (call-with-input-string "* term: This is a definition of term."
    (compose hashtable->alist file-details)))

(test-end "issue")
