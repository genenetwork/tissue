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

(define-module (tissue person)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (xapian xapian)
  #:export (%aliases
            resolve-alias
            index-person!))

(define %aliases
  (make-parameter #f))

(define (resolve-alias name aliases)
  "Resolve NAME against ALIASES, a list of aliases. ALIASES should be
in the form of the argument of the same name to `tissue-configuration'
in (tissue tissue). If no alias is found, NAME is returned as such."
  (cond
   ((find (cut member name <>)
          aliases)
    => (match-lambda
         ((canonical-name _ ...) canonical-name)
         (() name)))
   (else name)))

(define (index-person! term-generator name prefix)
  "Index all aliases of person of canonical NAME using TERM-GENERATOR
with PREFIX."
  (for-each (cut index-text! term-generator <> #:prefix prefix)
            (or (assoc name (%aliases))
                (list))))
