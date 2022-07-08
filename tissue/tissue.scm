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

(define-module (tissue tissue)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 match)
  #:use-module (tissue git)
  #:export (tissue-configuration
            tissue-configuration?
            tissue-configuration-project
            tissue-configuration-aliases
            tissue-configuration-indexed-documents
            tissue-configuration-web-css
            tissue-configuration-web-files
            gemtext-files-in-directory))

(define-record-type <tissue-configuration>
  (make-tissue-configuration project aliases indexed-documents
                             web-css web-files)
  tissue-configuration?
  (project tissue-configuration-project)
  (aliases tissue-configuration-aliases)
  (indexed-documents delayed-tissue-configuration-indexed-documents)
  (web-css tissue-configuration-web-css)
  (web-files delayed-tissue-configuration-web-files))

(define tissue-configuration-indexed-documents
  (compose force delayed-tissue-configuration-indexed-documents))

(define tissue-configuration-web-files
  (compose force delayed-tissue-configuration-web-files))

(define* (gemtext-files-in-directory #:optional directory)
  "Return a list of all gemtext files in DIRECTORY tracked in the
current git repository. If DIRECTORY is #f, return the list of all
gemtext files tracked in the current git repository regardless of
which directory they are in."
  (filter (lambda (filename)
            (and (or (not directory)
                     (string-prefix? directory filename))
                 (string-suffix? ".gmi" filename)))
          (git-tracked-files (current-git-repository))))

(define (pairify lst)
  "Return a list of pairs of successive elements of LST. For example,

(pairify (list 1 2 3 4 5 6))
=> ((1 . 2) (3 . 4) (5 . 6))"
  (match lst
    (() '())
    ((first second tail ...)
     (cons (cons first second)
           (pairify tail)))))

(define-syntax tissue-configuration
  (lambda (x)
    (syntax-case x ()
      ((_ args ...)
       #`((lambda* (#:key project (aliases '())
                    (indexed-documents (delay '()))
                    web-css (web-files (delay '())))
            "PROJECT is the name of the project. It is used in the title of the
generated web pages, among other places.

ALIASES is a list of aliases used to refer to authors in the
repository. Each element is in turn a list of aliases an author goes
by, the first of which is the canonical name of that author.

INDEXED-DOCUMENTS is a list of <indexed-documents> objects
representing documents to index.

WEB-CSS is the path to a CSS stylesheet. It is relative to the
document root and must begin with a /. If it is #f, no stylesheet is
used in the generated web pages.

WEB-FILES is a list of <file> objects representing files to be written
to the web output."
            (make-tissue-configuration project aliases
                                       indexed-documents web-css web-files))
          #,@(append-map (match-lambda
                           ((key . value)
                            (if (memq (syntax->datum key)
                                      (list #:indexed-documents #:web-files))
                                #`(#,key (delay #,value))
                                #`(#,key #,value))))
                         (pairify #'(args ...))))))))
