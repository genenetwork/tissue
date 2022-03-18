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
  #:use-module (srfi srfi-9)
  #:export (tissue-configuration
            tissue-configuration?
            tissue-configuration-project
            tissue-configuration-aliases
            tissue-configuration-web-css
            tissue-configuration-web-tags-path))

(define-record-type <tissue-configuration>
  (make-tissue-configuration project aliases web-css web-tags-path)
  tissue-configuration?
  (project tissue-configuration-project)
  (aliases tissue-configuration-aliases)
  (web-css tissue-configuration-web-css)
  (web-tags-path tissue-configuration-web-tags-path))

(define* (tissue-configuration #:key project (aliases '()) web-css (web-tags-path "/tags"))
  "PROJECT is the name of the project. It is used in the title of the
generated web pages, among other places.

ALIASES is a list of aliases used to refer to authors in the
repository. Each element is in turn a list of aliases an author goes
by, the first of which is the canonical name of that author.

WEB-CSS is the path to a CSS stylesheet. It is relative to the
document root and must begin with a /. If it is #f, no stylesheet is
used in the generated web pages.

WEB-TAGS-PATH is the path relative to the document root where the
per-tag issue listings are put. It must begin with a /. If it is #f,
per-tag issue listings are not generated."
  (make-tissue-configuration project aliases web-css web-tags-path))
