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

(use-modules (guix build-system gnu)
             (guix gexp)
             (guix git-download)
             ((guix licenses) #:prefix license:)
             (guix packages))

(define %source-dir (dirname (current-filename)))

(define tissue
  (package
    (name "tissue")
    (version "0.1.0")
    (source (local-file %source-dir
                        #:recursive? #t
                        #:select? (git-predicate %source-dir)))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "prefix=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "https://tissue.systemreboot.net")
    (synopsis "Text based issue tracker")
    (description "tissue is a text based issue tracker.")
    (license license:gpl3+)))

tissue
