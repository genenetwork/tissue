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
     (list #:make-flags #~(list (string-append "prefix=" #$output))
           #:modules `(((guix build guile-build-system)
                        #:select (target-guile-effective-version))
                       ,@%gnu-build-system-modules)
           #:phases
           (with-imported-modules '((guix build guile-build-system))
             #~(modify-phases %standard-phases
                 (delete 'configure)
                 (add-after 'install 'wrap
                   (lambda* (#:key inputs outputs #:allow-other-keys)
                     (let ((out (assoc-ref outputs "out"))
                           (effective-version (target-guile-effective-version)))
                       (wrap-program (string-append out "/bin/tissue")
                         `("GUILE_LOAD_PATH" prefix
                           (,(string-append out "/share/guile/site/" effective-version)
                            ,(getenv "GUILE_LOAD_PATH")))
                         `("GUILE_LOAD_COMPILED_PATH" prefix
                           (,(string-append out "/lib/guile/" effective-version "/site-ccache")
                            ,(getenv "GUILE_LOAD_COMPILED_PATH")))))))))))
    (home-page "https://tissue.systemreboot.net")
    (synopsis "Text based issue tracker")
    (description "tissue is a text based issue tracker.")
    (license license:gpl3+)))

tissue
