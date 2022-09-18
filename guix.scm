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

(use-modules ((gnu packages autotools) #:select (autoconf automake))
             ((gnu packages gettext) #:select (gnu-gettext))
             ((gnu packages guile) #:select (guile-3.0 guile-git))
             ((gnu packages guile-xyz) #:select (guile-filesystem guile-xapian))
             ((gnu packages skribilo) #:prefix guix:)
             (guix build-system gnu)
             (guix gexp)
             (guix git-download)
             ((guix licenses) #:prefix license:)
             (guix packages))

(define %source-dir (dirname (current-filename)))

;; tissue requires an unreleased version of skribilo for its gemtext
;; reader.
(define-public skribilo-latest
  (let ((commit "621eb1945aec8f26f5aee4bdf896f2434e145182")
        (revision "1"))
    (package
      (inherit guix:skribilo)
      (name "skribilo")
      (version (git-version "0.9.5" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.savannah.gnu.org/git/skribilo.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "16rdcvszl9x183y32hjdwns0lkrvkmwd2fsshymspb12k4cxj6i4"))))
      (native-inputs
       (modify-inputs (package-native-inputs guix:skribilo)
         (prepend autoconf)
         (prepend automake)
         (prepend gnu-gettext))))))

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
                 (replace 'patch-source-shebangs
                   (lambda* (#:key inputs #:allow-other-keys)
                     (substitute* "bin/tissue"
                       (("^exec guile")
                        (string-append "exec " (search-input-file inputs "/bin/guile"))))))
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
    (inputs
     (list guile-3.0 guile-filesystem guile-git guile-xapian))
    (propagated-inputs
     (list skribilo-latest))
    (home-page "https://tissue.systemreboot.net")
    (synopsis "Text based issue tracker")
    (description "tissue is a text based issue tracker.")
    (license license:gpl3+)))

tissue
