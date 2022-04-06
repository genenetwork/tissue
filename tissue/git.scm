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

(define-module (tissue git)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-171)
  #:use-module (tissue utils)
  #:export (git-top-level
            git-tracked-files))

(define (git-top-level)
  "Return the top-level directory of the current git
repository."
  (let loop ((curdir (getcwd)))
    (cond
     ((file-exists? (string-append curdir "/.git"))
      curdir)
     ((string=? curdir "/")
      (error "No git top level found"))
     (else
      (loop (dirname curdir))))))

(define (git-tracked-files)
  "Return a list of all files tracked in the current git repository."
  (call-with-input-pipe
      (cut port-transduce (tmap identity) rcons get-line <>)
    "git" "ls-files"))
