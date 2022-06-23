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

(define-module (tissue utils)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 popen)
  #:export (call-with-current-directory
            get-line-dos-or-unix
            memoize-thunk))

(define (call-with-current-directory curdir thunk)
  "Call THUNK with current directory set to CURDIR. Restore current
directory after THUNK returns."
  (let ((original-current-directory (getcwd)))
    (dynamic-wind (cut chdir curdir)
                  thunk
                  (cut chdir original-current-directory))))

(define (get-line-dos-or-unix port)
  "Read line from PORT. This differs from `get-line' in (rnrs io
ports) in that it also supports DOS line endings."
  (let ((line (get-line port)))
    (if (eof-object? line)
        line
        (string-trim-right line #\return))))

(define (memoize-thunk thunk)
  "Return a function memoizing THUNK."
  (let ((result #f))
    (lambda ()
      (unless result
        (set! result (thunk)))
      result)))
