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
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 popen)
  #:export (string-remove-prefix
            human-date-string
            call-with-current-directory
            get-line-dos-or-unix
            memoize-thunk))

(define (string-remove-prefix prefix str)
  "Remove PREFIX from STR."
  (substring str (string-length prefix)))

(define (human-date-string date)
  "Return a human readable rendering of DATE."
  (let ((elapsed-time
         (time-second
          (time-difference (date->time-monotonic (current-date))
                           (date->time-monotonic date)))))
    (cond
     ((< elapsed-time (* 2 60))
      (format #f "~a seconds ago" elapsed-time))
     ((< elapsed-time (* 2 60 60))
      (format #f "~a minutes ago" (round (/ elapsed-time 60))))
     ((< elapsed-time (* 2 24 60 60))
      (format #f "~a hours ago" (round (/ elapsed-time 60 60))))
     ((< elapsed-time (* 2 7 24 60 60))
      (format #f "~a days ago" (round (/ elapsed-time 60 60 24))))
     ((< elapsed-time (* 2 30 24 60 60))
      (format #f "~a weeks ago" (round (/ elapsed-time 60 60 24 7))))
     (else
      (format #f "on ~a" (date->string date "~b ~d ~Y"))))))

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
