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
  #:use-module (ice-9 filesystem)
  #:use-module (ice-9 popen)
  #:export (string-blank?
            string-contains?
            string-remove-prefix
            string-remove-suffix
            human-date-string
            call-with-current-directory
            call-with-temporary-directory
            call-with-output-pipe
            get-line-dos-or-unix
            memoize-thunk))

(define (string-blank? str)
  "Return #t if STR contains only whitespace. Else, return #f."
  (string-every char-set:whitespace str))

(define (string-contains? str1 str2)
  "Return #t if STR1 contains STR2. Else, return #f. This is different
from string-contains in that it does not return the index in STR1
where STR2 occurs as a substring."
  (and (string-contains str1 str2)
       #t))

(define (string-remove-prefix prefix str)
  "Remove PREFIX from STR."
  (substring str (string-length prefix)))

(define (string-remove-suffix suffix str)
  "Remove SUFFIX from STR."
  (substring str 0 (- (string-length str)
                      (string-length suffix))))

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

(define (call-with-temporary-directory proc)
  "Call PROC with a new temporary directory, and delete it when PROC
returns or exits non-locally."
  (let ((temporary-directory (mkdtemp "XXXXXX")))
    (dynamic-wind (const #t)
                  (cut proc temporary-directory)
                  (lambda ()
                    (when (file-exists? temporary-directory)
                      (delete-file-recursively temporary-directory))))))

(define (call-with-output-pipe proc program . args)
  "Execute PROGRAM ARGS ... in a subprocess with a pipe to it. Call PROC
with an output port to that pipe. Close the pipe once PROC exits, even
if it exits non-locally. Return the value returned by PROC."
  (let ((port #f))
    (dynamic-wind
      (cut set! port (apply open-pipe* OPEN_WRITE program args))
      (cut proc port)
      (lambda ()
        (let ((return-value (status:exit-val (close-pipe port))))
          (unless (and return-value
                       (zero? return-value))
            (error "Invocation of program failed" (cons program args))))))))

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
