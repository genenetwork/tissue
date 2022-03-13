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
  #:export (call-with-input-pipe
            get-line-dos-or-unix))

(define (call-with-input-pipe proc program . args)
  "Execute PROGRAM ARGS ... in a subprocess with a pipe to it. Call
PROC with an input port to that pipe. Close the pipe once PROC exits,
even if it exits non-locally. Return the value returned by PROC."
  (let ((port #f))
    (dynamic-wind (lambda () (set! port (apply open-pipe* OPEN_READ program args)))
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
