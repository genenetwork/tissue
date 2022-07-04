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

(define-module (tissue web static)
  #:use-module (rnrs exceptions)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-28)
  #:use-module (srfi srfi-171)
  #:use-module (ice-9 filesystem)
  #:use-module (skribilo engine)
  #:use-module (skribilo evaluator)
  #:use-module (skribilo reader)
  #:use-module (web uri)
  #:use-module (tissue git)
  #:use-module (tissue issue)
  #:use-module (tissue utils)
  #:export (%project-name
            file
            file?
            file-name
            file-writer
            replace-extension
            copier
            gemtext-reader
            gemtext-exporter
            skribe-exporter
            build-website))

(define %project-name
  (make-parameter #f))

(define-record-type <file>
  (file name writer)
  file?
  (name file-name)
  (writer file-writer))

(define (replace-extension file new-extension)
  "Return a new filename where the extension of FILE is replaced with
NEW-EXTENSION."
  (string-append (substring file 0 (1+ (string-index-right file #\.)))
                 new-extension))

(define (exporter file proc)
  "Return a writer function that exports FILE using PROC. PROC is
passed two arguments---the input port to read from and the output port
to write to."
  (lambda (out)
    (call-with-file-in-git (current-git-repository) file
      (cut proc <> out))))

(define (copier file)
  "Return a writer function that copies FILE."
  (exporter file
            (lambda (in out)
              (port-transduce (tmap (cut put-bytevector out <>))
                              (const #t)
                              get-bytevector-some
                              in))))

(define (gemtext-reader)
  "Return a skribilo reader for gemtext."
  ((reader:make (lookup-reader 'gemtext))
   ;; Relax the gemtext standard by joining adjacent lines.
   #:join-lines? #t))

(define* (gemtext-exporter file #:optional (reader (gemtext-reader)))
  "Return a writer function that exports FILE, a gemtext file."
  (exporter file
            (lambda (in out)
              (with-output-to-port out
                (cut evaluate-document
                     (evaluate-ast-from-port in #:reader reader)
                     (find-engine 'html))))))

(define* (skribe-exporter file #:optional (reader (make-reader 'skribe)))
  "Return a writer function that exports FILE, a skribe file."
  (exporter file
            (lambda (in out)
              (with-output-to-port out
                (cut evaluate-document
                     (evaluate-ast-from-port in #:reader reader)
                     (find-engine 'html))))))

(define (with-current-directory directory thunk)
  "Change current directory to DIRECTORY, execute THUNK and restore
original current directory."
  (let ((previous-current-directory (getcwd)))
    (dynamic-wind (const #t)
                  thunk
                  (cut chdir previous-current-directory))))

(define* (build-website repository-top-level output-directory css files
                        #:key (log-port (current-error-port)))
  "Export git repository with REPOSITORY-TOP-LEVEL to OUTPUT-DIRECTORY
as a website.

CSS is the path to a CSS stylesheet. If it is #f, no stylesheet is
included in the generated web pages.

TAGS-PATH is the path relative to the document root where the per-tag
issue listings are put. It must begin with a /. If it is #f, per-tag
issue listings are not generated.

FILES is a list of <file> objects representing files to be written to
the web output.

Log to LOG-PORT. When LOG-PORT is #f, do not log."
  ;; Set CSS.
  (when css
    (engine-custom-set! (find-engine 'html) 'css css))
  ;; Create output directory.
  (make-directories output-directory)
  ;; Write each of the <file> objects.
  (for-each (lambda (file)
              (let ((output-file
                     (string-append output-directory "/" (file-name file))))
                (when log-port
                  (display output-file log-port)
                  (newline log-port))
                (make-directories (dirname output-file))
                (call-with-output-file output-file
                  (lambda (port)
                    (with-current-directory repository-top-level
                                            (cut (file-writer file) port))))))
            files))
