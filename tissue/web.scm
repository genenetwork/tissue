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

(define-module (tissue web)
  #:use-module (rnrs exceptions)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-28)
  #:use-module (srfi srfi-171)
  #:use-module (skribilo ast)
  #:use-module (skribilo engine)
  #:use-module (skribilo evaluator)
  #:use-module (skribilo lib)
  #:use-module (skribilo package base)
  #:use-module (skribilo reader)
  #:use-module (skribilo utils keywords)
  #:use-module (skribilo writer)
  #:use-module (sxml simple)
  #:use-module (web uri)
  #:use-module (tissue conditions)
  #:use-module (tissue issue)
  #:use-module (tissue utils)
  #:export (%project-name
            %tags-path
            file
            file?
            file-name
            file-writer
            issue-listing
            replace-extension
            copier
            gemtext-reader
            gemtext-exporter
            skribe-exporter
            tag-issue-lister
            tag-pages
            build-website))

(define %project-name
  (make-parameter #f))

(define %tags-path
  (make-parameter #f))

(define-record-type <file>
  (file name writer)
  file?
  (name file-name)
  (writer file-writer))

(define (mkdir-p directory)
  "Create DIRECTORY and all its parents."
  (unless (or (string=? directory "/")
              (string=? directory "."))
    (mkdir-p (dirname directory)))
  (unless (file-exists? directory)
    (mkdir directory)))

(define (replace-extension file new-extension)
  "Return a new filename where the extension of FILE is replaced with
NEW-EXTENSION."
  (string-append (substring file 0 (1+ (string-index-right file #\.)))
                 new-extension))

(define-markup (issue-list-item #:rest opts
		                #:key (ident #f) (class "issue-list-item")
                                (file #f) (title #f)
                                (creator #f) (created-date #f)
                                (last-updater #f) (last-updated-date #f)
                                (assigned #f) (keywords #f) (open #f)
                                (tasks #f) (completed-tasks #f)
                                (posts #f))
  (new container
       (markup 'issue-list-item)
       (ident (or ident (symbol->string (gensym "issue-list-item"))))
       (class class)
       (loc &invocation-location)
       (required-options '(#:file #:title
                           #:creator #:created-date
                           #:last-updater #:last-updated-date
                           #:assigned #:keywords #:open
                           #:tasks #:completed-tasks
                           #:posts))
       (options `((#:file ,file)
                  (#:title ,title)
		  (#:creator ,creator)
                  (#:created-date ,created-date)
                  (#:last-updater ,last-updater)
                  (#:last-updated-date ,last-updated-date)
                  (#:assigned ,assigned)
                  (#:keywords ,keywords)
                  (#:open ,open)
                  (#:tasks ,tasks)
                  (#:completed-tasks ,completed-tasks)
                  (#:posts ,posts)
		  ,@(the-options opts #:ident #:class
                                 #:file #:title
                                 #:creator #:created-date
                                 #:last-updater #:last-updated-date
                                 #:assigned #:keywords #:open
                                 #:tasks #:completed-tasks
                                 #:posts)))
       (body (the-body opts))))

(define (sanitize-string str)
  "Downcase STR and replace spaces with hyphens."
  (string-map (lambda (c)
                (case c
                  ((#\space) #\-)
                  (else c)))
              (string-downcase str)))

(define (issue-list-item-markup-writer-action markup engine)
  (sxml->xml
   `(li (@ (class "issue-list-item"))
        (a (@ (href ,(string-append
                      "/" (encode-and-join-uri-path
                           (string-split
                            (replace-extension (markup-option markup #:file)
                                               "html")
                            #\/)))))
           ,(markup-option markup #:title))
        ,@(map (lambda (tag)
                 (let ((words (string-split tag (char-set #\- #\space))))
                   `(a (@ (href ,(string-append (%tags-path) "/"
                                                (uri-encode (sanitize-string tag))
                                                ".html"))
                          (class ,(string-append "tag"
                                                 (string-append " tag-" (sanitize-string tag))
                                                 (if (not (null? (lset-intersection
                                                                  string=? words
                                                                  (list "bug" "critical"))))
                                                     " tag-bug"
                                                     "")
                                                 (if (not (null? (lset-intersection
                                                                  string=? words
                                                                  (list "progress"))))
                                                     " tag-progress"
                                                     "")
                                                 (if (not (null? (lset-intersection
                                                                  string=? words
                                                                  (list "chore"))))
                                                     " tag-chore"
                                                     "")
                                                 (if (not (null? (lset-intersection
                                                                  string=? words
                                                                  (list "enhancement" "feature"))))
                                                     " tag-feature"
                                                     ""))))
                       ,tag)))
               (markup-option markup #:keywords))
        (span (@ (class "issue-list-item-metadata"))
              ,(string-append
                (format " opened ~a by ~a"
                        (date->string (markup-option markup #:created-date)
                                      "~b ~d ~Y")
                        (markup-option markup #:creator))
                (if (> (length (markup-option markup #:posts))
                       1)
                    (format ", last updated ~a by ~a"
                            (date->string (markup-option markup #:last-updated-date)
                                          "~b ~d ~Y")
                            (markup-option markup #:last-updater))
                    "")
                (if (zero? (markup-option markup #:tasks))
                    ""
                    (format "; ~a of ~a tasks done"
                            (markup-option markup #:completed-tasks)
                            (markup-option markup #:tasks))))))))

(markup-writer 'issue-list-item
               (find-engine 'html)
               #:options '(#:file #:title
                           #:creator #:created-date
                           #:last-updater #:last-updated-date
                           #:assigned #:keywords #:open
                           #:tasks #:completed-tasks
                           #:posts)
               #:action issue-list-item-markup-writer-action)

(define* (issue-listing #:optional (issues (reverse (issues))))
  "Return an issue listing for ISSUES, a list of <issue> objects. By
default, all issues are listed newest first."
  (itemize (map (lambda (issue)
                  (issue-list-item #:file (issue-file issue)
                                   #:title (issue-title issue)
                                   #:creator (issue-creator issue)
                                   #:created-date (issue-created-date issue)
                                   #:last-updater (issue-last-updater issue)
                                   #:last-updated-date (issue-last-updated-date issue)
                                   #:assigned (issue-assigned issue)
                                   #:keywords (issue-keywords issue)
                                   #:open (issue-open? issue)
                                   #:tasks (issue-tasks issue)
                                   #:completed-tasks (issue-completed-tasks issue)
                                   #:posts (issue-posts issue)))
                issues)))

(define (exporter file proc)
  "Return a writer function that exports FILE using PROC. PROC is
passed two arguments---the input port to read from and the output port
to write to."
  (lambda (out)
    ;; Files may be renamed or deleted, but not committed. Therefore,
    ;; raise an exception if the file does not exist.
    (if (file-exists? file)
        (call-with-input-file file
          (cut proc <> out))
        (raise (issue-file-not-found-error file)))))

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

(define* (tag-issue-lister tag #:key title)
  "Return a writer function that writes a issue listing of issues with TAG.

TITLE is the title to use in the head of the generated HTML, among
other places."
  (lambda (port)
    (with-output-to-port port
      (cut evaluate-document
           (document #:title title
                     (issue-listing
                      (reverse (filter (lambda (issue)
                                         (member tag (issue-keywords issue)))
                                       (issues)))))
           (find-engine 'html)))))

(define (tag-pages)
  "Return a list of <file> objects representing tag pages to be
exported to the web output."
  (map (lambda (tag)
         (file (string-append (%tags-path) "/" (sanitize-string tag) ".html")
               (tag-issue-lister tag
                                 #:title (string-append tag " — " (%project-name)))))
       (delete-duplicates (append-map issue-keywords (issues)))))

(define (with-current-directory directory thunk)
  "Change current directory to DIRECTORY, execute THUNK and restore
original current directory."
  (let ((previous-current-directory (getcwd)))
    (dynamic-wind (const #t)
                  thunk
                  (cut chdir previous-current-directory))))

;; TODO: Use guile-filesystem.
(define* (build-website repository-top-level output-directory css files)
  "Export git repository with REPOSITORY-TOP-LEVEL to OUTPUT-DIRECTORY
as a website.

CSS is the path to a CSS stylesheet. If it is #f, no stylesheet is
included in the generated web pages.

TAGS-PATH is the path relative to the document root where the per-tag
issue listings are put. It must begin with a /. If it is #f, per-tag
issue listings are not generated.

FILES is a list of <file> objects representing files to be written to
the web output."
  ;; Set CSS.
  (when css
    (engine-custom-set! (find-engine 'html) 'css css))
  ;; Create output directory.
  (mkdir-p output-directory)
  ;; Write each of the <file> objects.
  (for-each (lambda (file)
              (let ((output-file
                     (string-append output-directory "/" (file-name file))))
                (display output-file (current-error-port))
                (newline (current-error-port))
                (mkdir-p (dirname output-file))
                (call-with-output-file output-file
                  (lambda (port)
                    (with-current-directory repository-top-level
                                            (cut (file-writer file) port))))))
            files))
