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
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
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
  #:use-module (tissue issue)
  #:use-module (tissue utils)
  #:export (issue-listing
            build-website))

(define %tags-path
  (make-parameter #f))

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
                                (creator #f) (created-date #f) (created-relative-date #f)
                                (last-updater #f) (last-updated-date #f) (last-updated-relative-date #f)
                                (assigned #f) (keywords #f) (open #f)
                                (tasks #f) (completed-tasks #f)
                                (posts #f))
  (new container
       (markup 'issue-list-item)
       (ident (or ident (symbol->string (gensym "issue-list-item"))))
       (class class)
       (loc &invocation-location)
       (required-options '(#:file #:title
                           #:creator #:created-date #:created-relative-date
                           #:last-updater #:last-updated-date #:last-updated-relative-date
                           #:assigned #:keywords #:open
                           #:tasks #:completed-tasks
                           #:posts))
       (options `((#:file ,file)
                  (#:title ,title)
		  (#:creator ,creator)
                  (#:created-date ,created-date)
                  (#:created-relative-date ,created-relative-date)
                  (#:last-updater ,last-updater)
                  (#:last-updated-date ,last-updated-date)
                  (#:last-updated-relative-date ,last-updated-relative-date)
                  (#:assigned ,assigned)
                  (#:keywords ,keywords)
                  (#:open ,open)
                  (#:tasks ,tasks)
                  (#:completed-tasks ,completed-tasks)
                  (#:posts ,posts)
		  ,@(the-options opts #:ident #:class
                                 #:file #:title
                                 #:creator #:created-date #:created-relative-date
                                 #:last-updater #:last-updated-date #:last-updated-relative-date
                                 #:assigned #:keywords #:open
                                 #:tasks #:completed-tasks
                                 #:posts)))
       (body (the-body opts))))

(define (issue-list-item-markup-writer-action markup engine)
  (sxml->xml
   `(li (@ (class "issue-list-item"))
        (a (@ (href ,(replace-extension
                      (string-append "/" (markup-option markup #:file))
                      "html")))
           ,(markup-option markup #:title))
        ,@(map (lambda (tag)
                 (let ((words (string-split tag (char-set #\- #\space))))
                   `(a (@ (href ,(string-append (%tags-path) "/" tag ".html"))
                          (class ,(string-append "tag"
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
                        (markup-option markup #:created-relative-date)
                        (markup-option markup #:creator))
                (if (> (markup-option markup #:posts) 1)
                    (format ", last updated ~a by ~a"
                            (markup-option markup #:last-updated-relative-date)
                            (markup-option markup #:last-updater))
                    ""))))))

(markup-writer 'issue-list-item
               (find-engine 'html)
               #:options '(#:file #:title
                           #:creator #:created-date #:created-relative-date
                           #:last-updater #:last-updated-date #:last-updated-relative-date
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
                                   #:created-relative-date (issue-created-relative-date issue)
                                   #:last-updater (issue-last-updater issue)
                                   #:last-updated-date (issue-last-updated-date issue)
                                   #:last-updated-relative-date (issue-last-updated-relative-date issue)
                                   #:assigned (issue-assigned issue)
                                   #:keywords (issue-keywords issue)
                                   #:open (issue-open issue)
                                   #:tasks (issue-tasks issue)
                                   #:completed-tasks (issue-completed-tasks issue)
                                   #:posts (issue-posts issue)))
                issues)))

(define* (build-issue-listing issues output-file #:key title)
  "Write an issues listing page listing ISSUES to OUTPUT-FILE."
  (mkdir-p (dirname output-file))
  (with-output-to-file output-file
    (cut evaluate-document
         (document #:title title
                   (issue-listing issues))
         (find-engine 'html))))

;; TODO: Use guile-filesystem.
(define* (build-website output-directory #:key title (tags-path "/tags"))
  "Export current git repository to OUTPUT-DIRECTORY as a website.

TITLE is the title to use head of the generated HTML, among other
places. TAGS-PATH is the path relative to the document root where the
per-tag issue listings are put. It must begin with a /. If it is #f,
per-tag issue listings are not generated."
  (mkdir-p output-directory)
  ;; Publish files.
  (call-with-input-pipe
   (lambda (port)
     (port-transduce
      (tmap (lambda (input-file)
              (unless (string-prefix? "." (basename input-file))
                (let* ((relative-input-file input-file)
                       (output-file (string-append output-directory "/"
                                                   (if (or (string-suffix? ".gmi" relative-input-file)
                                                           (string-suffix? ".skb" relative-input-file))
                                                       (replace-extension relative-input-file "html")
                                                       relative-input-file))))
                  (display (format "~a -> ~a~%" input-file output-file))
                  (mkdir-p (dirname output-file))
                  (if (or (string-suffix? ".gmi" input-file)
                          (string-suffix? ".skb" input-file))
                      (with-output-to-file output-file
                        (cut evaluate-document
                             (call-with-input-file input-file
                               (cut evaluate-ast-from-port <>
                                    #:reader ((reader:make (lookup-reader
                                                            (cond
                                                             ((string-suffix? ".gmi" input-file)
                                                              'gemtext)
                                                             ((string-suffix? ".skb" input-file)
                                                              'skribe)))))))
                             (find-engine 'html)))
                      (copy-file input-file output-file))))))
      rcons get-line port))
   "git" "ls-files")
  (parameterize ((%tags-path tags-path))
    ;; Publish index.
    (let ((output-file (string-append output-directory "/index.html")))
      (display (format "~a~%" output-file))
      (build-issue-listing (reverse (issues)) output-file
                           #:title title))
    ;; Publish per-tag listings.
    (when tags-path
      (for-each (lambda (tag)
                  (let ((output-file (string-append output-directory
                                                    tags-path "/" tag ".html")))
                    (display (format "tag: ~a -> ~a~%" tag output-file))
                    (build-issue-listing (reverse (filter (lambda (issue)
                                                            (member tag (issue-keywords issue)))
                                                          (issues)))
                                         output-file
                                         #:title title)))
                (delete-duplicates (append-map issue-keywords (issues)))))))
