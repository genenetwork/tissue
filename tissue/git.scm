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
  #:use-module (rnrs arithmetic bitwise)
  #:use-module (rnrs conditions)
  #:use-module (rnrs exceptions)
  #:use-module (rnrs hashtables)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-171)
  #:use-module (ice-9 match)
  #:use-module (git)
  #:use-module (git types)
  #:use-module ((system foreign) #:select (%null-pointer
                                           dereference-pointer
                                           pointer->string
                                           string->pointer))
  #:use-module (bytestructures guile)
  #:use-module (tissue utils)
  #:export (reference-set-target!
            reference-symbolic-target
            condition-git-error
            git-top-level
            %current-git-repository
            current-git-repository
            commit-author-date
            git-tracked-file?
            git-tracked-files
            call-with-file-in-git
            file-modification-table
            clone-options))

;; We bind additional functions from libgit2 that are not already
;; bound in guile-git. TODO: Contribute them to guile-git.

(define diff-find-similar!
  (let ((proc (libgit2->procedure* "git_diff_find_similar" '(* *))))
    (lambda (diff)
      (proc (diff->pointer diff) %null-pointer)
      diff)))

(define reference-set-target!
  (let ((proc (libgit2->procedure* "git_reference_set_target" '(* * * *))))
    (lambda* (reference oid #:optional log-message)
      (let ((out (make-double-pointer)))
        (proc out
              (reference->pointer reference)
              (oid->pointer oid)
              (if log-message
                  (string->pointer log-message)
                  %null-pointer))
        (pointer->reference (dereference-pointer out))))))

(define reference-symbolic-target
  (let ((proc (libgit2->procedure '* "git_reference_symbolic_target" '(*))))
    (lambda (reference)
      (pointer->string
       (proc (reference->pointer reference))))))

(define (condition-git-error condition)
  "Return <git-error> object from CONDITION. If none, return #f."
  (and (irritants-condition? condition)
       (find git-error? (condition-irritants condition))))

(define %current-git-repository
  (make-parameter #f))

(define (current-git-repository)
  "Return the current git repository, either the repository specified by
the %current-git-repository parameter or the repository at the current
directory."
  (or (%current-git-repository)
      (repository-open-ext (getcwd) (list))))

(define (git-top-level)
  "Return the top-level directory of the current git repository."
  (dirname (repository-directory (current-git-repository))))

(define (head-tree repository)
  "Return tree of HEAD in REPOSITORY."
  (commit-tree
   (commit-lookup repository
                  (reference-name->oid repository "HEAD"))))

(define (commit-author-date commit)
  "Return the author date of COMMIT as an SRFI-19 date object."
  (let ((time (signature-when (commit-author commit))))
    (time-monotonic->date
     (make-time time-monotonic
                0
                (time-time time))
     (* 60 (time-offset time)))))

(define (git-tracked-file? path repository)
  "Return non-#f if PATH is a file tracked in REPOSITORY. Else, return
#f."
  (guard (c ((let ((git-error (condition-git-error c)))
               (and git-error
                    (= (git-error-code git-error)
                       GIT_ENOTFOUND)))
             #f))
    (tree-entry-bypath (head-tree repository)
                       path)))

(define* (git-tracked-files #:optional (repository (current-git-repository)))
  "Return a list of all files and directories tracked in REPOSITORY. The
returned paths are relative to the top-level directory of REPOSITORY
and do not have a leading slash."
  (tree-list (head-tree repository)))

(define (call-with-file-in-git repository path proc)
  "Call PROC on an input port reading contents of PATH in REPOSITORY."
  (let* ((path-tree-entry (tree-entry-bypath (head-tree repository)
                                             path))
         (path-object (tree-entry->object repository path-tree-entry))
         (blob (blob-lookup repository (object-id path-object))))
    (call-with-port (open-bytevector-input-port (blob-content blob))
      proc)))

(define (commit-deltas repository commit)
  "Return the list of <diff-delta> objects created by COMMIT with
respect to its first parent in REPOSITORY."
  (match (commit-parents commit)
    ((parent _ ...)
     (let ((diff (diff-tree-to-tree repository
                                    (commit-tree parent)
                                    (commit-tree commit))))
       (diff-find-similar! diff)
       (diff-fold (lambda (delta progress result)
                    (cons delta result))
                  (lambda (delta binary result)
                    result)
                  (lambda (delta hunk result)
                    result)
                  (lambda (delta hunk line result)
                    result)
                  (list)
                  diff)))
    (() (list))))

(define (file-modification-table repository)
  "Return a hashtable mapping files to the list of commits in REPOSITORY
that modified them."
  (let ((result (make-hashtable string-hash string=?))
        (renames (make-hashtable string-hash string=?)))
    (fold-commits
     (lambda (commit _)
       (map (lambda (delta)
              ;; Map old filename to current filename if they are
              ;; different. Note that this manner of following renames
              ;; requires a linear git history and will not work with
              ;; branch merges.
              (unless (string=? (diff-file-path (diff-delta-old-file delta))
                                (diff-file-path (diff-delta-new-file delta)))
                (hashtable-set! renames
                                (diff-file-path (diff-delta-old-file delta))
                                (diff-file-path (diff-delta-new-file delta))))
              (hashtable-update! result
                                 ;; If necessary, translate old
                                 ;; filename to current filename.
                                 (hashtable-ref renames
                                                (diff-file-path (diff-delta-old-file delta))
                                                (diff-file-path (diff-delta-old-file delta)))
                                 (cut cons commit <>)
                                 (list)))
            (commit-deltas repository commit)))
     #f
     repository)
    result))

(define* (clone-options #:key bare?)
  (let ((clone-options (make-clone-options)))
    (bytestructure-set!
     (clone-options-bytestructure clone-options)
     'bare
     (if bare? 1 0))
    clone-options))
