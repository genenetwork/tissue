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
  #:use-module (rnrs hashtables)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-171)
  #:use-module (ice-9 match)
  #:use-module (git)
  #:use-module (git types)
  ;; There are many name conflicts between (system foreign). So, we
  ;; carefully select a few and prefix the rest.
  #:use-module ((system foreign) #:select (%null-pointer
                                           null-pointer?
                                           pointer->string
                                           make-pointer
                                           dereference-pointer))
  #:use-module ((system foreign) #:prefix foreign:)
  #:use-module ((bytestructures guile) #:select (bs:pointer
                                                 bs:struct
                                                 bs:vector
                                                 bytestructure-ref))
  #:use-module ((bytestructures guile) #:prefix bs:)
  #:use-module (tissue utils)
  #:export (git-top-level
            current-git-repository
            commit-date
            git-tracked-files
            file-modification-table))

;; We bind additional functions from libgit2 that are not already
;; bound in guile-git. TODO: Contribute them to guile-git.

(define pointer->bytestructure
  (@@ (git structs) pointer->bytestructure))

(define bytestructure->pointer
  (@@ (git structs) bytestructure->pointer))

(define %oid
  (bs:vector 20 bs:uint8))

(define %index-time
  (bs:struct `((seconds ,bs:int32)
               (nanoseconds ,bs:uint32))))

(define %index-entry
  (bs:struct `((ctime ,%index-time)
               (mtime ,%index-time)
               (dev ,bs:uint32)
               (ino ,bs:uint32)
               (mode ,bs:uint32)
               (uid ,bs:uint32)
               (gid ,bs:uint32)
               (file-size ,bs:uint32)
               (id ,%oid)
               (flags ,bs:uint16)
               (flags-extended ,bs:uint16)
               (path ,(bs:pointer bs:uint8)))))

(define-record-type <index-time>
  (%make-index-time seconds nanoseconds)
  index-time?
  (seconds index-time-seconds)
  (nanoseconds index-time-nanoseconds))

(define (pointer->index-time pointer)
  (if (null-pointer? pointer)
      #f
      (let ((bs (pointer->bytestructure pointer %index-time)))
        (%make-index-time
         (bytestructure-ref bs 'seconds)
         (bytestructure-ref bs 'nanoseconds)))))

(define-record-type <index-entry>
  (%make-index-entry ctime mtime dev ino mode uid gid file-size id flags path)
  index-entry?
  (ctime index-entry-ctime)
  (mtime index-entry-mtime)
  (dev index-entry-dev)
  (ino index-entry-ino)
  (mode index-entry-mode)
  (uid index-entry-uid)
  (gid index-entry-gid)
  (file-size index-entry-file-size)
  (id index-entry-id)
  (flags index-entry-flags)
  (path index-entry-path))

(define (pointer->index-entry pointer)
  (if (null-pointer? pointer)
      #f
      (let* ((bs (pointer->bytestructure pointer %index-entry))
             (flags (bytestructure-ref bs 'flags)))
        (%make-index-entry
         (pointer->index-time
          (bytestructure->pointer (bytestructure-ref bs 'ctime)))
         (pointer->index-time
          (bytestructure->pointer (bytestructure-ref bs 'mtime)))
         (bytestructure-ref bs 'dev)
         (bytestructure-ref bs 'ino)
         (bytestructure-ref bs 'mode)
         (bytestructure-ref bs 'uid)
         (bytestructure-ref bs 'gid)
         (bytestructure-ref bs 'file-size)
         (pointer->oid
          (bytestructure->pointer (bytestructure-ref bs 'id)))
         (if (zero? (logand flags
                            (bitwise-arithmetic-shift 1 15)))
             (list)
             (list 'assume-valid))
         (pointer->string
          (make-pointer (bytestructure-ref bs 'path))
          (bitwise-and flags (bitwise-not (bitwise-arithmetic-shift 1 13))))))))

;; The repository-index function in guile-git has a bug. See
;; https://gitlab.com/guile-git/guile-git/-/merge_requests/34 . To
;; work around this, we redefine it here.
(define repository-index
  (let ((proc (libgit2->procedure* "git_repository_index" (list '* '*))))
    (lambda (repository)
      (let ((out (make-double-pointer)))
        (proc out (repository->pointer repository))
        (pointer->index (dereference-pointer out))))))

(define index-version
  (let ((proc (libgit2->procedure foreign:unsigned-int "git_index_version" (list '*))))
    (lambda (index)
      (proc (index->pointer index)))))

(define index-entry-count
  (compose (libgit2->procedure foreign:size_t "git_index_entrycount" (list '*))
           index->pointer))

(define index-entry
  (let ((proc (libgit2->procedure '* "git_index_get_byindex" (list '* foreign:size_t))))
    (lambda (index n)
      (unless (= (index-version index) 2)
        (error "Unsupported git index version:" (index-version index)))
      (pointer->index-entry (proc (index->pointer index) n)))))

(define (index-entries index)
  "Return the list of all entries in INDEX, a <git-index> object. The
return value is a list of <index-entry> objects."
  (map (lambda (i)
         (index-entry index i))
       (iota (index-entry-count index))))

(define diff-find-similar!
  (let ((proc (libgit2->procedure* "git_diff_find_similar" '(* *))))
    (lambda (diff)
      (proc (diff->pointer diff) %null-pointer)
      diff)))

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

(define (current-git-repository)
  "Return the current git repository."
  (repository-open (git-top-level)))

(define (commit-date commit)
  (time-monotonic->date
   (make-time time-monotonic
              0
              (commit-time commit))
   (* 60 (commit-time-offset commit))))

(define* (git-tracked-files #:optional (repository (current-git-repository)))
  "Return a list of all files tracked in REPOSITORY. The returned
filenames are relative to the top-level directory of REPOSITORY and do
not have a leading slash."
  (map index-entry-path
       (index-entries (repository-index repository))))

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
