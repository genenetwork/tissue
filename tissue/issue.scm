;;; tissue --- Text based issue tracker
;;; Copyright © 2022 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2022 Frederick Muriuki Muriithi <fredmanglis@gmail.com>
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

(define-module (tissue issue)
  #:use-module (rnrs hashtables)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-171)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (tissue utils)
  #:export (issue
            issue-file
            issue-title
            issue-creator
            issue-created-date
            issue-created-relative-date
            issue-last-updater
            issue-last-updated-date
            issue-last-updated-relative-date
            issue-assigned
            issue-keywords
            issue-open?
            issue-tasks
            issue-completed-tasks
            issue-posts
            issues))

(define-record-type <issue>
  (issue file title creator created-date created-relative-date
         last-updater last-updated-date last-updated-relative-date
         assigned keywords open tasks completed-tasks posts)
  issue?
  (file issue-file)
  (title issue-title)
  (creator issue-creator)
  (created-date issue-created-date)
  (created-relative-date issue-created-relative-date)
  (last-updater issue-last-updater)
  (last-updated-date issue-last-updated-date)
  (last-updated-relative-date issue-last-updated-relative-date)
  (assigned issue-assigned)
  (keywords issue-keywords)
  (open issue-open?)
  (tasks issue-tasks)
  (completed-tasks issue-completed-tasks)
  (posts issue-posts))

(define (hashtable-append! hashtable key new-values)
  "Append NEW-VALUES to the list of values KEY is associated to in
HASHTABLE. Deduplicate the resulting list if necessary. If KEY is not
associated to any value in HASHTABLE, assume it is associated to the
empty list."
  (hashtable-update! hashtable
                     key
                     (cut apply lset-adjoin equal? <> new-values)
                     '()))

(define (comma-split str)
  "Split string at commas, trim whitespace from both ends of the split
strings, and return them as a list."
  (map (cut string-trim-both <>)
       (string-split str #\,)))

(define (remove-prefix prefix str)
  "Remove PREFIX from STR."
  (substring str (string-length prefix)))

(define (<=n-words? str n)
  "Return #t if STR has N words or less. Else, return #f."
  (<= (length (string-split str #\space))
      n))

(define (list-line->alist line)
  "Split list LINE such as \"assigned: foo, keywords: fubar, bar\"
into an association list of key-value pairs. Keys are symbols. Values
are lists of strings. If LINE does not contain such key-value pairs,
return #f."
  (and (string-match "^\\* [a-zA-Z]+:" line)
       (fold (lambda (element result)
               (cond
                ((string-match "^([a-zA-Z]+):[ ]*(.*)" element)
                 => (lambda (m)
                      (cons (list (string->symbol
                                   (string-downcase (match:substring m 1)))
                                  (match:substring m 2))
                            result)))
                (else
                 (match result
                   (((key . values) tail ...)
                    (cons (cons key (cons element values))
                          tail))))))
             '()
             (comma-split (remove-prefix "* " line)))))

(define (unix-time->date timestamp)
  "Convert unix TIMESTAMP to an SRFI-19 date object."
  (time-monotonic->date
   (make-time time-monotonic 0 timestamp)))

(define (file-details file)
  "Return a hashtable of details extracted from gemini FILE."
  (let ((result (make-eq-hashtable)))
    ;; Files may be renamed or deleted, but not committed. Therefore,
    ;; only read the file if it exists.
    (when (file-exists? file)
      (call-with-input-file file
	(lambda (port)
          (port-transduce (tmap (lambda (line)
                                  (cond
                                   ;; Checkbox lists are tasks. If the
                                   ;; checkbox has any character other
                                   ;; than space in it, the task is
                                   ;; completed.
                                   ((string-match "^\\* \\[(.)\\]" line)
                                    => (lambda (m)
					 (hashtable-update! result 'tasks 1+ 0)
					 (unless (string=? (match:substring m 1) " ")
                                           (hashtable-update! result 'completed-tasks 1+ 0))))
                                   ((let ((alist (list-line->alist line)))
                                      (and alist
                                           ;; Every value string is 2
                                           ;; words or less.
                                           (every (match-lambda
                                                    ((_ . values)
                                                     (every (cut <=n-words? <> 2)
                                                            values)))
                                                  alist)
                                           alist))
                                    => (lambda (alist)
                                         ;; Insert values based on
                                         ;; their keys.
                                         (for-each (match-lambda
                                                     (((or 'assign 'assigned) . values)
                                                      (hashtable-append! result 'assigned values))
                                                     (((or 'keywords 'severity 'status 'priority 'tags 'type) . values)
                                                      (hashtable-append! result 'keywords values))
                                                     (_ #t))
                                                   alist)))
                                   ;; A more fuzzy heuristic to find keywords
                                   ((and (string-prefix? "* " line)
					 ;; Is every comma-separated
					 ;; element two words utmost?
					 (every (cut <=n-words? <> 2)
						(comma-split (remove-prefix "* " line)))
					 ;; Does any comma-separated
					 ;; element contain a potential
					 ;; keyword?
					 (any (lambda (element)
						(any (lambda (keyword)
						       (string-contains element keyword))
						     (list "request" "bug" "critical"
                                                           "enhancement" "progress"
                                                           "testing" "later" "documentation"
                                                           "help" "closed")))
					      (comma-split (remove-prefix "* " line))))
                                    (hashtable-append! result 'keywords
						       (comma-split
							(remove-prefix "* " line))))
                                   ;; The first level one heading is the
                                   ;; title.
                                   ((string-prefix? "# " line)
                                    (unless (hashtable-contains? result 'title)
				      (hashtable-set! result 'title
						      (remove-prefix "# " line)))))))
                          (const #t)
                          get-line-dos-or-unix
                          port))))
    (call-with-input-pipe
     (lambda (port)
       (hashtable-set!
        result 'posts
        (port-transduce
         (compose (tenumerate)
                  (tmap (match-lambda
                          ((index . line)
                           (let ((alist (call-with-input-string line read)))
                             (when (zero? index)
                               (hashtable-set! result 'last-updater
                                               (assq-ref alist 'author))
                               (hashtable-set! result 'last-updated-date
                                               (unix-time->date (assq-ref alist 'author-date)))
                               (hashtable-set! result 'last-updated-relative-date
                                               (assq-ref alist 'author-relative-date)))
                             (hashtable-set! result 'creator
                                             (assq-ref alist 'author))
                             (hashtable-set! result 'created-date
                                             (unix-time->date (assq-ref alist 'author-date)))
                             (hashtable-set! result 'created-relative-date
                                             (assq-ref alist 'author-relative-date)))))))
         rcount get-line port)))
     "git" "log"
     (string-append "--format=format:("
                    "(author . \"%an\")"
                    "(author-date . %at)"
                    "(author-relative-date . \"%ar\")"
                    ")")
     "--" file)
    result))

(define (memoize-thunk thunk)
  "Return a function memoizing THUNK."
  (let ((result #f))
    (lambda ()
      (unless result
        (set! result (thunk)))
      result)))

(define issues
  (memoize-thunk
   (lambda ()
     "Return a list of all issues, sorted oldest first."
     ;; Get all gemini files except README.gmi and hidden files. Text
     ;; editors tend to create hidden files while editing, and we want to
     ;; avoid them.
     (sort (call-with-input-pipe
            (lambda (port)
              (port-transduce
               (tfilter-map (lambda (file)
                              (and (string-suffix? ".gmi" file)
                                   (not (string=? (basename file) "README.gmi"))
                                   (not (string-prefix? "." (basename file)))
                                   (let* ((file-details (file-details file))
                                          ;; Downcase keywords to make
                                          ;; them case-insensitive.
                                          (all-keywords (map string-downcase
                                                             (hashtable-ref file-details 'keywords '()))))
                                     (issue file
                                            ;; Fallback to filename if title has no alphabetic
                                            ;; characters.
                                            (let ((title (hashtable-ref file-details 'title "")))
                                              (if (string-any char-set:letter title) title file))
                                            (hashtable-ref file-details 'creator #f)
                                            (hashtable-ref file-details 'created-date #f)
                                            (hashtable-ref file-details 'created-relative-date #f)
                                            (hashtable-ref file-details 'last-updater #f)
                                            (hashtable-ref file-details 'last-updated-date #f)
                                            (hashtable-ref file-details 'last-updated-relative-date #f)
                                            (hashtable-ref file-details 'assigned '())
                                            ;; "closed" is a special keyword to indicate
                                            ;; the open/closed status of an issue.
                                            (delete "closed" all-keywords)
                                            (not (member "closed" all-keywords))
                                            (hashtable-ref file-details 'tasks 0)
                                            (hashtable-ref file-details 'completed-tasks 0)
                                            (hashtable-ref file-details 'posts #f))))))
               rcons get-line port))
            "git" "ls-files")
           (lambda (issue1 issue2)
             (time<? (date->time-monotonic (issue-created-date issue1))
                     (date->time-monotonic (issue-created-date issue2))))))))
