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
  #:use-module (git)
  #:use-module (tissue git)
  #:use-module (tissue utils)
  #:export (%issue-files
            %aliases
            issue
            issue-file
            issue-title
            issue-creator
            issue-created-date
            issue-last-updater
            issue-last-updated-date
            issue-assigned
            issue-keywords
            issue-open?
            issue-tasks
            issue-completed-tasks
            issue-posts
            post
            post-author
            post-date
            issue->alist
            alist->issue
            post->alist
            alist->post
            authors
            issues))

(define %issue-files
  (make-parameter #f))

(define %aliases
  (make-parameter #f))

(define-record-type <issue>
  (issue file title creator created-date last-updater last-updated-date
         assigned keywords open tasks completed-tasks posts)
  issue?
  (file issue-file)
  (title issue-title)
  (creator issue-creator)
  (created-date issue-created-date)
  (last-updater issue-last-updater)
  (last-updated-date issue-last-updated-date)
  (assigned issue-assigned)
  (keywords issue-keywords)
  (open issue-open?)
  (tasks issue-tasks)
  (completed-tasks issue-completed-tasks)
  ;; List of <post> objects, oldest first.
  (posts issue-posts))

(define-record-type <post>
  (post author date)
  post?
  (author post-author)
  (date post-date))

(define (date->iso-8601 date)
  "Convert DATE, an SRFI-19 date object, to an ISO-8601 date string."
  (date->string date "~4"))

(define (iso-8601->date str)
  "Convert STR, an ISO-8601 date string, to an SRFI-19 date object."
  (string->date str "~Y-~m-~dT~H:~M:~S~z"))

(define (issue->alist issue)
  "Convert ISSUE, a <issue> object, to an association list that can be
serialized."
  `((file . ,(issue-file issue))
    (title . ,(issue-title issue))
    (creator . ,(issue-creator issue))
    (created-date . ,(date->iso-8601 (issue-created-date issue)))
    (last-updater . ,(issue-last-updater issue))
    (last-updated-date . ,(date->iso-8601 (issue-last-updated-date issue)))
    (assigned . ,(issue-assigned issue))
    (keywords . ,(issue-keywords issue))
    (open . ,(issue-open? issue))
    (tasks . ,(issue-tasks issue))
    (completed-tasks . , (issue-completed-tasks issue))
    (posts . ,(map post->alist (issue-posts issue)))))

(define (post->alist post)
  "Convert POST, a <post> object, to an association list that can be
serialized."
  `((author . ,(post-author post))
    (date . ,(date->iso-8601 (post-date post)))))

(define (alist->issue alist)
  "Convert ALIST to an <issue> object."
  (issue (assq-ref alist 'file)
         (assq-ref alist 'title)
         (assq-ref alist 'creator)
         (iso-8601->date (assq-ref alist 'created-date))
         (assq-ref alist 'last-updater)
         (iso-8601->date (assq-ref alist 'last-updated-date))
         (assq-ref alist 'assigned)
         (assq-ref alist 'keywords)
         (assq-ref alist 'open)
         (assq-ref alist 'tasks)
         (assq-ref alist 'completed-tasks)
         (map alist->post
              (assq-ref alist 'posts))))

(define (alist->post alist)
  "Convert ALIST to a <post> object."
  (post (assq-ref alist 'author)
        (iso-8601->date (assq-ref alist 'date))))

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

(define authors
  (memoize-thunk
   (lambda ()
     "Return a list of all authors who have committed to this git
repository. The returned list is sorted in lexicographic order."
     (sort (delete-duplicates
            (append-map (lambda (issue)
                          (map post-author (issue-posts issue)))
                        (issues)))
           string<?))))

(define (resolve-alias name aliases)
  "Resolve NAME against ALIASES, a list of aliases. ALIASES should be
in the form of the argument of the same name to `tissue-configuration'
in (tissue tissue). If no alias is found, NAME is returned as such."
  (cond
   ((find (cut member name <>)
          aliases)
    => (match-lambda
         ((canonical-name _ ...) canonical-name)
         (() name)))
   (else name)))

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
                                                      (hashtable-append! result 'assigned
                                                                         (map (cut resolve-alias <> (%aliases))
                                                                              values)))
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
    result))

(define issues
  (memoize-thunk
   (lambda ()
     "Return a list of all issues, sorted oldest first."
     ;; Get all gemini files except README.gmi and hidden files. Text
     ;; editors tend to create hidden files while editing, and we want to
     ;; avoid them.
     (let ((file-modification-table (file-modification-table (current-git-repository))))
       (sort (filter-map (lambda (file)
                           (let* ((file-details (file-details file))
                                  ;; Downcase keywords to make them
                                  ;; case-insensitive.
                                  (all-keywords (map string-downcase
                                                     (hashtable-ref file-details 'keywords '())))
                                  (commits (hashtable-ref file-modification-table file #f))
                                  (commit-authors (map (lambda (commit)
                                                         (resolve-alias (signature-name (commit-author commit))
                                                                        (%aliases)))
                                                       commits)))
                             (issue file
                                    ;; Fallback to filename if title has no alphabetic
                                    ;; characters.
                                    (let ((title (hashtable-ref file-details 'title "")))
                                      (if (string-any char-set:letter title) title file))
                                    (first commit-authors)
                                    (commit-date (first commits))
                                    (last commit-authors)
                                    (commit-date (last commits))
                                    (hashtable-ref file-details 'assigned '())
                                    ;; "closed" is a special keyword to indicate
                                    ;; the open/closed status of an issue.
                                    (delete "closed" all-keywords)
                                    (not (member "closed" all-keywords))
                                    (hashtable-ref file-details 'tasks 0)
                                    (hashtable-ref file-details 'completed-tasks 0)
                                    (map (lambda (commit author)
                                           (post author (commit-date commit)))
                                         commits
                                         commit-authors))))
                         (%issue-files))
             (lambda (issue1 issue2)
               (time<? (date->time-monotonic (issue-created-date issue1))
                       (date->time-monotonic (issue-created-date issue2)))))))))
