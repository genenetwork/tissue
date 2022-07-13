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
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-171)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (oop goops)
  #:use-module (term ansi-color)
  #:use-module (git)
  #:use-module (web uri)
  #:use-module (xapian xapian)
  #:use-module (tissue document)
  #:use-module (tissue file-document)
  #:use-module (tissue git)
  #:use-module (tissue person)
  #:use-module (tissue utils)
  #:export (<issue>
            issue-assigned
            issue-keywords
            issue-open?
            issue-tasks
            issue-completed-tasks
            issue->alist
            alist->issue
            post->alist
            alist->post
            print-issue
            print-issue-to-gemtext
            issues
            read-gemtext-issue
            index-issue))

(define-class <issue> (<file-document>)
  (assigned #:accessor issue-assigned #:init-keyword #:assigned)
  (keywords #:accessor issue-keywords #:init-keyword #:keywords)
  (open? #:accessor issue-open? #:init-keyword #:open?)
  (tasks #:accessor issue-tasks #:init-keyword #:tasks)
  (completed-tasks #:accessor issue-completed-tasks #:init-keyword #:completed-tasks))

(define-method (document-boolean-terms (issue <issue>))
  "Return the boolean terms in ISSUE."
  (append (list (string-append "A" (file-document-creator issue))
                (string-append "XA" (file-document-last-updater issue))
                (string-append "XS" (if (issue-open? issue)
                                        "open" "closed")))
          (map (cut string-append "XI" <>)
               (issue-assigned issue))
          (map (cut string-append "K" <>)
               (issue-keywords issue))
          (next-method)))

(define-method (print (issue <issue>) mset port)
  "Print ISSUE, an <issue> object, in search results."
  (let ((number-of-posts (length (file-document-commits issue))))
    (display (colorize-string (document-title issue) 'MAGENTA 'UNDERLINE)
             port)
    (unless (null? (issue-keywords issue))
      (display " " port)
      (display (string-join (map (cut colorize-string <> 'ON-BLUE)
                                 (issue-keywords issue))
                            " ")
               port))
    (unless (null? (issue-assigned issue))
      (display (colorize-string (string-append " (assigned: "
                                               (string-join (issue-assigned issue)
                                                            ", ")
                                               ")")
                                'GREEN)
               port))
    (when (> number-of-posts 1)
      (display (string-append " ["
                              (number->string number-of-posts)
                              " posts]")
               port))
    (newline port)
    (display (if (issue-open? issue)
                 (colorize-string "ISSUE" 'BOLD 'YELLOW)
                 (colorize-string "✓ ISSUE" 'BOLD 'GREEN))
             port)
    (display " " port)
    (display (colorize-string (file-document-path issue) 'YELLOW)
             port)
    (newline port)
    (display (string-append
              "opened "
              (colorize-string (human-date-string (file-document-created-date issue)) 'CYAN)
              " by "
              (colorize-string (file-document-creator issue) 'CYAN))
             port)
    (when (> number-of-posts 1)
      (display (string-append (colorize-string "," 'CYAN)
                              " last updated "
                              (colorize-string (human-date-string (file-document-last-updated-date issue))
                                               'CYAN)
                              " by "
                              (colorize-string (file-document-last-updater issue)
                                               'CYAN))
               port))
    (unless (zero? (issue-tasks issue))
      (display (string-append (colorize-string "; " 'CYAN)
                              (number->string (issue-completed-tasks issue))
                              "/"
                              (number->string (issue-tasks issue))
                              " tasks done")
               port))
    (newline port)
    (let ((snippet (document-snippet issue mset)))
      (unless (string-null? snippet)
        (display snippet port)
        (newline port)
        (newline port)))))

(define (print-issue-to-gemtext issue)
  "Print ISSUE to gemtext."
  (let ((number-of-posts (length (file-document-commits issue))))
    (format #t "# ~a" (document-title issue))
    (unless (null? (issue-keywords issue))
      (format #t " [~a]"
              (string-join (issue-keywords issue)
                           ", ")))
    (unless (null? (issue-assigned issue))
      (format #t " (assigned: ~a)"
              (string-join (issue-assigned issue)
                           ", ")))
    (when (> number-of-posts 1)
      (format #t " [~a posts]" number-of-posts))
    (newline)
    (format #t "opened ~a by ~a"
            (human-date-string (file-document-created-date issue))
            (file-document-creator issue))
    (when (> number-of-posts 1)
      (format #t ", last updated ~a by ~a"
              (human-date-string (file-document-last-updated-date issue))
              (file-document-last-updater issue)))
    (unless (zero? (issue-tasks issue))
      (format #t "; ~a/~a tasks done"
              (issue-completed-tasks issue)
              (issue-tasks issue)))
    (newline)
    (newline)))

(define (sanitize-string str)
  "Downcase STR and replace spaces with hyphens."
  (string-map (lambda (c)
                (case c
                  ((#\space) #\-)
                  (else c)))
              (string-downcase str)))

(define-method (document->sxml (issue <issue>) mset)
  "Render ISSUE, an <issue> object, to SXML. MSET is the xapian MSet
object representing a list of search results."
  `(li (@ (class ,(string-append "search-result search-result-issue "
                                 (if (issue-open? issue)
                                     "search-result-open-issue"
                                     "search-result-closed-issue"))))
       (a (@ (href ,(document-web-uri issue))
             (class "search-result-title"))
          ,(document-title issue))
       ,@(map (lambda (tag)
                (let ((words (string-split tag (char-set #\- #\space))))
                  `(a (@ (href ,(string-append
                                 "/search?query="
                                 (uri-encode
                                  ;; Hyphenate tag if it has
                                  ;; spaces. Xapian accepts hyphenated
                                  ;; strings as exact phrases.
                                  (string-append "tag:" (sanitize-string tag)))))
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
              (issue-keywords issue))
       (div (@ (class "search-result-metadata"))
            ,(string-append
              (format #f "opened ~a by ~a"
                      (human-date-string (file-document-created-date issue))
                      (file-document-creator issue))
              (if (> (length (file-document-commits issue))
                     1)
                  (format #f ", last updated ~a by ~a"
                          (human-date-string (file-document-last-updated-date issue))
                          (file-document-last-updater issue))
                  "")
              (if (zero? (issue-tasks issue))
                  ""
                  (format #f "; ~a of ~a tasks done"
                          (issue-completed-tasks issue)
                          (issue-tasks issue)))))
       ,@(let ((snippet (document-sxml-snippet issue mset)))
           (if snippet
               (list `(div (@ (class "search-result-snippet"))
                           ,@snippet))
               (list)))))

(define (hashtable-prepend! hashtable key new-values)
  "Prepend NEW-VALUES to the list of values KEY is associated to in
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
                ;; Begin new key.
                ((string-match "^([a-zA-Z]+):[ ]*(.*)" element)
                 => (lambda (m)
                      (cons (list (string->symbol
                                   (string-downcase (match:substring m 1)))
                                  (match:substring m 2))
                            result)))
                ;; Add to current key.
                (else
                 (match result
                   (((key . values) tail ...)
                    (cons (cons key (cons element values))
                          tail))))))
             '()
             (comma-split (string-remove-prefix "* " line)))))

(define (file-details port)
  "Return a hashtable of details extracted from input PORT reading a
gemtext file."
  (let ((result (make-eq-hashtable)))
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
                             ((list-line->alist line)
                              => (lambda (alist)
                                   ;; Insert values based on
                                   ;; their keys.
                                   (for-each (match-lambda
                                               (((or 'assign 'assigned) . values)
                                                (hashtable-prepend! result 'assigned
                                                                    (map (cut resolve-alias <> (%aliases))
                                                                         values)))
                                               (((or 'keyword 'keywords 'severity 'status
                                                     'priority 'tag 'tags 'type)
                                                 . values)
                                                (hashtable-prepend! result 'keywords values))
                                               (_ #t))
                                             alist)))
                             ;; A more fuzzy heuristic to find keywords
                             ((and (string-prefix? "* " line)
				   ;; Is every comma-separated
				   ;; element two words utmost?
				   (every (cut <=n-words? <> 2)
					  (comma-split (string-remove-prefix "* " line)))
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
					(comma-split (string-remove-prefix "* " line))))
                              (hashtable-prepend! result 'keywords
						  (comma-split
						   (string-remove-prefix "* " line))))
                             ;; The first level one heading is the
                             ;; title.
                             ((string-prefix? "# " line)
                              (unless (hashtable-contains? result 'title)
				(hashtable-set! result 'title
						(string-remove-prefix "# " line)))))))
                    (const #t)
                    get-line-dos-or-unix
                    port)
    result))

(define (read-gemtext-issue file)
  "Read issue from gemtext FILE. Return an <issue> object."
  (let* ((file-document (read-gemtext-document file))
         (file-details (call-with-file-in-git (current-git-repository) file
                         file-details))
         ;; Downcase keywords to make them
         ;; case-insensitive.
         (all-keywords (map string-downcase
                            (hashtable-ref file-details 'keywords '()))))
    (make <issue>
      #:path file
      ;; Fallback to filename if title has no alphabetic characters.
      #:title (let ((title (hashtable-ref file-details 'title "")))
                (if (string-any char-set:letter title) title file))
      #:assigned (hashtable-ref file-details 'assigned '())
      ;; "closed" is a special keyword to indicate the open/closed
      ;; status of an issue.
      #:keywords (delete "closed" all-keywords)
      #:open? (not (member "closed" all-keywords))
      #:tasks (hashtable-ref file-details 'tasks 0)
      #:completed-tasks (hashtable-ref file-details 'completed-tasks 0)
      #:commits (file-document-commits file-document))))
