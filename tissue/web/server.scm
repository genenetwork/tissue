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

(define-module (tissue web server)
  #:use-module (rnrs conditions)
  #:use-module (rnrs exceptions)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-171)
  #:use-module (ice-9 filesystem)
  #:use-module (ice-9 match)
  #:use-module (htmlprag)
  #:use-module (sxml simple)
  #:use-module ((system repl server) #:select (make-unix-domain-server-socket))
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web server)
  #:use-module (web uri)
  #:use-module (git)
  #:use-module (xapian wrap)
  #:use-module ((xapian xapian) #:renamer (lambda (symbol)
                                            (case symbol
                                              ((parse-query) 'xapian:parse-query)
                                              (else symbol))))
  #:use-module (tissue document)
  #:use-module (tissue git)
  #:use-module (tissue search)
  #:use-module (tissue utils)
  #:export (start-web-server))

(define %css
  "
body {
    max-width: 1000px;
    margin: 0 auto;
}

form { text-align: center; }
.search-hint { line-height: 2em; }
.search-filter {
    background-color: gray;
    color: white;
    padding: 0 0.2em;
}

.search-results-statistics {
    list-style: none;
    padding: 0;
}
.search-results-statistics li {
    display: inline;
    margin: 0.5em;
}
.search-results-statistics a { color: blue; }
.current-search-type { font-weight: bold; }

.search-results { padding: 0; }
.search-result {
    list-style-type: none;
    padding: 0.5em;
}
.search-result a {
    text-decoration: none;
    font-size: larger;
}
.document-type {
    font-size: xx-small;
    font-weight: bold;
    padding: 0 0.2em;
    background-color: darkmagenta;
    color: white;
    text-transform: uppercase;
}
.open-issue-document-type { background-color: green; }
.search-result-metadata {
    color: dimgray;
    font-size: smaller;
}
.search-result-snippet { font-size: smaller; }

.tags {
    list-style-type: none;
    padding: 0;
    display: inline;
}
.tag { display: inline; }
.tag a {
    padding: 0 0.2em;
    color: white;
    background-color: blue;
    margin: auto 0.25em;
    font-size: smaller;
}
.tag-bug a { background-color: red; }
.tag-feature a { background-color: green; }
.tag-progress a, .tag-unassigned a {
    background-color: orange;
    color: black;
}
.tag-chore a {
    background-color: khaki;
    color: black;
}")

(define* (make-search-page results query css
                           #:key
                           page-uri-path page-uri-parameters
                           matches
                           matched-open-issues matched-closed-issues
                           matched-documents matched-commits
                           current-search-type)
  "Return SXML for a page with search RESULTS produced for QUERY.

CSS is a URI to a stylesheet. PAGE-URI-PATH is the path part of the
URI to the page. PAGE-URI-PARAMETERS is an association list of
parameters in the query string of the URI of the page.

MATCHES is the number of matches. MATCHED-OPEN-ISSUES,
MATCHED-CLOSED-ISSUES, MATCHED-DOCUMENTS and MATCHED-COMMITS are
respectively the number of open issues, closed issues, documents and
commits matching the current query. CURRENT-SEARCH-TYPE is the type of
document search results are being showed for."
  `(html
    (head
     (title "Tissue search")
     (style ,%css)
     ,@(if css
           (list `(link (@ (href "/style.css")
                           (rel "stylesheet")
                           (type "text/css"))))
           (list)))
    (body
     (form (@ (action "/search") (method "GET"))
           (input (@ (type "text")
                     (name "query")
                     (value ,query)
                     (placeholder "Enter search query")))
           (input (@ (type "hidden")
                     (name "type")
                     (value ,(symbol->string current-search-type))))
           (input (@ (type "submit") (value "Search"))))
     (details (@ (class "search-hint"))
              (summary "Hint")
              (p "Refine your search with filters "
                 ,@(append-map (lambda (filter)
                                 (list `(span (@ (class "search-filter"))
                                              ,filter)
                                       ", "))
                               (list "type:issue"
                                     "type:document"
                                     "is:open"
                                     "is:closed"
                                     "title:git"
                                     "creator:mani"
                                     "lastupdater:vel"
                                     "assigned:muthu"
                                     "tag:feature-request"))
                 "etc. Optionally, combine search terms with boolean
operators "
                 (span (@ (class "search-filter"))
                       "AND")
                 " and "
                 (span (@ (class "search-filter"))
                       "OR")
                 ". See " (a (@ (href "https://xapian.org/docs/queryparser.html"))
                             "Xapian::QueryParser Syntax")
                 " for detailed documentation."))
     ,(let ((search-result-statistic
             (lambda (search-type format-string matches)
               `(li (a (@ (href ,(string-append
                                  page-uri-path
                                  "?"
                                  (query-string
                                   (acons "type" (symbol->string search-type)
                                          (alist-delete "type" page-uri-parameters)))))
                          ,@(if (eq? search-type current-search-type)
                                '((class "current-search-type"))
                                '()))
                       ,(format #f format-string matches))))))
        `(ul (@ (class "search-results-statistics"))
             ,(search-result-statistic 'all "~a All" matches)
             ,(search-result-statistic 'open-issue "~a open issues" matched-open-issues)
             ,(search-result-statistic 'closed-issue "~a closed issues" matched-closed-issues)
             ,(search-result-statistic 'document "~a documents" matched-documents)
             ,(search-result-statistic 'commit "~a commits" matched-commits)))
     (ul (@ (class "search-results"))
         ,@results))))

(define (query-parameters query)
  "Return an association list of query parameters in web QUERY string."
  (if query
      (map (lambda (parameter)
             (match (string-split parameter #\=)
               ((key value)
                (cons (uri-decode key)
                      (uri-decode value)))))
           (string-split query #\&))
      '()))

(define (query-string parameters)
  "Return a query string for association list of PARAMETERS."
  (string-join
   (map (match-lambda
          ((key . value)
           (string-append (uri-encode key)
                          "="
                          (uri-encode value))))
        parameters)
   "&"))

(define %mime-types
  '(("gif" image/gif)
    ("html" text/html)
    ("jpeg" image/jpeg)
    ("jpg" image/jpeg)
    ("js" text/javascript)
    ("json" application/json)
    ("png" image/png)
    ("pdf" application/pdf)
    ("svg" image/svg+xml)
    ("txt" text/plain)))

(define (matches db query filter)
  "Return the number of matches in DB for QUERY filtering with FILTER
query. QUERY and FILTER are Xapian Query objects."
  (MSet-get-matches-estimated
   (enquire-mset
    (enquire
     db (new-Query (Query-OP-FILTER) query filter))
    #:maximum-items (database-document-count db))))

(define (handler request body hosts state-directory)
  "Handle web REQUEST with BODY and return two values---the response
headers and body.

See `start-web-server' for documentation of HOSTS and
STATE-DIRECTORY."
  (let* ((path (uri-path (request-uri request)))
         (parameters (query-parameters (uri-query (request-uri request))))
         (hostname (match (assq-ref (request-headers request) 'host)
                     ((hostname . _) hostname)))
         (host-parameters (or (assoc-ref hosts hostname)
                              (raise (condition
                                      (make-message-condition "Unknown host")
                                      (make-irritants-condition hostname))))))
    (format #t "~a ~a\n"
            (request-method request)
            path)
    (parameterize ((%current-git-repository
                    (repository-open
                     (string-append state-directory "/" hostname "/repository"))))
      (cond
       ;; Search page
       ((member path (list "/" "/search"))
        (let* ((search-query (or (assoc-ref parameters "query")
                                 ""))
               (search-type (match (assoc-ref parameters "type")
                              ((or "open-issue" "closed-issue" "commit" "document")
                               (string->symbol (assoc-ref parameters "type")))
                              (_ 'all)))
               (filter-alist `((open-issue . ,(parse-query "type:issue AND is:open"))
                               (closed-issue . ,(parse-query "type:issue AND is:closed"))
                               (commit . ,(parse-query "type:commit"))
                               (document . ,(parse-query "type:document")))))
          (values '((content-type . (text/html)))
                  (sxml->html
                   (call-with-database (string-append state-directory "/" hostname "/xapian")
                     (lambda (db)
                       (let* ((query (parse-query search-query))
                              (mset (enquire-mset
                                     (let* ((query (new-Query (Query-OP-FILTER)
                                                              query
                                                              (or (assq-ref filter-alist search-type)
                                                                  (Query-MatchAll))))
                                            (enquire (enquire db query)))
                                       ;; Sort by recency date (slot
                                       ;; 0) when query is strictly
                                       ;; boolean.
                                       (when (boolean-query? query)
                                         (Enquire-set-sort-by-value enquire 0 #t))
                                       enquire)
                                     #:offset 0
                                     #:maximum-items (database-document-count db))))
                         (make-search-page
                          (reverse
                           (mset-fold (lambda (item result)
                                        (cons (document->sxml
                                               (call-with-input-string (document-data (mset-item-document item))
                                                 (compose scm->object read))
                                               mset)
                                              result))
                                      '()
                                      mset))
                          search-query
                          (assq-ref host-parameters 'css)
                          #:page-uri-path path
                          #:page-uri-parameters parameters
                          #:matches (matches db query (Query-MatchAll))
                          #:matched-open-issues (matches db query (assq-ref filter-alist 'open-issue))
                          #:matched-closed-issues (matches db query (assq-ref filter-alist 'closed-issue))
                          #:matched-documents (matches db query (assq-ref filter-alist 'document))
                          #:matched-commits (matches db query (assq-ref filter-alist 'commit))
                          #:current-search-type search-type))))))))
       ;; Static files
       ((let ((file-path
               (find file-exists?
                     ;; Try path and path.html.
                     (list (string-append state-directory "/" hostname "/website" path)
                           (string-append state-directory "/" hostname "/website" path ".html")))))
          (and file-path
               ;; Check that the file really is within the document
               ;; root.
               (string-prefix? (string-append state-directory "/" hostname "/website/")
                               (canonicalize-path file-path))
               (canonicalize-path file-path)))
        => (lambda (file-path)
             (values `((content-type . ,(or (assoc-ref %mime-types (string-remove-prefix
                                                                    "." (file-name-extension file-path)))
                                            '(application/octet-stream))))
                     (call-with-input-file file-path
                       get-bytevector-all))))
       ;; Not found
       (else
        (values (build-response #:code 404)
                (string-append "Resource not found: "
                               (uri->string (request-uri request)))))))))

(define (start-web-server socket-address hosts state-directory)
  "Start web server listening on SOCKET-ADDRESS.

HOSTS is an association list mapping host names to another association
list containing parameters for that host. STATE-DIRECTORY is the path
to the tissue state directory."
  (format (current-error-port)
          "Tissue web server listening on ~a~%"
          (cond
           ;; IPv4 address
           ((= (sockaddr:fam socket-address) AF_INET)
            (format #f "~a:~a"
                    (inet-ntop (sockaddr:fam socket-address)
                               (sockaddr:addr socket-address))
                    (sockaddr:port socket-address)))
           ;; IPv6 address
           ((= (sockaddr:fam socket-address) AF_INET6)
            (format #f "[~a]:~a"
                    (inet-ntop (sockaddr:fam socket-address)
                               (sockaddr:addr socket-address))
                    (sockaddr:port socket-address)))
           ;; Unix socket
           ((= (sockaddr:fam socket-address) AF_UNIX)
            (sockaddr:path socket-address))))
  (run-server (lambda (request body)
                ;; Explicitly dereference the module and handler
                ;; variable each time so as to support live hacking.
                ((module-ref (resolve-module '(tissue web server))
                             'handler)
                 request body hosts state-directory))
              'http
              (cond
               ;; IPv4 or IPv6 address
               ((or (= (sockaddr:fam socket-address) AF_INET)
                    (= (sockaddr:fam socket-address) AF_INET6))
                (list #:family (sockaddr:fam socket-address)
                      #:addr (sockaddr:addr socket-address)
                      #:port (sockaddr:port socket-address)))
               ;; Unix socket
               ((= (sockaddr:fam socket-address) AF_UNIX)
                (let ((socket (make-unix-domain-server-socket
                               #:path (sockaddr:path socket-address))))
                  ;; Grant read-write permissions to all users.
                  (chmod (sockaddr:path socket-address) #o666)
                  (list #:socket socket))))))
