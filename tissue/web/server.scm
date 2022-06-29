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
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (htmlprag)
  #:use-module (sxml simple)
  #:use-module ((system repl server) #:select (make-unix-domain-server-socket))
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web server)
  #:use-module (web uri)
  #:use-module (xapian xapian)
  #:use-module (tissue conditions)
  #:use-module (tissue document)
  #:use-module (tissue search)
  #:export (start-web-server))

(define %css
  "
body {
    max-width: 1000px;
    margin: 0 auto;
}

form {
    text-align: center;
}

.search-result {
    list-style-type: none;
    padding: 0.5em;
}

.search-result a {
    text-decoration: none;
    font-size: larger;
}

.search-result-title, .search-result-title:before {
    vertical-align: middle;
}

.search-result-document .search-result-title:before, .search-result-issue .search-result-metadata:before {
    font-size: xx-small;
    font-weight: bold;
    margin: 0 0.2em;
    padding: 0 0.4em;
    background-color: darkmagenta;
    color: white;
}

.search-result-document .search-result-title:before {
    content: \"DOC\";
}

.search-result-open-issue .search-result-metadata:before {
    content: \"ISSUE\";
    background-color: green;
}

.search-result-closed-issue .search-result-metadata:before {
    content: \"✓ ISSUE\";
}

.search-result-metadata {
    color: dimgray;
    font-size: smaller;
}

.search-result-snippet {
    font-size: smaller;
}

a.tag {
    padding: 0.25em 0.4em;
    color: white;
    background-color: blue;
    border-radius: 0.5em;
    margin: auto 0.25em;
    font-size: smaller;
}

a.tag-bug {
    background-color: red;
}

a.tag-feature {
    background-color: green;
}

a.tag-progress, a.tag-unassigned {
    background-color: orange;
    color: black;
}

a.tag-chore {
    background-color: khaki;
    color: black;
}")

(define (make-search-page results query css)
  "Return SXML for a page with search RESULTS produced for QUERY. CSS is
a URI to a stylesheet."
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
           (input (@ (type "submit") (value "Search"))))
     (ul ,@results))))

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

(define (handler request body xapian-index css)
  "Handle web REQUEST with BODY and return two values---the response
headers and body. XAPIAN-INDEX is the path to the xapian database
relative to the top-level of the current git repository. CSS is a URI
to a stylesheet."
  (let ((path (uri-path (request-uri request)))
        (parameters (query-parameters (uri-query (request-uri request)))))
    (format #t "~a ~a\n"
            (request-method request)
            path)
    (cond
     ((member path (list "/" "/search"))
      (let ((search-query (or (assoc-ref parameters "query")
                              "")))
        (values '((content-type . (text/html)))
                (sxml->html
                 (make-search-page
                  (call-with-database xapian-index
                    (lambda (db)
                      (search-map document->sxml db search-query)))
                  search-query
                  css)))))
     (else
      (values (build-response #:code 404)
              (string-append "Resource not found: "
                             (uri->string (request-uri request))))))))

(define (start-web-server socket-address xapian-index css)
  "Start web server listening on SOCKET-ADDRESS. XAPIAN-INDEX is the
path to the xapian database relative to the top-level of the current
git repository. CSS is a URI to a stylesheet."
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
                 request body xapian-index css))
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
                (list #:socket (make-unix-domain-server-socket
                                #:path (sockaddr:path socket-address)))))))
