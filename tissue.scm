(tissue-configuration
 #:project "tissue"
 #:indexed-documents (append (map (lambda (filename)
                                    (slot-set (read-gemtext-issue filename)
                                              'web-uri
                                              (string-append "/" (string-remove-suffix ".gmi" filename))))
                                  (gemtext-files-in-directory "issues"))
                             (map (lambda (commit)
                                    (slot-set commit
                                              'web-uri
                                              (string-append "https://git.systemreboot.net/tissue/commit/?id="
                                                             (commit-hash commit))))
                                  (commits-in-current-repository)))
 #:web-files (cons (file "index.html"
                         (skribe-exporter "website/index.skb"))
                   (filter-map (lambda (filename)
                                 (file (replace-extension filename "html")
                                       (gemtext-exporter filename)))
                               (gemtext-files-in-directory "issues"))))
