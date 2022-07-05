(tissue-configuration
 #:project "tissue"
 #:indexed-documents (map (lambda (filename)
                            (indexed-document (cut read-gemtext-issue filename)
                                              (string-append "/" (replace-extension filename "html"))))
                          (gemtext-files-in-directory "issues"))
 #:web-files (cons (file "index.html"
                         (skribe-exporter "website/index.skb"))
                   (filter-map (lambda (filename)
                                 (file (replace-extension filename "html")
                                       (gemtext-exporter filename)))
                               (gemtext-files-in-directory "issues"))))
