(tissue-configuration
 #:project "tissue"
 #:indexed-documents (map (lambda (filename)
                            (indexed-document (cut read-gemtext-issue filename)
                                              (string-append "/" (replace-extension filename "html"))))
                          (gemtext-files-in-directory "issues")))
