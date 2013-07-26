#lang racket/base

(require
 (for-syntax
  racket/base
  racket/syntax))

(require
 racket/contract
 racket/list
 racket/string
 racket/cmdline
 racket/match
 racket/file
 xml
 sxml
 "utils.rkt"
 "symbols.rkt"
 scribble/render
 (prefix-in text:     scribble/text-render)
 (prefix-in markdown: scribble/markdown-render)
 (prefix-in html:     scribble/html-render)
 (prefix-in latex:    scribble/latex-render)
 (prefix-in pdf:      scribble/pdf-render))

(provide
 (contract-out
  [xml->xexp
   (->* (any/c) ()
        xexpr/c)]
  [xml-file->xexp
   (->* (path?) ()
        xexpr/c)]
  [entry-file?
   (->* ((or/c string? path?)) ()
        boolean?)]
  [comment-file?
   (->* ((or/c string? path?)) ()
        boolean?)]
  [entry-file-contents
   (->* ((or/c string? path?)) ()
        any/c)]
  [comment-file-contents
   (->* ((or/c string? path?)) ()
        any/c)]
  [xml-file->scribble-data
   (->* ((and/c (or/c string? path?) file-exists?)) ()
        any/c)]
  [xml-file->scribble-file
   (->* ((and/c (or/c string? path?) file-exists?) string?) ()
        any)]
  [main
   (->* ((list/c file-exists? string?)) ()
        any/c)]))

;;; Parameters
(define current-verbosity (make-parameter 0))
(define current-render-type (make-parameter #f))

;;; Global definitions
(define program-name "livescribe")

(define scribble-suffix ".scrbl")

(define markdown-suffix ".md")

(define scribble-header "#lang scribble/base")

(define entry-marker 'event)

(define comment-marker 'comments)

(define entry-metadata-fields-xml
  '(itemid
    eventtime
    url
    ditemid
    event_timestamp
    reply_count
    logtime
    opt_preformatted
    personifi_tags
    hasscreened
    commentalter
    revtime
    opt_backdated
    current_moodid
    current_music
    revnum
    can_comment
    anum))

(define entry-body-fields-xml
  '(subject
    event
    taglist))

(define comment-metadata-fields-xml
  '(id
    parentid
    state
    date))

(define comment-body-fields-xml
  '(subject
    body))

(define (dl0 . rst)
  (displayln (apply string-append rst)))

(define (dl . rst)
  (displayln (string-join rst))
  (newline))

;;; Essentials
(define (xml->xexp data)
  (xml->xexpr
   (document-element
    (read-xml data))))

(define (xml-file->xexp file)
  (call-with-input-file file
    (λ (in)
      (xml->xexp in))))

;;; Helpers
(define (remove-newline-space str)
  (string-replace str "\n " ""))

(define (remove-newline str)
  (string-replace str "\n" ""))

(define (remove-newlines str)
  ((compose remove-newline remove-newline-space) str))

(define (xml-suffix? str)
  (if (regexp-match "[xX][mM][lL]" (suffix str))
      #t
      #f))

(define (suffix->scrbl path)
  (path-replace-suffix path scribble-suffix))

(define (suffix->md path)
  (path-replace-suffix path markdown-suffix))

(define (tag-value lst)
  (if (= (length lst) 3)
      (string-trim (third lst))
      ""))

(define (sxpath-value path data)
  (let ([value ((sxpath `(// ,path)) data)])
    (cond [(null? value) (list '())]
          [else value])))

(define (collect-tag-values data tags)
  (map-append tag-value
              (collect sxpath-value data tags)))

(define (iso-8601-date date)
  (string-replace date " " "T"))

;;; The following two procedures are esh-too-pid.
(define (ljdump-entry-files path)
  (filter (λ (file)
            (string=? (substring (path->string file) 0 1)
                      "L"))
          (ls path)))

(define (ljdump-comment-files path)
  (filter (λ (file)
            (string=? (substring (path->string file) 0 1)
                      "C"))
          (ls path)))

(define (string-remove str char)
  (string-replace str (char->string char) ""))

(define excluded-title-chars
  '(#\/ #\" #\? #\! #\' #\, #\. #\@ #\% #\= #\- #\+ #\^ #\& #\*
    #\( #\) #\[ #\] #\{ #\} #\| #\" #\_ #\$ #\= #\: #\;))

(define (spaces->dashes str)
  (string-replace str " " "-"))

(define string-normalizers
  (append (list string-downcase
                spaces->dashes
                string-normalize-spaces)
          (map (λ (char)
                 (λ (str)
                   (string-remove str char)))
               excluded-title-chars)))

(define (string-truncate str)
  (let ([string-max (string-length str)]
        [ideal-max 40])
    (if (< string-max ideal-max)
        (substring str 0 string-max)
        (substring str 0 ideal-max))))

(define (normalize-string str)
  ((apply compose string-normalizers) str))

(define (title-string str)
  (if (= (string-length str) 0)
      "title"
      (let* ([s (string-truncate (normalize-string str))]
             [end (string-ref s (- (string-length s) 1))])
        (if (char=? end #\-)
            (substring s 0 (- (string-length s) 2))
            s))))

;;; Bruce force hack to replace the &...; symbols in the HTML files
;;; because I don't know how to inject arbitrary literal HTML code
;;; for the HTML output. See notes in procedure
;;; `entry-file->scribble-data`
(define (replace-symbols str table)
  (let loop ([keys (hash-keys table)]
             [acc str])
    (cond [(null? keys) acc]
          [else (loop (cdr keys)
                      (string-replace
                       acc
                       (car keys)
                       (char->string (hash-ref table (car keys)))))])))

(define (replace-symbols-file file table)
  (let ([data (replace-symbols (file->string file) table)])
    (with-output-to-file file
      #:exists 'truncate/replace
      (λ ()
        (display data)))))

(define (post-process file)
  ;; TODO: Is actually acceptable?
  (case (current-render-type)
    [(html) (replace-symbols-file
             (path-replace-suffix file ".html")
             symbol-table)]
    [else (void)]))

;;; A neat, debugging procedure shamelessly stolen from
;;; http://github.com/greghendershott/frog/
(define (prn lvl fmt . args)
  (when (>= (current-verbosity) lvl)
    (apply printf fmt args)
    (newline)))

(define (prn0 fmt . args) (apply prn 0 fmt args))
(define (prn1 fmt . args) (apply prn 1 fmt args))
(define (prn2 fmt . args) (apply prn 2 fmt args))


;;; Entries
(define (entry-metadata data)
  (collect-tag-values data entry-metadata-fields-xml))

(define (entry-body data)
  (append-map (λ (tag)
                (case tag
                  [(subject taglist)
                   (collect-tag-values data (list tag))]
                  [(event)
                   (list (foldr string-append ""
                                (rrest (first (sxpath-value tag data)))))]))
              entry-body-fields-xml))

(define (entry-data-contents data)
  (append (entry-metadata data)
          (entry-body data)))

(define (entry-file-contents file)
  (let ([data (xml-file->xexp file)])
    (entry-data-contents data)))

;;; Comments
(define (comment-metadata data)
  (for/list ([items (collect sxpath-value data
                             comment-metadata-fields-xml)])
    (map tag-value items)))

(define (comment-body data)
  (append-map (λ (tag)
                (let ([ltag (list tag)])
                  (case tag
                    [(subject)
                     (list (collect-tag-values data ltag))]
                    [(body)
                     (list (map (λ (x)
                                  (foldr string-append "" (rrest x)))
                                (sxpath-value tag data)))])))
              comment-body-fields-xml))

(define (comment-data-contents data)
  (collect-cars
   (append (comment-metadata data)
           (comment-body data))))

(define (comment-file-contents file)
  (let ([data (xml-file->xexp file)])
    (comment-data-contents data)))

;;; Predicates
(define (entry-xexp? xexp)
  (eqv? (car xexp) entry-marker))

(define (comment-xexp? xexp)
  (eqv? (car xexp) comment-marker))

(define (entry-file? file)
  (entry-xexp? (xml-file->xexp file)))

(define (comment-file? file)
  (comment-xexp? (xml-file->xexp file)))

;;; String formatters
(define ($ cmd str [datum ""] [open "|{"] [close "}|"])
  (let ([at "@"]
        [dat (cond [(not (empty-string? datum))
                    (string-append "[" datum "]")]
                   [else ""])]
        [scmd (symbol->string cmd)])
    (string-append at scmd dat open str close)))

;;; Headers
(define (display-scribble-header)
  (dl scribble-header))

(define (create-sutils-file) '())

(define (display-headers)
  (display-scribble-header))

;;; File writers
;;; (_ entry-file->frog-markdown-data infile outfile)
(define (out->file proc infile outfile)
  (prn1 "Converting ~a to ~a." infile outfile)
  (let ([ifile (ensure-object-path infile)]
        [ofile (ensure-object-path outfile)])
    (with-output-to-file ofile
      #:exists 'truncate/replace
      (λ () (proc infile)))))

;; (_ entry-file->frog-markdown)
(define-syntax (define/out->file stx)
  (syntax-case stx ()
    [(_ base)
     (with-syntax ([name/data (format-id stx "~a-data" #'base)]
                   [name/file (format-id stx "~a-file" #'base)])
       #'(define (name/file infile outfile)
           (out->file name/data infile outfile)))]))

;;; Entry files
(define (entry-file->scribble-data file)
  (let ([item (entry-file-contents file)])
    (display-headers)
    (match item
      [(list item-id
             event-time
             url
             d-item-id
             event-timestamp
             reply-count
             log-time
             opt-preformatted
             personifi-tags
             has_screened
             comment-alter
             rev-time
             opt-backdated
             current-mood-id
             current-music
             rev-num
             can-comment
             a-num
             subject
             body
             tag-list)
       (dl ($ 'title subject))
       (dl ($ 'bold "Item ID:") item-id)
       (dl ($ 'bold "Event Time:") event-time)
       (dl ($ 'bold "Event Timestamp:") event-timestamp)
       (dl ($ 'bold "Revision time:") rev-time)
       (dl ($ 'bold "Revision number:") rev-num)
       (dl ($ 'bold "Log time:") log-time)
       (dl ($ 'bold "Reply count:") reply-count)
       (dl ($ 'bold "Current Mood:") current-mood-id)
       (dl ($ 'bold "Current Music:") current-music)
       (dl ($ 'bold "URL:") ($ 'url url))
       (dl ($ 'bold "Tags:") tag-list)
       (dl ($ 'bold "Body:"))

       ;; NOTE:
       ;; The following expression works quite well with the Markdown
       ;; output, only. But for the rest, the HTML tags are inserted.
       ;; This is understable for the PDF and LaTeX output since they
       ;; has no intrinsic knowldege of HTML.
       ;; What we need to have is the ability to insert the HTML
       ;; content here, as is. That is, we don't want the "<" symbols
       ;; to be translated to "&lt;".
       ;;
       ;; UPDATE:
       ;; The ideal solution is to use something like the one described in
       ;; http://goo.gl/iQLj5. Unfortunately, I don't know how to apply
       ;; it outside Javascript. So what we're doing now is we replace
       ;; all the &...; symbols found in the HTML document, as listed in
       ;; ./symbols.rkt
       (dl ($ 'para body))])))


;;; Entry data via Frog
(define (entry-file->frog-markdown-data file)
  (let ([item (entry-file-contents file)])
    (match item
      [(list item-id
             event-time
             url
             d-item-id
             event-timestamp
             reply-count
             log-time
             opt-preformatted
             personifi-tags
             has_screened
             comment-alter
             rev-time
             opt-backdated
             current-mood-id
             current-music
             rev-num
             can-comment
             a-num
             subject
             body
             tag-list)
       ;; We'll use log-time since it seems to be the earliest of all
       ;; the dates available in the file
       (let ([date-string (iso-8601-date log-time)])
         (dl0 "    Title: " subject)
         (dl0 "    Date: " date-string)
         (dl0 "    Tags: " tag-list)
         (newline)
         (dl0 body))])))

(define/out->file entry-file->frog-markdown)

;; Comment data to Disqus
(define (comment-file->disqus-comment-data file)
  ;; (for ([item (comment-file-contents file)])
  ;;   (match item
  ;;     [(list id
  ;;            parent-id
  ;;            state
  ;;            date
  ;;            subject
  ;;            body)
  ;;      (print-disqus-comment ...)
  ;;      ]))
  #t
  )

(define/out->file comment-file->disqus-comment)

(define (make-title file)
  (let ([item (entry-file-contents file)])
    (match item
      [(list item-id
             event-time
             url
             d-item-id
             event-timestamp
             reply-count
             log-time
             opt-preformatted
             personifi-tags
             has_screened
             comment-alter
             rev-time
             opt-backdated
             current-mood-id
             current-music
             rev-num
             can-comment
             a-num
             subject
             body
             tag-list)
       (let ([date (car (string-split log-time))]
             [title (title-string subject)])
         (string-append date "-" title))])))

(define (build-frog-markdown-path file)
  (build-path (string-append (make-title file) markdown-suffix)))

(define (build-frog-scribble-path file)
  (build-path (string-append (make-title file) scribble-suffix)))

;; TODO
(define (entry-file->frog-scribble-data file) #t)
(define/out->file entry-file->frog-scribble)

;;; Comment files
(define (comment-file->scribble-data file)
  (display-headers)
  (dl ($ 'title "Comments"))
  (for ([item (comment-file-contents file)])
    (match item
      [(list id
             parent-id
             state
             date
             subject
             body)
       (dl ($ 'section subject))
       (dl ($ 'bold "Subject:") subject)
       (dl ($ 'bold "ID:") id)
       (dl ($ 'bold "Parent ID:") parent-id)
       (dl ($ 'bold "State:") state)
       (dl ($ 'bold "Date:") date)
       (dl ($ 'bold "Body:"))
       (dl ($ 'para body))])))


;;; Disqus stuff
;; (define (comment-file->disqus-data file) #t)
;; (define/out->file comment-file->disqus)

;; TODO: Merge this with xml-file->frog-markdown-file
(define (foo->bar infile outfile)
  (let ([ifile (ensure-object-path infile)]
        [ofile (ensure-object-path outfile)])
    (cond [(entry-file? ifile)
           (entry-file->frog-markdown-file ifile ofile)]
          [(comment-file? ifile)
           (comment-file->disqus-comment-file ifile ofile)])))

(define (build-disqus-comment-path file) #t)

;;; Frog stuff

;;; NOTE
;;; Do we actually need to do this since we are going to use Disqus
;;; for the comments?
;;; What if we output directly to the Disqus XML format, instead?

;;; TODO?
(define (comment-file->frog-markdown-data file) #t)
(define (comment-file->frog-markdown-file file) #t)
(define (comment-file->frog-scribble-data file) #t)
(define (comment-file->frog-scribble-file file) #t)


;; XML and Disqus stuff
(define (print-xml xexpr)
  (write-xml/content (xexpr->xml xexpr)))

(define (print-disqus-comment post-title
                              post-url
                              post-content
                              post-date

                              comment-id
                              comment-parent-id
                              comment-author
                              comment-author-email
                              comment-author-url
                              comment-author-ip
                              comment-date
                              comment-content

                              disqus-user-id
                              disqus-avatar

                              (comment-approved "1")
                              (disqus-id "disqus_identifier")
                              (comment-status "open"))
  (display "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
  (print-xml
   `(rss ((version "2.0")
          (xmlns:content "http://purl.org/rss/1.0/modules/content/")
          (xmlns:dsq "http://www.disqus.com/")
          (xmlns:dc "http://purl.org/dc/elements/1.1/")
          (xmlns:wp "http://wordpress.org/export/1.0/"))
         (channel
          ()
          (item
           ()
           (title () ,post-title)
           (link () ,post-url)
           (content:encoded () ,post-content)
           (dsq:thread_identifier () ,disqus-id)
           (wp:post_date_gmt () ,post-date)
           (wp:comment_status () ,comment-status)
           (wp:comment
            ()
            (dsq:remote
             ()
             (dsq:id () ,disqus-user-id)
             (dsq:avatar () ,disqus-avatar))
            (wp:comment_id () ,comment-id)
            (wp:comment_author () ,comment-author)
            (wp:comment_author_email () ,comment-author-email)
            (wp:comment_author_url () ,comment-author-url)
            (wp:comment_author_IP () ,comment-author-ip)
            (wp:comment_date_gmt () ,comment-date)
            (wp:comment_content () ,comment-content)
            (wp:comment_approved () ,comment-approved)
            (wp:comment_parent () ,comment-parent-id))))))
  (newline))

;;; Regular dispatchers

;; Scribble intermediary
(define (xml-file->scribble-data file)
  (cond [(entry-file? file)
         (entry-file->scribble-data file)]
        [(comment-file? file)
         (comment-file->scribble-data file)]))

(define (xml-file->scribble-file infile outfile)
  (prn1 "Converting ~a to ~a." infile outfile)
  (let ([ifile (ensure-object-path infile)]
        [ofile (ensure-object-path outfile)])
    (with-output-to-file ofile
      #:exists 'truncate/replace
      (λ ()
        (xml-file->scribble-data ifile)))))

;; Frog direct
(define (xml-file->frog-markdown-file infile outfile)
  (let ([ifile (ensure-object-path infile)]
        [ofile (ensure-object-path outfile)])
    (cond [(entry-file? ifile)
           (entry-file->frog-markdown-file ifile ofile)]
          ;; TODO: Since we're aiming to move the comments to Disqus,
          ;; do we need to have this?
          [(comment-file? ifile)
           #t])))

(define (xml-file->frog-scribble-file infile outfile) #t)

;;; Render
(define (build-listof-parts files)
  (map (λ (file)
         (dynamic-require `(file ,file) 'doc))
       files))

(define (build-listof-dests files suffix)
  (map (λ (file)
         (path-replace-suffix file suffix))
       files))

(define (render-file type file)
  (let* ([files (map ensure-string-path (list file))]
         [parts (build-listof-parts files)])
  (case type
    [(markdown md)
     (prn1 "Rendering ~a as Markdown." file)
     (render parts (build-listof-dests files ".md")
             #:render-mixin markdown:render-mixin)]
    [(text txt)
     (prn1 "Rendering ~a as Plaintext." file)
     (render parts (build-listof-dests files ".txt")
             #:render-mixin text:render-mixin)]
    [(html)
     (prn1 "Rendering ~a as single HTML file." file)
     (render parts (build-listof-dests files ".html")
             #:render-mixin html:render-mixin)]
    [(latex)
     (prn1 "Rendering ~a as LaTeX." file)
     (render parts (build-listof-dests files ".tex")
             #:render-mixin latex:render-mixin)]
    [(pdf)
     (prn1 "Rendering ~a as PDF." file)
     (render parts (build-listof-dests files ".pdf")
             #:render-mixin pdf:render-mixin)]
    [else (error 'render-file "Unknown render type: ~a" type)])))

;;; Top-level
(define (main files)
  (for ([file files])
    (let ([scribble-file (suffix->scrbl file)]
          [render-type (current-render-type)]
          [frog-markdown-file (build-frog-markdown-path file)]
          [frog-scribble-file (build-frog-scribble-path file)]
          [disqus-comment-file (build-disqus-comment-path file)])

      (case render-type
        [(frog-markdown)
         (xml-file->frog-markdown-file
          file
          frog-markdown-file)]

        [(frog-scribble)
         (xml-file->frog-scribble-file
          file
          frog-scribble-file)]

        [(disqus-comment)
         ;; (xml-file->disqus-comment-file
         ;;  file
         ;;  disqus-comment-file)
         #t]

        [(html markdown text latex pdf)
         ;; The Scribble file created here acts only as an intermediary
         ;; format, and that we're mostly interested at the end
         ;; formats like HTML. It is also important to note that the
         ;; file created does not follow any specific format.
         (xml-file->scribble-file file scribble-file)

         (when (and (file-exists? scribble-file)
                    render-type)
           (render-file render-type scribble-file))

         (post-process file)]

        ;; TODO: This is ill, because it doesn't provide any form of
        ;; feedback, to indicate whethere the operation was successful
        ;; or not
        [else #t]))))

(module+ main
  (command-line
   #:program program-name

   #:once-each
   [("--frog-markdown" "--fm")
    (""
     "Render to Frog Markdown")
    (current-render-type 'frog-markdown)]
   [("--frog-scribble" "--fs")
    (""
     "Render to Frog Scribble")
    (current-render-type 'frog-scribble)]

   [("--html")
    (""
     "Render HTML files.")
    (current-render-type 'html)]
   [("--markdown")
    (""
     "Render Markdown files.")
    (current-render-type 'markdown)]
   [("--text")
    (""
    "Render Text files.")
    (current-render-type 'text)]
   [("--latex")
    (""
    "Render LaTeX files.")
    (current-render-type 'latex)]
   [("--pdf")
    (""
    "Render PDF files.")
    (current-render-type 'pdf)]

   #:once-any
   [("-v" "--verbose")
    (""
     "Compile with verbose messages.")
    (current-verbosity 1)
    (prn1 "Verbose output enabled.")]
   [("-V" "--very-verbose")
    (""
     "Compile with very verbose messages.")
    (current-verbosity 2)
    (prn2 "Very verbose output enabled.")]
   #:args (file . another-file)
   (let ([files (cons file another-file)])
     (main files))))
