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
 (prefix-in pdf:      scribble/pdf-render)
 frog/util)


;;;-------------------------------------------------------------------
;;; Contracts
;;; TODO: Write more of these

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
  [xml-file->generic-scribble-data
   (->* ((and/c (or/c string? path?) file-exists?)) ()
        any/c)]
  [xml-file->generic-scribble-file
   (->* ((and/c (or/c string? path?) file-exists?) string?) ()
        any)]
  [main
   (->* ((list/c file-exists? string?)) ()
        any/c)]))


;;;-------------------------------------------------------------------
;;; Parameters

(define current-verbosity (make-parameter 0))
(define current-render-type (make-parameter #f))
(define current-disqus-file (make-parameter #f))
(define current-site (make-parameter #f))
(define auto-mode (make-parameter #f))


;;;-------------------------------------------------------------------
;;; Global definitions

(define scribble-suffix ".scrbl")
(define markdown-suffix ".md")
(define xml-suffix ".xml")

(define scribble-base-header "#lang scribble/base")
(define scribble-manual-header "#lang scribble/manual")

(define entry-marker 'event)
(define comment-marker 'comments)


;;;-------------------------------------------------------------------
;;; Entry fields

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


;;;-------------------------------------------------------------------
;;; Comment fields

(define comment-metadata-fields-xml
  '(id
    parentid
    state
    date))

(define comment-body-fields-xml
  '(user
    subject
    body))


;;;-------------------------------------------------------------------
;;; String printers

(define (dl0 . rst)
  (displayln (apply string-append rst)))

(define (dl . rst)
  (displayln (string-join rst))
  (newline))


;;;-------------------------------------------------------------------
;;; XML transformers

(define (xml->xexp data)
  (xml->xexpr
   (document-element
    (read-xml data))))

(define (xml-file->xexp file)
  (call-with-input-file file
    (λ (in)
      (xml->xexp in))))


;;;-------------------------------------------------------------------
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

(define (suffix->scrbl path) (path-replace-suffix path scribble-suffix))
(define (suffix->md path) (path-replace-suffix path markdown-suffix))
(define (suffix->xml path) (path-replace-suffix path xml-suffix))

;;; (tag-value '((user () "karim_sabi")))
;;; "karim_sabi"
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

(define (ljdump-entry-files path)
  (filter (λ (file)
            (string=?
             (substring (path->string file) 0 1) "L"))
          (ls path)))

(define (ljdump-comment-files path)
  (filter (λ (file)
            (string=?
             (substring (path->string file) 0 1) "C"))
          (ls path)))

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

(define (normalize-string str)
  ((apply compose string-normalizers) str))

;; (define (make-title-string str)
;;   (if (= (string-length str) 0)
;;       "title"
;;       (let* ([s (string-truncate (normalize-string str))]
;;              [end (string-ref s (- (string-length s) 1))])
;;         (if (char=? end #\-)
;;             (substring s 0 (- (string-length s) 2))
;;             s))))

(define (make-title-string str)
  (if (= (string-length str) 0)
      "title"
      (let* ([s (string-downcase (our-encode str))]
             [end (string-ref s (- (string-length s) 1))])
        (if (char=? end #\-)
            (substring s 0 (- (string-length s) 2))
            s))))

;;; A bruce force hack to replace the &...; symbols in the HTML
;;; files because I don't know how to inject arbitrary literal HTML
;;; code for the HTML output. See notes in procedure
;;; `entry-file->generic-scribble-data`
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

;;; String formatter
(define ($ cmd str [datum ""] [open "|{"] [close "}|"])
  (let ([at "@"]
        [dat (cond [(not (empty-string? datum))
                    (string-append "[" datum "]")]
                   [else ""])]
        [scmd (symbol->string cmd)])
    (string-append at scmd dat open str close)))

;;; (_ entry-file->frog-markdown-data infile outfile)
(define (out->file proc infile outfile)
  (prn1 "Converting ~a to ~a." infile outfile)
  (let ([infile-path (ensure-object-path infile)]
        [outfile-path (ensure-object-path outfile)])
    (with-output-to-file outfile-path
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


;;;-------------------------------------------------------------------
;;; Entry data extractors

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

(define (entry-xexp? xexp)
  (eqv? (car xexp) entry-marker))

(define (entry-file? file)
  (entry-xexp? (xml-file->xexp file)))


;;;-------------------------------------------------------------------
;;; Comment data extractors

(define (comment-metadata data)
  (for/list ([items (collect sxpath-value data
                             comment-metadata-fields-xml)])
    (map tag-value items)))

(define (comment-body data)
  (append-map (λ (tag)
                (let ([ltag (list tag)])
                  (case tag
                    [(user)
                     (list
                      (map (λ (path)
                             (cond [(eqv? (car path) 'user) (third path)]
                                   [else "anonymous"]))
                           (map fifth ((sxpath '(comment)) data))))]
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

(define (comment-xexp? xexp)
  (eqv? (car xexp) comment-marker))

(define (comment-file? file)
  (comment-xexp? (xml-file->xexp file)))


;;;-------------------------------------------------------------------
;;; Data formatters

;;; Entry file to generic Scribble data
(define (entry-file->generic-scribble-data file)
  (let ([item (entry-file-contents file)])
    (dl scribble-base-header)
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

;;; Entry file to Frog Markdown data
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

;;; TODO: Entry file to Frog Scribble data
(define (entry-file->frog-scribble-data file) #t)
(define/out->file entry-file->frog-scribble)

;;; Comment files
(define (comment-file->generic-scribble-data file)
  (dl scribble-base-header)
  (dl scribble-base-header)
  (dl ($ 'title "Comments"))
  (for ([item (comment-file-contents file)])
    (match item
      [(list id
             parent-id
             state
             date
             user
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


;;;-------------------------------------------------------------------
;;; Disqus stuff

(define (print-xml xexpr)
  (write-xml/content (xexpr->xml xexpr)))

(define (cdata-text text)
  (string-append "<![CDATA[" text "]]>"))

(define (build-entry-comment-pairs (directory "."))
  (filter (λ (x)
            (not (empty? x)))
          (for/list ([index (in-naturals)]
                     [file (directory-list directory)])
            (let* ([num (number->string index)]
                   [l-num (string-append "L-" num)]
                   [c-num (string-append "C-" num)])
              (if (and (file-exists? l-num)
                       (file-exists? c-num))
                  (list l-num c-num)
                  '())))))

(define (build-disqus-comment-head path-pairs)
  (for/list ([path-pair path-pairs])
    (match path-pair
      [(list entry-file comment-file)
       (let ([entry-item (entry-file-contents entry-file)]
             [comment-item (comment-file-contents comment-file)])
         (match entry-item
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
            (let ([date-string (iso-8601-date log-time)])
              `(item
                ()
                (title () ,subject)
                (link () ,(build-location entry-file (current-site)))
                (content:encoded () "<![CDATA[content]]>")
                ;; (content:encoded () ,(cdata-text body))
                (dsq:thread_identifier () ,(build-location entry-file))
                (wp:post_date_gmt () ,log-time)
                (wp:comment_status () "open")
                ,@(build-disqus-comment-body comment-item date-string)))]))])))

(define (build-disqus-comment-body comment-item date-string)
  (let ([default-disqus-id "disqusid"]
        [default-avatar "http://www.arayaclean.com/images/default-avatar.png"]
        [default-url "http://foo.bar.baz"]
        [default-address "@domain.com"]
        [default-ip-address "127.0.0.1"]
        [default-approval "1"])
    (for/list ([comment comment-item])
      (match comment
        [(list id
               parent-id
               state
               date
               user
               subject
               body)
         `(wp:comment
           ()
           (wp:comment_id () ,id)
           (wp:comment_author () ,user)
           (wp:comment_author_url () ,default-url)
           (wp:comment_author_email
            ()
            ,(string-append (string-replace user " " "_") default-address))
           (wp:comment_author_IP () ,default-ip-address)
           (wp:comment_date_gmt () ,(string-replace date-string "T" " "))
           (wp:comment_content
            ()
            ,(cdata-text (if (< (string-length body) 3) "..." body)))
           (wp:comment_approved () ,default-approval)
           (wp:comment_parent
            ()
            ,(if (= (string-length parent-id) 0)
                 "0"
                 parent-id)))]))))

(define (build-disqus-comment-data (directory (current-directory)))
  (let ([path-pairs (build-entry-comment-pairs directory)])
    (display "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
    (print-xml
     `(rss ((version "2.0")
            (xmlns:content "http://purl.org/rss/1.0/modules/content/")
            (xmlns:dsq "http://www.disqus.com/")
            (xmlns:dc "http://purl.org/dc/elements/1.1/")
            (xmlns:wp "http://wordpress.org/export/1.0/"))
           (channel
            ()
            ,@(build-disqus-comment-head path-pairs))))))

(define (build-disqus-comment-file directory output-file)
  (with-output-to-file output-file
    #:exists 'truncate/replace
    (λ ()
      (build-disqus-comment-data directory)))
  (replace-symbols-file output-file symbol-table))

(define (build-location file (site #f) (ssl #f))
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
       (let ([time (car (string-split log-time))]
             [title (make-title-string subject)])
         (cond [site (string-append
                      "http://" site "/blog/"
                      (string-replace time "-" "/")
                      "/"
                      title
                      "/")]
               [else (string-append
                      time
                      "-"
                      title)]))])))

(define (build-frog-markdown-path file)
  (build-path (string-append (build-location file) markdown-suffix)))

(define (build-frog-scribble-path file)
  (build-path (string-append (build-location file) scribble-suffix)))


;;;-------------------------------------------------------------------
;;; File writers

;; Frog Markdown
(define (xml-file->frog-markdown-file infile outfile)
  (let ([infile-path (ensure-object-path infile)]
        [outfile-path (ensure-object-path outfile)])
    (cond [(entry-file? infile-path)
           (entry-file->frog-markdown-file infile-path outfile-path)]
          [else #f])))

;;; TODO: Frog Scribble
;;; The source XML files contain HTML formatting. If we plug them here
;;; as is, it wouldn't work, because Scribble doesn't understand them,
;;; unlike Markdown.
(define (xml-file->frog-scribble-file infile outfile)
  (let ([infile-path (ensure-object-path infile)]
        [outfile-path (ensure-object-path outfile)])
    (cond [(entry-file? infile-path)
           (entry-file->frog-scribble-file infile-path outfile-path)]
          [else #f])))

;;; Generic Scribble
(define (xml-file->generic-scribble-data file)
  (cond [(entry-file? file)
         (entry-file->generic-scribble-data file)]
        [(comment-file? file)
         (comment-file->generic-scribble-data file)]))

(define (xml-file->generic-scribble-file infile outfile)
  (prn1 "Converting ~a to ~a." infile outfile)
  (let ([infile-path (ensure-object-path infile)]
        [outfile-path (ensure-object-path outfile)])
    (with-output-to-file outfile-path
      #:exists 'truncate/replace
      (λ ()
        (xml-file->generic-scribble-data infile-path)))))


;;;-------------------------------------------------------------------
;;; Renderers

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


;;;-------------------------------------------------------------------
;;; Top-level

(define (main files)
  (let ([render-type (current-render-type)]
        [input-files (if (auto-mode)
                         (append (ljdump-entry-files (current-directory))
                                 files)
                         files)])
    (case render-type
      [(disqus-comment)
       (and (current-site)
            (build-disqus-comment-file (current-directory) (current-disqus-file)))]
      [else
       (for ([file input-files])
         (let ([scribble-file (suffix->scrbl file)])
           (case render-type
             [(frog-markdown)
              (xml-file->frog-markdown-file file (build-frog-markdown-path file))]

             [(frog-scribble)
              (xml-file->frog-scribble-file file (build-frog-scribble-path file))]

             [(generic-scribble)
              (xml-file->generic-scribble-file file scribble-file)]

             [(html markdown text latex pdf)
              ;; The Scribble file created here acts only as an intermediary
              ;; format, and that we're mostly interested at the end
              ;; formats like HTML. It is also important to note that the
              ;; files created do not follow a specific format.
              (xml-file->generic-scribble-file file scribble-file)
              (when (and (file-exists? scribble-file)
                         render-type)
                (render-file render-type scribble-file))
              (post-process file)]

             [else #f])))])))

(module+ main
  (command-line
   #:once-each
   [("--markdown" "--frog-markdown")
    (""
     "Render to Frog Markdown output.")
    (current-render-type 'frog-markdown)]
   [("--scribble" "--frog-scribble")
    (""
     "Render to Frog Scribble output.")
    (current-render-type 'frog-scribble)]

   [("--site" "--root" "--root-site")
    site
    (""
     "Specify the root site to use.")
    (current-site site)]
   [("--disqus" "--disqus-file")
    disqus-file
    (""
     "Specify the output Disqus XML import file.")
    (current-disqus-file disqus-file)
    (current-render-type 'disqus-comment)]

   [("--generic-scribble" "--plain-scribble")
    (""
     "Render to generic Scribble output.")
    (current-render-type 'generic-scribble)]

   [("--auto" "--auto-mode")
    (""
     "Pick up ljdump files automatically.")
    (auto-mode #t)]

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

   ;; #:args (file . another-file)
   ;; (let ([files (cons file another-file)])
   ;;   (main files))

   #:args files
   (main files)))
