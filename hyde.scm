(use srfi-1 hyde hyde-atom lowdown regex rfc3339 srfi-69 srfi-13 list-utils)

; adds markdown parsing through the lowdown egg
(translators (cons (list "md" markdown->html) (translators)))
(define translate-markdown (make-external-translator markdown-program))
; change output-dir
(output-dir "")

; default page layouts for certain page types
(default-page-vars '(((: bos "blog-posts/" (+ any) ".md" eos)
                      (layouts "blog.sxml" "default.sxml")
                      (displaytitle ""))
                     ((: bos "index.md")
                      (layouts "index.sxml" "default.sxml"))
                     ((: bos "tags.md")
                      (layouts "tags.sxml" "default.sxml"))
                     ((: bos "archive.md")
                      (layouts "archive.sxml" "default.sxml"))))

; "safe-linked" navbar
(define (navbar)
  (let ((nav `(("index.html" "Home")
               ("archive.html" "Archive")
               ("about.html" "About Me")
               ("tags.html" "Tags")
               ("feed.atom" (img (@ (src ,(safe-link "rss.png")))))))) 
    `(nav (ul ,(map (lambda (x) `(a (@ (href ,(safe-link (car x))))
                                     (li (button ,(car (cdr x)))))) nav)))))

; makes sure certain links always link to the root directory instead of the
; possible risk of linking to a nonexistant page in a subdirectory
(define site-link "https://libbro.github.io/")
(define (safe-link s)
  (string-append site-link s))

; wraps all page tags in a code tag and links to the respective tag in the tag
; page
(define (display-tags lt)
  (map (lambda (x) `(a (@ (href ,(string-append (safe-link "tags.html#") x))) 
                       (code ,x))) lt))

; renders and links page titles and information, used in frontpage
(define (render-posts pg)
  (map (lambda (page) (let ((var-list (page-vars (cdr page))))
                        `((a (@ (href ,(fix-link (car page)))) (h5 ,(alist-ref `title var-list equal?)))))) pg))

; a list of blog-posts ordered by date
(define (blog-posts)
  (blog-sort (filter (lambda (x) (not (equal? #f (string-match (regexp "blog-posts/.*\.+md$") (car x))))) (pages))))

(define (blog-page)
  (render-posts (blog-posts)))

(define (blog-frontpage)
  (cond
    ((< (length (blog-posts)) 20) (blog-page))
    (else (render-posts (take (blog-posts) 20)))))

; here lies the blog comparison functions

; compares the date values of two blog-posts with fn
(define (blog-date-cmp blg1 blg2 fn)
  (let ((blog1 (alist-ref `date (page-vars (cdr blg1)) equal?))
        (blog2 (alist-ref `date (page-vars (cdr blg2)) equal?)))
    (fn blog1 blog2)))

; blog sorting based on the blog date comparison function
(define (blog-sort lob)
  (sort lob (lambda (x0 x1) (blog-date-cmp x0 x1 >))))

; replaces ".whatever" with ".html"
(define (fix-link s)
  (define string-list (map string (string->list s)))
  (define (fl s0) (cond
                    ((null-list? s0) '())
                    ((string=? (car s0) ".") '("." "h" "t" "m" "l"))
                    (else (cons (car s0) (fl (cdr s0))))))
  (foldl string-append "" (fl string-list)))

; a wrapper for take which doesn't raise an exception if n > (length list)
(define (safe-take n l)
  (cond
    ((< (length l) n) l)
    (else (take l n))))

(define (member? x l)
  (ormap (lambda (d) (equal? x d)) l))

; collects the tag values from all of the blog posts and puts it into a
; nice little hash table
(define (collect-tags blg)
  (define HT (make-hash-table))
  (begin (map (lambda (blog) (let ((tags (alist-ref `tags (page-vars (cdr blog)))))
                        (map (lambda (t) (if (member? t (hash-table-keys HT))
                                           (hash-table-set! HT t (append (hash-table-ref HT t) (list blog)))
                                           (hash-table-set! HT t (list blog)))) tags))) blg)
         HT))

; maps through a hash table by extracting all of its keys, sorting it with srtfn and returning
; a list of the key associations. From there it applies a two input function to the key value 
; pair
(define (sorted-hash-map ht fn srtfn)
  (letrec ((keys (sort (hash-table-keys ht) srtfn))
           (two-fun (lambda (kl xl)
                      (cond
                        ((null-list? kl) '())
                        (else (cons (fn (car kl) (car xl)) (two-fun (cdr kl) (cdr xl))))))))
    (two-fun keys (map (lambda (x) (hash-table-ref ht x)) keys))))

; less generic form
(define (hash-table-page-map ht fn)
  (sorted-hash-map ht fn string<))

; renders tag page with a hash table input
(define (render-tagpage tht)
  (hash-table-page-map tht (lambda (k x) (append `((a (@ (name ,k) (class "tag-header")) (h2 ,k))) (render-posts x)))))
