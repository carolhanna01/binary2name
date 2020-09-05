#! /usr/bin/guile -s
!#

(read-set! keywords 'prefix)

;; Export the help tree as HTML.

(use-modules (srfi srfi-13)
             (srfi srfi-1)
             (container nodal-tree)
             (oop goops)
             (sxml simple)
             (sxml transform)
             (texinfo)
             (texinfo html)
             (texinfo nodal-tree))

;; The caller is responsible for carring the returned list.
(define (arg-ref key %-args)
  (and=> (assq key (cdr %-args)) cdr))
(define (arg-req key %-args)
  (or (arg-ref key %-args)
      (error "Missing argument:" key %-args)))

(define (complement pred) (lambda (x) (not (pred x))))
(define-public (build-path . args)
  "Builds a path from elements in @var{args}, ignoring those equal to
\"\"."
  (string-join (filter (complement string-null?) args) "/"))

(if (not (eq? (length (program-arguments)) 3))
    (begin
      (format #t "Usage: make-html.scm TEXINFO-FILE DOCUMENT-PATH\n")
      (exit 1)))

(define texinfo-file (cadr (program-arguments)))
(define top-node (stexi->nodal-tree
                  (call-with-file-and-dir texinfo-file texi->stexi) 1))
(define document-path (caddr (program-arguments)))

(if (not (and (file-exists? document-path)
              (file-is-directory? document-path)))
    (error (string-append "Error: " document-path " is not a valid directory")))

(format #t "Building documents in ~A...\n" document-path)

(define (get-depth node)
  (let loop ((node node) (depth -1))
    (if node
        (loop (node-ref node 'parent) (1+ depth))
        depth)))

(define-public (list-join l infix)
  "Infixes @var{infix} into list @var{l}."
  (let loop ((ret '()) (l l))
    (cond
     ((null? l)
      (reverse ret))
     ((null? (cdr l))
      (loop (cons (car l) ret) (cdr l)))
     (else
      (loop (cons* infix (car l) ret) (cdr l))))))

(define xhtml-doctype
  (string-append
   "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" "
   "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n"))

(define (write-html-file path node)
  (let* ((children (node-children node))
         (depth (get-depth node))
         (leaf-node? (null? children))
         (header-path (string-append
                       (apply build-path (make-list (+ depth 2) "..")) "/")))
    (define (link-tail node)
      (string-append
       (urlify (node-ref node 'name))
       "/"))
    (define (make-navigation)
      (let* ((siblings (if (zero? depth)
                           (list node)
                           (node-children (node-ref node 'parent))))
             (index (list-index (lambda (n) (eq? n node)) siblings)) ;; srfi-1
             (numsiblings (length siblings))
             (prev (and (positive? index)
                        (list-ref siblings (1- index))))
             (next (and (< index (1- numsiblings))
                        (list-ref siblings (1+ index))))
             (next-following (if leaf-node? next (car children)))
             (up-path "../"))
        `(div (@ (class "book-navigation reversed"))
              ,(if prev 
                   `(a (@ (href ,(string-append  "../" (link-tail prev)))) "<")
                   "<")
              " | "
              (a (@ (href ,up-path)) "^")
              " | "
              ,(if next
                   `(a (@ (href ,(string-append  "../" (link-tail next)))) ">")
                   ">")
              " | "
              (a (@ (href
                     ,(if next-following
                          (if leaf-node?
                              (string-append "../" (link-tail next-following))
                              (link-tail next-following))
                          up-path)))
                 ":>"))))

    (define (make-subsections)
        (and (not leaf-node?)
             `(p (@ (class "subsections"))
                 ,@(list-join
                    (map
                     (lambda (node)
                       `(a (@ (href ,(link-tail node)))
                           ,(node-ref node 'name)))
                     children)
                    " | "))))

    (define (make-head)
      `(head
        (title
         ,(string-append "guile-gnome: docs: " (node-ref node 'name)))
        (style (@ (type "text/css"))
          ,(string-append "@import url(" header-path "base.css);"))))

    (define (make-body body)
      `(body
        (div (@ (id "body"))
             (div (@ (id "heading"))
                  (h1 "guile-gnome")
                  (div (@ (id "menu-bar") (class "reversed"))
                       (a (@ (href ,(apply string-append (make-list depth "../"))))
                          "top") " "
                       (a (@ (href ,(string-append header-path "docs/")))
                          "docs") " "
                       (a (@ (href ,(string-append header-path "download/")))
                          "download") " "
                       (a (@ (href ,(string-append header-path "dev/")))
                          "developers") " "
                       (a (@ (href ,(string-append header-path "contact/")))
                          "contact") " "
                       (a (@ (href ,(string-append header-path "links/")))
                          "links")))
             (div (@ (id "text"))
                  ,(make-navigation)
                  ,(make-subsections)
                  ,@body
                  ,(make-navigation)))))
        
    (let* ((file (open-output-file path))
           (stexi (node-ref node 'value)))
      (sxml->xml
       (pre-post-order
        (stexi->shtml stexi)
        `((head . ,(lambda (tag . body)
                     (make-head)))
          (body . ,(lambda (tag . body)
                     (make-body body)))
          (*text*          . ,(lambda (tag text) text))
          (*default*       . ,(lambda (tag . body)
                                (cons tag body)))))
       file)
      (close-port file))))

(define (maybe-mkdir path)
  (if (or (not (file-exists? path)) (not (file-is-directory? path)))
      (mkdir path)))

;; keep in sync with the one in (sxml texinfo html)
(define (urlify str)
  (string-downcase
   (string-map
    (lambda (c)
      (case c
        ((#\space #\/ #\:) #\-)
        (else c)))
    str)))

(define (make-document-path parent-path . nodes)
  (apply
   build-path
   parent-path
   (map
    (lambda (node)
      (urlify (node-ref node 'name)))
    nodes)))

(define (write-node parent-path node)
  (let ((children (node-children node)))
    (maybe-mkdir parent-path)
    (let ((path (build-path parent-path "index.html")))
      (format #t "Making node ~A...\n" path)
      (write-html-file path node)
      (for-each
       (lambda (node)
         (write-node (make-document-path parent-path node) node))
       children))))

(define (ref-resolver node-name manual-name)
  (define (find-node-named node name)
    (or
     (let loop ((tree (node-ref node 'value)))
       (cond
        ((null? tree) #f)
        ((and (pair? (car tree))
              (eq? (caar tree) 'node)
              (string=? (car (arg-req 'name (cadar tree))) name))
         node)
        (else
         (loop (cdr tree)))))
     (or-map
      (lambda (node)
        (find-node-named node name))
      (node-children node))))
  
  (if manual-name
      #f ;; We don't support refs to named manuals at the moment..
      (let* ((node (cond
                    ((or (not node-name) (string=? node-name "top"))
                     top-node)
                    (else
                     (or (find-node-named top-node node-name)
                         (error "No such node in manual:" manual-name node-name)))))
             (depth (get-depth node))
             (top-of-docs-path (apply build-path (make-list (1- depth) ".."))))
        (string-append
         (apply
          make-document-path
          top-of-docs-path
          (let loop ((nodes (list node)))
            (let ((parent (node-ref (car nodes) 'parent)))
              (if (not parent)
                  (cdr nodes)
                  (loop (cons parent nodes))))))
         "/#"
         (urlify node-name)))))
         
;; Install our own ref resolver.
(add-ref-resolver! ref-resolver)

;; finally, do the work.
(write-node (make-document-path document-path) top-node)
