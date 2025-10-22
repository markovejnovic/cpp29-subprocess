#!/usr/bin/env guile
!#

;;; Process Markdown files by running template transformations.

(use-modules (ice-9 ftw)
             (ice-9 textual-ports)
             (srfi srfi-1)
             (srfi srfi-26))

(define (find-git-root start-path)
  (let loop ((path (canonicalize-path start-path)))
    (cond
      ((file-exists? (string-append path "/.git")) path)
      ((string=? path "/") #f)  ; Reached root, not found
      (else (loop (dirname path))))))

(define (read-file file)
  "Read the contents of a file into a string"
  (let ((full-path (canonicalize-path file)))
    (if (file-exists? full-path)
        (call-with-input-file full-path get-string-all)
        (begin
          (display (string-append "Error: File not found - " full-path "\n"))
          (exit 1)))))

(define (filename-without-ext path)
  (let* ((base (basename path))
         (dot-pos (string-rindex base #\.)))
    (if dot-pos
        (substring base 0 dot-pos)
        base)))

(define (parse-md-file file-source)
  "Extract :::(template-blocks) from the Markdown source"
  (define (accumulator all-blocks current-block line)
    (let ((old-lines (cdr (assoc 'lines current-block))))
      (if (eq? (cdr (assoc 'type current-block)) 'basic)
        (if (string=? line ":::(template-block)") ; Start of block marker
          (vector (cons current-block all-blocks)
                  (list (cons 'type 'template)
                        (cons 'lines '())))
          (vector all-blocks (list (cons 'type 'basic)
                                   (cons 'lines (cons line old-lines)))))
        (if (string=? line ":::(/template-block)") ; End of block marker
          (vector (cons current-block all-blocks)
                  (list (cons 'type 'basic)
                  (cons 'lines '())))
          (vector all-blocks (list (cons 'type 'template)
                                   (cons 'lines (cons line old-lines))))))))

  (let ((lines (string-split file-source #\newline)))
    (let ((blocks (vector-ref
                    (fold (lambda (el acc) (accumulator (vector-ref acc 0)
                                                        (vector-ref acc 1) el))
                          (vector '()
                                  (list (cons 'type 'basic)
                                        (cons 'lines '()))) lines) 0)))
      (reverse
        (map (lambda (block)
             (list (cons 'type (cdr (assoc 'type block)))
                   (cons 'content (string-join (reverse (cdr (assoc 'lines block))) "\n"))))
             blocks)))))



(define (transform-blocks block-list script-path)
  "Take all blocks and transform them appropriately"
  (define (basic-transformer block) block)
  (define (template-transformer block)
    (define mdlisp-module (make-fresh-user-module))
    (module-define! mdlisp-module '%repo-root (find-git-root "."))
    (module-define! mdlisp-module '%my-path script-path)
    (module-define!
      mdlisp-module
      '%my-siblings (lambda ()
                      (scandir (dirname script-path)
                               (lambda (name)
                                 (not (member name
                                              (list "." ".." (basename script-path))))))))
    (module-define! mdlisp-module '%without-ext filename-without-ext)

    (eval-string block mdlisp-module))

  (define (transformer block)
    (let ((block-type (cdr (assoc 'type block)))
          (content (cdr (assoc 'content block))))
      (case block-type
        ((basic) (basic-transformer content))
        ((template) (template-transformer content)))))

  (map transformer block-list))

(define (main args)
  (if (null? args)
      (begin
        (display "Usage: md-proc.scm -- <path>\n")
        (exit 1))
      (let ((full-path (canonicalize-path (car args))))
        (if (file-exists? full-path)
          (let ((md-source (call-with-input-file full-path get-string-all)))
            (let ((md-blocks (parse-md-file md-source)))
              (let ((transformed (transform-blocks md-blocks full-path)))
                (display (string-join transformed "\n"))
                (newline))))
          (begin
            (display (string-append "Error: File not found - " full-path "\n"))
            (exit 1))))))

;; Get command-line arguments (skip the script name and -- separator)
(let* ((all-args (cdr (command-line)))
       (args (filter (lambda (arg) (not (string=? arg "--"))) all-args)))
  (main args))
