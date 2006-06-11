;; Guile Gtk html online manual href link munging.
;;
;; Copyright 2005, 2006 Free Software Foundation


;; Usage: guile -s mung-html.scm filename.html ...
;;
;; The given files are modified, so html links created by "makeinfo --html"
;; point to the right places on the gnu web server.
;;
;; Normally makeinfo spits out say
;;
;;     href="../guile/Foo.html"
;;
;; but it needs to become
;;
;;     href="/software/guile/docs/docs-1.8/guile-ref/Foo.html"
;;
;; This program is only meant for use after a makeinfo run, so if you make a
;; change to fix some of the munging here then you need to start from
;; makeinfo again, the program can't fix up what it's already munged.
;;


(use-modules (ice-9 regex)
	     (ice-9 rw)
	     (srfi srfi-13))


;; return string read from PORT up to eof
;;
(define (read-string-to-eof port)
  (let ((buf (make-string 4096)))
    (do ((lst   '()
		(cons (substring buf 0 nread) lst))
	 (nread (read-string!/partial buf port)
		(read-string!/partial buf port)))
	((not nread)                       ;; stop at #f
	 (string-concatenate-reverse lst)) ;; join parts
      ;; allow for 0 meaning EWOULDBLOCK
      ;; if 0 is in fact from a signal then this select does no harm
      (if (= 0 nread)
	  (select (list port) '() '())))))

;; return STR with all occurrances of OLD changed to NEW
;;
(define (string-substitute/global str old new)
  (let more ((upto 0)
	     (lst  '()))
    (let ((match (string-contains str old upto)))
      (if match
	  (more (+ match (string-length old))
		(cons* new (substring str upto match) lst))
	  (string-concatenate-reverse (cons (substring str upto) lst))))))

;; return STR with all matches of REGEXP changed according to SUBST-BITS
;; SUBST-BITS is `regexp-substitute' parameters
;;
;; writing biggish blocks to a string port is very slow in Guile 1.6.7,
;; avoid that
;;
(define (string-regexp-substitute/global str regexp . subst-bits)
  (let ((re (make-regexp regexp)))
    (let more ((upto 0)
	       (lst  '()))
      (let ((m (regexp-exec re str upto)))
	(if m
	    (begin
	      (more (match:end m)
		    (cons* (apply regexp-substitute #f m subst-bits)
			   (substring str upto (match:start m))
			   lst)))
	    (string-concatenate-reverse (cons (substring str upto) lst)))))))

(for-each
 (lambda (filename)
   (let* ((orig-body (call-with-input-file filename read-string-to-eof))
	  (body      orig-body))


     ;; guile
     ;; http://www.gnu.org/software/guile/docs/docs-1.8/guile-ref/index.html
     (set! body
	   (string-substitute/global
     	    body "href=\"../guile/index.html#Top"
     	    "href=\"/software/guile/docs/docs-1.8/guile-ref/index.html"))
     (set! body
     	   (string-regexp-substitute/global
     	    body "href=\"[.][.]/guile/([^.]+[.]html)#[^\"]+"
	    "href=\"/software/guile/docs/docs-1.8/guile-ref/" 1))



     (let ((m-list (list-matches "href=\"[.][.]/[a-z-]+/[^.]+[.]html(#[^\"]+)?\"" body)))
       (or (null? m-list)
	   (begin
	     (format #t "Unmunged links in ~a:\n" filename)
	     (for-each (lambda (m)
			 (format #t "    ~a\n" (match:substring m)))
		       m-list))))

     (if (not (string=? body orig-body))
	 (call-with-output-file filename
	   (lambda (port)
	     (display body port))))))

 (cdr (program-arguments)))

(exit 0)
