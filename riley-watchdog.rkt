#lang racket
(require racket/cmdline)

(define (get-input path)
  (call-with-input-file path
    (lambda (in)
      (let loop ((inpu (read-line in))
		 (acc '()))
	(if (equal? eof inpu)
	    (reverse acc)
	    (loop (read-line in) (cons (string-split inpu "\t") acc)))))))

(define (format-input lst)
  (map (lambda (y) (take (drop y 1) 4)) lst))

(define (output-format lst)
  (call-with-output-file "test.tex"
    (lambda (out)
      (display lst out)) #:exists 'replace))

					; Latex Constants
(define header-name "\\dotfill \\textsc{Ordered by:} ")
(define header-set "\\dotfill \\textsc{Set:} ")
(define header-show "\\dotfill \\textsc{Show:} ")
(define date-header "\\\\ {\\bf Pickup Date:} \\today \\dotfill")
(define rental-period "{\\bf {Rental Period:}")
(define begin-table "\\begin{invoiceTable}")
(define end-table "\\end{invoiceTable}")
(define unitrow "\\unitrow{")
(define begin-img-table "\\figureSeriesFloat{}{")
(define end-img-table "}")
(define end-document " \\end{document}")
(define begin-document "\\begin{document}")
;;;Takes an invoice and root-dir
(define tail-conf "
\\par
{\\scriptsize \\begin{singlespace} It is agreed that \\textsc{Lessee} assumes all liability and responsibility for the item(s) listed below.  When item(s) have been accepted by \\textsc{Lessee} (as evidenced by \\textsc{Lessee’s} signature below), and while in the custody and possession of \\textsc{Lessee}, its employees, agents, owners, directors and officers and any other person or entity to whom the item(s) is entrusted by \\textsc{Lessee}, or assigns.  Liability extends to the full replacement cost or repair, in Lessor’s sole discretion.  Further, Lessee assumes full responsibility for any liability arising because of the use of the item(s) during the term of the lease and until item(s) is returned to the custody and control of Lessor (as evidenced by the written acknowledgement by Lessor). Further, to the extent permitted by law, the \\textsc{Lessee} agrees to protect, indemnify, defend and hold harmless Lessor, its directors, officers, agents, shareholders, and employees, against all claims or damages to people or property and costs (including reasonable attorney’s fees), up to Lessor’s pro-rata share of the item(s), arising out of or connected with the operation of \\textsc{Lessee’s} activities with respect to the item(s), including damage caused by inadequate maintenance of the item(s), use of the item(s), or a part thereof, by any customer, any guest, invitee, or by any agent of the \\textsc{Lessee}. Replacement value of all items (unless otherwise noted) equals (10X) first week rate. \\end{singlespace}}

{\\color{red} \\textsc{Signature}}\\hspace{0.5cm} \\makebox[3in]{\\hrulefill} \\hspace{0.5cm} \\textsc{Date}\\hspace{0.5cm} \\makebox[1in]{\\hrulefill} \\\\
\\textsc{Print}\\hspace{1.25cm} \\makebox[3in]{\\hrulefill}
  ")

(define document-conf "\\documentclass{invoice}

\\def \\tab {\\hspace*{3ex}} 

\\usepackage{color}
\\usepackage{courier}
\\usepackage{setspace}
\\usepackage{graphicx}
\\usepackage{pgffor}
\\usepackage{caption}
\\usepackage{expl3}
")

(define heading-conf "\\includegraphics[scale=.33]{caps}
\\hfil{\\huge\\color{red}{\\textsc{Checkout Sheet}}}\\hfil
 \\bigskip\\break 
\\break
\\hrule 

12 Turner Field \\hfill \\emph{Mobile:} (000) 000-0000 \\\\
APT000 \\hfill{ \\emph{Office:} (000) 000-0000} \\\\

Atlanta 30011 \\hfill email@email.org
\\\\ \\\\
{\\bf Invoice To:} \\\\ ")

					; --------------------- End Constants
(define (format-description item)
  (string-append unitrow (first item) "}{"
		 (second item) "}{"
		 (third item) "}{}{}"))

(define (write-latex header)
  (let ((showdata (first header))
	(itemdata (rest header )))
    (call-with-output-string
     (lambda (out)
       (display document-conf out)
       (display "\\graphicspath{{.}}" out)
       (display begin-document out)
       (newline out)
       (display heading-conf out)
       (display "\\dotfill \\textsc{Ordered by: " out)
       (display (third showdata) out)
       (display "}" out)
       (display "\\dotfill \\textsc{Set: " out)
       (display (second showdata) out)
       (display "}" out)
       (display "\\dotfill \\textsc{Show: " out)
       (display (first showdata) out)
       (display "}" out)
       (newline out) (newline out)
       (display date-header out)
       (display rental-period out)
       (newline out)
       (display tail-conf out)
       (display begin-table out)
       (newline out)
       (map (lambda (b) (display (format-description b) out)
		    (newline out)) itemdata)
       (display end-table out)
       (display end-document out)))))

(define (directory-from-path path)
  (let-values ([(x y z) (split-path path)])
    x))

(define (create-latex path)
  (output-format (write-latex (format-input (get-input path))))
  (system "pdflatex -interaction nonstopmode test.tex")
  "test.pdf")

(define (get-pictures path)
  (string-append (path->string (directory-from-path path))
                 "pictures.pdf"))

(define (combine-latex path)
  (let ((stu (create-latex path))
        (cuf (get-pictures path)))
    (system (string-append
             "gs -q -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -sOutputFile="
             (path->string (directory-from-path path)) "finished.pdf "
             stu " " cuf))))

;; If -c is not set then assume a .ril already exists
(provide main)
(define (main . args)
  (command-line #:program "(latexprog)"
		#:args (path) path))

(combine-latex (main))


