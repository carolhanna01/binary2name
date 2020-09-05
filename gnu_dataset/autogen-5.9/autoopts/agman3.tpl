{+ AutoGen5 template  -*- nroff -*-

## agman3.tpl -- Template for command line man pages
##
##  AutoOpts copyright 1992-2006 Bruce Korb
##
## Time-stamp:      "2005-04-10 09:46:57 bkorb"
## Author:          Bruce Korb <bkorb@gnu.org>
## Maintainer:      Bruce Korb <bkorb@gnu.org>
## Created:         Mon Jun 28 15:35:12 1999
##              by: bkorb
## ---------------------------------------------------------------------
## $Id: agman3.tpl,v 4.4 2006/03/31 19:52:10 bkorb Exp $
## ---------------------------------------------------------------------

null

(setenv "SHELL" "/bin/sh")

+}{+

(if (exist? "see-also")
    (define see-also (string-append (get "see-also") " "))
    (define see-also "")  )

+}{+

FOR export_func     +}{+
  (if (not (exist? "private"))
      (set! see-also (string-append see-also
            (get "name") "(3) " ))  )
  +}{+

ENDFOR export_func  +}{+


FOR export_func                +}{+
  IF (not (exist? "private"))  +}{+

    (out-push-new (string-append
         (get "name") ".3" ))

+}.TH {+name+} 3 {+ `date +%Y-%m-%d` +} "" "Programmer's Manual"
{+

;; The following "dne" argument is a string of 5 characters:
;; '.' '\\' '"' and two spaces.  It _is_ hard to read. "
;;
(dne ".\\\"  ")

+}
.\"
.SH NAME
{+name+} - {+what+}
.sp 1
.SH SYNOPSIS
{+IF (exist? "header") +}
#include <\fI{+header+}\fP>
.br{+
  ENDIF+}
cc [...] -o outfile infile.c -l{+library+} [...]
.sp 1
{+ ?% ret-type "%s" void
+} \fB{+name+}\fP({+
  IF (not (exist? "arg")) +}void{+
  ELSE  +}{+
    FOR arg ", " +}{+arg-type+} \fI{+arg-name+}\fP{+
    ENDFOR arg +}{+
  ENDIF +});
.sp 1
.SH DESCRIPTION
{+
  INCLUDE "agman-lib.tpl"
+}{+
(get "doc")    +}{+
  IF (exist? "arg") +}{+
    FOR arg         +}
.TP
.IR {+ arg-name +}
{+ arg-desc  +}{+

    ENDFOR  arg     +}{+
  ENDIF  arg exists +}{+

  IF (exist? "ret-type") +}
.sp 1
.SH RETURN VALUE
{+ret-desc+}{+

  ENDIF +}{+

  IF (exist? "err") +}
.sp 1
.SH ERRORS
{+ err +}{+

  ENDIF +}{+

  IF (exist? "example") +}
.sp 1
.SH EXAMPLES
.nf
.in +5
{+ example +}
.in -5
.fi{+

  ENDIF +}{+

emit-man-text

+}
.SH SEE ALSO
The \fIinfo\fP documentation for the \fI-l{+library+}\fP library.
.br
{+
(define tmp-txt (get "see"))
(if (> (string-length see-also) 0)
    (set! tmp-txt (string-append see-also ", " tmp-txt))  )

(shellf "echo '%s' | \
sed 's@%s(3) @@;s/3) $/3)/;s/(3) /(3), /g;s/, *,/,/g;s/^, *//'"
    tmp-txt (get "name")) +}
{+

    (out-pop)    +}{+

  ENDIF private  +}{+

ENDFOR  export_func


+}
