[= Autogen5 Template -*- Mode: scheme -*-

#  List the output suffixes that are to be generated.
#  Header must come first.  An env variable is set that
#  is used in processing the C file.
#
#  $Id: options.tpl,v 4.15 2007/02/04 17:44:12 bkorb Exp $
# Time-stamp:      "2007-01-03 19:12:00 bkorb"

h
c

# This file contains the templates used to generate the
# option descriptions for client programs, and it declares
# the macros used in the templates.

# Automated Options copyright 1992-2007 Bruce Korb

(define have-cb-procs     (make-hash-table 31))
(define is-ext-cb-proc    (make-hash-table 31))
(define cb-proc-name      (make-hash-table 31))
(define test-proc-name    (make-hash-table 31))
(define disable-name      (make-hash-table 31))
(define disable-prefix    (make-hash-table 31))
(define ifdef-ed          (make-hash-table 31))
(define tmp-ct            0)

(define extract-fmt       "\n/* extracted from %s near line %d */\n")

(setenv "SHELL" "/bin/sh")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# --  COPYRIGHT  --

# Automated Options is free software.
# You may redistribute it and/or modify it under the terms of the
# GNU General Public License, as published by the Free Software
# Foundation; either version 2, or (at your option) any later version.

# Automated Options is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with Automated Options.  See the file "COPYING".  If not,
# write to:  The Free Software Foundation, Inc.,
#            51 Franklin Street, Fifth Floor,
#            Boston, MA  02110-1301, USA.

# As a special exception, Bruce Korb gives permission for additional
# uses of the text contained in his release of AutoOpts.

# The exception is that, if you link the AutoOpts library with other
# files to produce an executable, this does not by itself cause the
# resulting executable to be covered by the GNU General Public License.
# Your use of that executable is in no way restricted on account of
# linking the AutoOpts library code into it.

# This exception does not however invalidate any other reasons why
# the executable file might be covered by the GNU General Public License.

# This exception applies only to the code released by Bruce Korb under
# the name AutoOpts.  If you copy code from other sources under the
# General Public License into a copy of AutoOpts, as the General Public
# License permits, the exception does not apply to the code that you add
# in this way.  To avoid misleading anyone as to the status of such
# modified files, you must delete this exception notice from them.

# If you write modifications of your own for AutoOpts, it is your choice
# whether to permit this exception to apply to your modifications.
# If you do not wish that, delete this exception notice.

# --  END COPYRIGHT  --

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

=][=
 (if (= "h" (suffix))
     (begin
          (if (not (exist? "flag.name"))
              (error "No options have been defined" ))

          (if (> (count "flag") 100)
              (error (sprintf "%d options are too many - limit of 100"
                          (count "flag")) ))

          (if (not (and (exist? "prog-name") (exist? "prog-title")))
              (error "prog-name and prog-title are required"))
          (define prog-name (get "prog-name"))
          (if (> (string-length prog-name) 16)
              (error (sprintf "prog-name limited to 16 characters:  %s"
                              prog-name)) )
     )

     (if (exist? "library") (out-delete))
 )

 (dne " *  " "/*  ") =]
 *
 * Generated from AutoOpts 28:0:3 templates.
 */[=

  include "optlib"   =]
[= INVOKE Option_Copyright =][=

CASE    (suffix)     =][=

== h                 =][=

  (define do-ifdefs (or (exist? "flag.ifdef") (exist? "flag.ifndef")))

  ;; IF    long options are disallowed
  ;;   AND at least one flag character (value) is supplied
  ;; THEN every option must have a 'value' attribute
  ;;
  (define must-have-values
          (and (not (exist? "long-opts")) (exist? "flag.value")))

  (if (and (exist? "reorder-args") (not (exist? "argument")) )
    (error
      "Reordering arguments requires operands (the 'argument' attribute)"))

  (if (and must-have-values (exist? "flag.disable"))
      (error "options can be disabled only with a long option name"))

  (if (exist? "flag.extract-code")
      (shellf "f=%s.c ; test -s $f && mv -f $f $f.save"
              (base-name)))

  (if (and (exist? "usage") (exist? "gnu-usage"))
      (error "'usage' and 'gnu-usage' conflict." ))

  (if (> (count "flag.default") 1)
      (error "Too many default options"))

  (if (exist? "library") (begin
      (if (not (exist? "flag[0].documentation"))
          (error "The first option of a library must be a documentation option"))
      (if (not (exist? "flag[0].lib-name"))
          (error "The first option of a library must specify 'lib-name'"))
      (if (< 1 (count "flag.lib-name"))
          (error "a library must only have one 'flag.lib-name'"))
  )   )

  ;;  Establish a number of variations on the spelling of the
  ;;  program name.  Use these Scheme defined values throughout.
  ;;
  (define pname           (string->c-name!    (get "prog-name")))
  (define pname-cap       (string-capitalize  pname))
  (define pname-up        (string-upcase      pname))
  (define pname-down      (string-downcase    pname))
  (define main-guard      (string-append "TEST_" pname-up "_OPTS" ))
  (define number-opt-index  -1)
  (define default-opt-index -1)
  (define make-test-main  (if (exist? "test-main") #t
                              (string? (getenv "TEST_MAIN")) ))

  (define descriptor "")
  (define opt-name   "")
  (define flg-name   "")
  (define UP-name    "")
  (define cap-name   "")
  (define low-name   "")
  (define tmp-val    "")
  (define enum-pfx   "")
  (define added-hdr  "")

  (define set-flag-names (lambda () (begin
      (set! flg-name (get "name"))
      (set! UP-name  (up-c-name "name"))
      (set! cap-name (string-capitalize UP-name ))
      (set! low-name (string-downcase   UP-name ))
      (set! enum-pfx (if (exist? ".prefix-enum")
                         (up-c-name (string-append (get "prefix-enum") "_"))
                         (string-append UP-prefix UP-name "_") ))
  ) ) )

  (if (exist? "prefix")
   (begin
     (define UP-prefix  (string-append (string-upcase! (get "prefix")) "_"))
     (define Cap-prefix (string-capitalize UP-prefix))
     (define OPT-pfx    (string-append UP-prefix "OPT_"))
     (define INDEX-pfx  (string-append "INDEX_" OPT-pfx))
     (define VALUE-pfx  (string-append "VALUE_" OPT-pfx))
   )
   (begin
     (define UP-prefix  "")
     (define Cap-prefix "")
     (define OPT-pfx    "OPT_")
     (define INDEX-pfx  "INDEX_OPT_")
     (define VALUE-pfx  "VALUE_OPT_")
   )  )

   (define up-c-name  (lambda (ag-name)
      (string-upcase! (string->c-name! (get ag-name)))  ))

   (define cap-c-name  (lambda (ag-name)
      (string-capitalize! (string->c-name! (get ag-name)))  ))

   (define index-name (lambda (i-name)
      (string-append INDEX-pfx (up-c-name i-name))  ))

   (if (exist? "preserve-case")
      (begin
        (define optname-from "_^")
        (define optname-to   "--")
      )
      (begin
        (define optname-from "A-Z_^")
        (define optname-to   "a-z--")
   )  )

   (define version-text (string-append prog-name
           (if (exist? "package")
               (string-append " (" (get "package") ")")
               "" )
           " - " (get "prog-title")
           (if (exist? "version")
               (string-append " - Ver. " (get "version"))
               "" ) ))

   (define lib-opt-ptr "")

  =][=
  FOR flag           =][=

    (if (> (len "name") 32)
        (error (sprintf "Option %d name exceeds 32 characters: %s"
                       (for-index) (get "name")) ))

    (if (< 1 (count "value"))
        (error (sprintf "Option %s has too many `value's" (get "name"))))

    (if (and must-have-values
             (not (exist? "documentation"))
             (not (exist? "value")))
        (error (sprintf "Option %s needs a `value' attribute" (get "name"))))

    (set! tmp-val
           (+ (if (exist? "call-proc")    1 0)
              (if (exist? "flag-code")    1 0)
              (if (exist? "extract-code") 1 0)
              (if (exist? "flag-proc")    1 0)
              (if (exist? "unstack-arg")  1 0)
              (if (exist? "stack-arg")    1 0)  ))

    ;;  IF there is one of the above callback proc types AND there is an
    ;;     option argument of type non-string, THEN oops.  Conflict.
    ;;
    (if (and (> tmp-val 0) (exist? "arg-type")
             (not (=* (get "arg-type") "str"))  )
        (error (sprintf
               "Option %s has a %s argument and a conflicting callback procedure"
               (get "name") (get "arg-type") )
    )   )

    ;;  Count up the ways a callback procedure was specified.  Must be 0 or 1
    ;;
    (if (< 1 (+ (if (exist? "arg-range") 1 0)
                (if (~* (get "arg-type") "key|set") 1 0) tmp-val))
       (error (sprintf "Option %s has multiple callback specifications"
                      (get "name")) ))

    (if (< 1 (+ (count "ifdef") (count "ifndef") ))
        (error (sprintf "Option %s has multiple 'ifdef-es'" (get "name") )) )

    (if (and (exist? "stack-arg") (not (exist? "arg-type")))
        (error (sprintf "Option %s has stacked args, but no arg-type"
                        (get "name"))))

    (if (and (exist? "min") (exist? "must-set"))
        (error (sprintf "Option %s has both 'min' and 'must-set' attributes"
                        (get "name"))))

    (if (exist? "lib-name")
        (set! lib-opt-ptr (string->c-name! (string-append
                          (get "lib-name") "_" (get "name") "_optDesc_p"))) )
  =][=
  ENDFOR flag        =][=

  `if [ -z "${CLexe}" ] ; then
    CLexe="\`type columns 2>/dev/null\`"
    if [ $? -ne 0 ]
    then echo failure
      CLexe=false
    else
      CLexe="\`echo ${CLexe}|sed 's,.* ,,'\`"
  fi ; fi
  if ${CLexe} -v > /dev/null ; then : ; else
    exec 1>&2
    echo 'CLexe is' ${CLexe}
    echo 'PATH  is' ${PATH}
    echo 'pwd   is' \`pwd\`
    die "Cannot find a working 'columns' program"
  fi` =][=

  include "opthead"  =][= # create the option header

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # =][=

== c  =][=

   (if (exist? "library") (out-delete))  =][=

   include "optcode" =][= ;; create the option source code

   (if (exist? "flag.extract-code")
       (shellf "test -f %1$s.c && rm -f %1$s.c.save" (base-name)))  =][=

ESAC =]
/* [= (out-name) =] ends here */[=

# options.tpl ends here =]
