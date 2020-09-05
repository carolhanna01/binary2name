[= AutoGen5 Template Library -*- Mode: Text -*-

# $Id: optlib.tpl,v 4.19 2006/11/20 00:22:49 bkorb Exp $

# Automated Options copyright 1992-2006 Bruce Korb

# Time-stamp:      "2006-10-29 07:07:27 bkorb"

=][=

(define get-opt-value (lambda (val)
        (if (<= val 32) val (+ val 96))  ))

(define have-proc   #f)
(define proc-name   "")
(define test-name   "")
(define tmp-text    "")
(define is-extern   #t)
(define is-priv     #t)
(define make-callback-procs #f)

(define need-stacking (lambda()
    (if (not (exist? "max"))
        #f
        (if (> (string->number (get "max")) 1)
            #t
            #f
)   )   ))

;;; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  =][=

DEFINE save-name-morphs

   Save the various flag name morphs into hash tables

   Every option descriptor has a pointer to a handler procedure.  That
   pointer may be NULL.  We generate a procedure for keyword,
   set-membership and range checked options.  "optionStackArg" is called
   if "stack-arg" is specified.  The specified procedure is called if
   "call-proc" is specified.  Finally, we insert the specified code for
   options with "flag-code" or "extract-code" attributes.

   This all changes, however, if "make-test-main" is set.  It is set if
   either "test-main" is specified as a program/global attribute, or if
   the TEST_MAIN environment variable is defined.  This should be set
   if either the program is intended to digest options for an incorporating
   shell script, or else if the user wants a quick program to show off the
   usage text and command line parsing.  For that environment, all callbacks
   are disabled except "optionStackArg" for stacked arguments and the
   keyword set membership options.

 =][=

  IF

    (set-flag-names)
    (hash-create-handle! ifdef-ed flg-name
              (and do-ifdefs (or (exist? "ifdef") (exist? "ifndef")))  )
    (set! proc-name (string-append "doOpt" cap-name))
    (set! is-priv   #t)

    (exist? "call-proc")

    =][=

    (set! have-proc #t)
    (set! is-extern #t)
    (set! is-priv   #f)
    (set! proc-name (get "call-proc"))
    (set! test-name (if need-stacking "optionStackArg" "NULL"))

  =][=
  ELIF (or (exist? "extract-code")
           (exist? "flag-code")
           (exist? "arg-range"))
    =][=

    (set! have-proc #t)
    (set! is-extern #f)
    (set! test-name (if (exist? "arg-range") proc-name
                        (if need-stacking "optionStackArg" "NULL")  ))

  =][=
  ELIF (exist? "flag-proc")     =][=

    (set! have-proc #t)
    (set! is-priv   #f)
    (set! proc-name (string-append "doOpt" (cap-c-name "flag-proc")))
    (set! test-name (if need-stacking "optionStackArg" "NULL"))
    (set! is-extern #f)

  =][=
  ELIF (exist? "stack-arg")     =][=

    (set! have-proc #t)
    (set! is-priv   #f)
    (set! proc-name "optionStackArg")
    (set! test-name (if need-stacking proc-name "NULL"))
    (set! is-extern #t)

  =][=
  ELIF (exist? "unstack-arg")   =][=

    (set! have-proc #t)
    (set! is-priv   #f)
    (set! proc-name "optionUnstackArg")
    (set! test-name (if need-stacking proc-name "NULL"))
    (set! is-extern #t)

  =][=
  ELSE =][=
    CASE arg-type               =][=
    =*   bool                   =][=
         (set! proc-name "optionBooleanVal")
         (set! test-name proc-name)
         (set! is-extern #t)
         (set! is-priv   #f)
         (set! have-proc #t)    =][=

    =*   num                    =][=
         (set! proc-name "optionNumericVal")
         (set! test-name proc-name)
         (set! is-extern #t)
         (set! is-priv   #f)
         (set! have-proc #t)    =][=

    ~*   key|set                =][=
         (set! test-name proc-name)
         (set! is-extern #f)
         (set! have-proc #t)    =][=

    ~*   hier|nest              =][=
         (set! proc-name "optionNestedVal")
         (set! test-name proc-name)
         (set! is-extern #t)
         (set! is-priv   #f)
         (set! have-proc #t)    =][=

    *                           =][=
         (set! have-proc #f)    =][=
    ESAC                        =][=

  ENDIF =][=

  ;;  If these are different, then a #define name is inserted into the
  ;;  option descriptor table.  Never a need to mess with it if we are
  ;;  not building a "test main" procedure.
  ;;
  (if (not make-test-main)
      (set! test-name proc-name))

  (if have-proc
      (begin
        (hash-create-handle! have-cb-procs   flg-name #t)
        (hash-create-handle! cb-proc-name    flg-name proc-name)
        (hash-create-handle! test-proc-name  flg-name test-name)
        (hash-create-handle! is-ext-cb-proc  flg-name is-extern)
        (set! make-callback-procs #t)
      )
      (begin
        (hash-create-handle! have-cb-procs   flg-name #f)
        (hash-create-handle! cb-proc-name    flg-name "NULL")
        (hash-create-handle! test-proc-name  flg-name "NULL")
      )
  )

  (if (exist? "default")
      (set! default-opt-index (for-index)) )

=][=

ENDDEF save-name-morphs

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Emit the "#define SET_OPT_NAME ..." and "#define DISABLE_OPT_NAME ..."  =][=

DEFINE set-defines

=]
#define SET_[=(. opt-name)=][= (if (exist? "arg-type") "(a)")
  =]   STMTS( \
        [=set-desc=].optActualIndex = [=(for-index)=]; \
        [=set-desc=].optActualValue = VALUE_[=(. opt-name)=]; \
        [=set-desc=].fOptState &= OPTST_PERSISTENT_MASK; \
        [=set-desc=].fOptState |= [=opt-state=][=
  CASE  arg-type =][=
  =*  str        =]; \
        [=set-desc=].optArg.argString = (a)[=
  =*  num        =]; \
        [=set-desc=].optArg.argInt = (a)[=
  =*  bool       =]; \
        [=set-desc=].optArg.argBool = (a)[=
  =*  key        =]; \
        [=set-desc=].optArg.argEnum = (a)[=
  =*  set        =]; \
        [=set-desc=].optArg.argIntptr = (a)[=
  ESAC arg-type  =][=

  IF (hash-ref have-cb-procs flg-name) =]; \
        (*([=(. descriptor)=].pOptProc))( &[=(. pname)=]Options, \
                [=(. pname)=]Options.pOptDesc + [=set-index=] );[=
  ENDIF "callout procedure exists" =] )[=

  IF (exist? "disable") =]
#define DISABLE_[=(. opt-name)=]   STMTS( \
        [=set-desc=].fOptState &= OPTST_PERSISTENT_MASK; \
        [=set-desc=].fOptState |= OPTST_SET | OPTST_DISABLED; \
        [=set-desc=].optArg.argString = NULL[=
    IF (hash-ref have-cb-procs flg-name) =]; \
        (*([=(. descriptor)=].pOptProc))( &[=(. pname)=]Options, \
                [=(. pname)=]Options.pOptDesc + [=set-index=] );[=
    ENDIF "callout procedure exists" =] )[=

  ENDIF disable exists =][=

ENDDEF set-defines

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Emit the copyright comment  =][=

DEFINE Option_Copyright =]
/*
 *  This file was produced by an AutoOpts template.  AutoOpts is a
 *  copyrighted work.  This [=
 (if (= "h" (suffix)) "header" "source") =] file is not encumbered by AutoOpts
 *  licensing, but is provided under the licensing terms chosen by the
 *  [= prog-name =] author or copyright holder.  AutoOpts is licensed under
 *  the terms of the LGPL.  The redistributable library (``libopts'') is
 *  licensed under the terms of either the LGPL or, at the users discretion,
 *  the BSD license.  See the AutoOpts and/or libopts sources for details.[=

IF (exist? "copyright") =]
 *
 * This source file is copyrighted and licensed under the following terms:
 *
 * [=(sprintf "%s copyright %s %s - all rights reserved"
     prog-name (get "copyright.date") (get "copyright.owner") ) =][=

  CASE (get "copyright.type") =][=

    =  gpl  =]
 *
[=(gpl  prog-name " * " ) =][=

    = lgpl  =]
 *
[=(lgpl prog-name (get "copyright.owner") " * " ) =][=

    =  bsd  =]
 *
[=(bsd  prog-name (get "copyright.owner") " * " ) =][=

    = note  =]
 *
[=(prefix " * " (get "copyright.text"))=][=

    *       =] * <<indeterminate license type>>[=

  ESAC =][=
ENDIF "copyright exists" =]
 */
[=

ENDDEF Option_Copyright

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Emit the #define's for a single option  =][=

DEFINE Option_Defines             =][=
  (define value-desc (string-append UP-prefix "DESC("
          (if (exist? "equivalence")
              (up-c-name "equivalence")
              UP-name) ")" ))     =][=

  IF (hash-ref ifdef-ed flg-name) =]
#if[=ifndef "n"=]def [= ifdef =][= ifndef =][=
  ENDIF =][=

  CASE (get "arg-type")           =][=

  =*   key                        =]
typedef enum {[=
    (if (not (exist? "arg-default"))
        (string-append " " enum-pfx "UNDEFINED = 0,")) =]
[=(shellf
"for f in %s ; do echo %s${f} ; done | ${CLexe} -I4 --spread=3 --sep=,"
          (string-upcase! (string->c-name! (join " " (stack "keyword"))))
    enum-pfx )=]
} te_[=(string-append Cap-prefix cap-name)=];[=

  =*   set                        =][=
    (define setmember-fmt (string-append "\n#define %-24s 0x%0"
       (shellf "expr '(' %d + 4 ')' / 4" (count "keyword")) "XUL"
       (if (> (count "keyword") 32) "L" "")  ))
    (define full-prefix (string-append UP-prefix UP-name) )  =][=

    FOR    keyword                =][=

      (sprintf setmember-fmt
         (string->c-name! (string-append
             full-prefix "_" (string-upcase! (get "keyword")) ))
         (ash 1 (for-index))  ) =][=

    ENDFOR keyword                =][=

    (sprintf setmember-fmt (string->c-name! (string-append
             full-prefix "_MEMBERSHIP_MASK"))
             (- (ash 1 (count "keyword")) 1) )  =][=

  ESAC  (get "arg-type")

=]
#define VALUE_[=
  (set! tmp-val (for-index))
  (sprintf "%-18s" opt-name)=] [=

  CASE  value    =][=
  !E             =][= (get-opt-value tmp-val) =][=
  ==  "'"        =]'\''[=
  ==  "\\"       =]'\\'[=
  ~~  "[ -~]"    =]'[=value=]'[=

  =*  num        =][=
      (if (>= number-opt-index 0)
          (error "only one number option is allowed")  )
      (set! number-opt-index tmp-val)
      (get-opt-value tmp-val)   =][=

  *              =][=(error (sprintf
    "Error:  value for opt %s is `%s'\nmust be single char or 'NUMBER'"
    (get "name") (get "value")))=][=

  ESAC           =][=

  CASE arg-type  =][=

  =*  num        =]
#define [=(. OPT-pfx)=]VALUE_[=(sprintf "%-14s" UP-name)
                 =] ([=(. value-desc)=].optArg.argInt)[=

  =*  key        =]
#define [= (sprintf "%-24s" (string-append OPT-pfx UP-name "_VAL2STR(_v)"))
                 =] optionKeywordName( &[=(. value-desc)=], (_v))
#define [=(. OPT-pfx)=]VALUE_[=(sprintf "%-14s" UP-name)
                 =] ([=(. value-desc)=].optArg.argEnum)[=

  =*  set        =]
#define [=(sprintf "%sVALUE_%-14s ((uintptr_t)%s.optCookie)"
                   OPT-pfx UP-name value-desc)
                 =][=

  =*  bool       =]
#define [=(. OPT-pfx)=]VALUE_[=(sprintf "%-14s" UP-name)
                 =] ([=(. value-desc)=].optArg.argBool)[=

  ESAC           =][=


  IF (== (up-c-name "equivalence") UP-name) =]
#define WHICH_[=(sprintf "%-18s" opt-name)
                 =] ([=(. descriptor)=].optActualValue)
#define WHICH_[=(. UP-prefix)=]IDX_[=(sprintf "%-14s" UP-name)
                 =] ([=(. descriptor)=].optActualIndex)[=
  ENDIF          =][=
  IF (exist? "settable") =][=

    IF (exist? "unstack-arg") =][=

      set-defines
           set-desc  = (string-append UP-prefix "DESC("
                           (up-c-name "unstack-arg") ")" )
           set-index = (index-name "unstack-arg")
           opt-state = "OPTST_SET | OPTST_EQUIVALENCE" =][=

    ELIF (and (exist? "equivalence")
              (not (== (up-c-name "equivalence") UP-name))) =][=

      set-defines
           set-desc  = (string-append UP-prefix "DESC("
                           (up-c-name "equivalence") ")" )
           set-index = (index-name "equivalence")
           opt-state = "OPTST_SET | OPTST_EQUIVALENCE" =][=

    ELSE  "is equivalenced"       =][=

      set-defines
           set-desc  = (string-append UP-prefix "DESC(" UP-name ")" )
           set-index = (for-index)
           opt-state = OPTST_SET  =][=

    ENDIF is/not equivalenced     =][=

  ENDIF settable                  =][=
  IF (hash-ref ifdef-ed flg-name) =]
#endif /* [= ifdef =][= ifndef =] */[=
  ENDIF =][=

ENDDEF Option_Defines

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Define the arrays of values associated with an option (strings, etc.) =][=

DEFINE   emit-nondoc-option     =][=
  #
  #  This is *NOT* a documentation option: =]
tSCC    z[= (sprintf "%-25s" (string-append cap-name
                    "_NAME[]" )) =] = "[=(. UP-name)=]";[=

  #  IF this option can be disabled,
  #  THEN we must create the string for the disabled version
  #  =][=
  IF (> (len "disable") 0) =]
tSCC    [=

    (hash-create-handle! disable-name   flg-name (string-append
        "zNot" cap-name "_Name" ))
    (hash-create-handle! disable-prefix flg-name (string-append
        "zNot" cap-name "_Pfx" ))

    (sprintf "zNot%-23s" (string-append cap-name "_Name[]")) =]= "[=

       (string-tr! (string-append (get "disable") "-" flg-name)
                   optname-from optname-to) =]";
tSCC    [= (sprintf "zNot%-23s" (string-append cap-name "_Pfx[]"))
             =]= "[=(string-downcase! (get "disable"))=]";[=


      #  See if we can use a substring for the option name:
      #  =][=
      IF (> (len "enable") 0) =]
tSCC    [=(sprintf "z%-26s" (string-append cap-name "_Name[]")) =]= "[=
        (string-tr! (string-append (get "enable") "-" flg-name)
                    optname-from optname-to) =]";[=
      ELSE =]
#define [=(sprintf "z%-27s " (string-append cap-name
        "_Name")) =](zNot[= (. cap-name) =]_Name + [=
        (+ (string-length (get "disable")) 1 ) =])[=
      ENDIF =][=


    ELSE  No disablement of this option:
    =][=
    (hash-create-handle! disable-name   flg-name "NULL")
    (hash-create-handle! disable-prefix flg-name "NULL") ""
  =]
tSCC    z[=    (sprintf "%-26s" (string-append cap-name "_Name[]"))
             =]= "[= (string-tr! (string-append
        (if (exist? "enable") (string-append (get "enable") "-") "")
        (get "name"))   optname-from optname-to) =]";[=

    ENDIF (> (len "disable") 0) =][=

    #  Check for special attributes:  a default value
    #  and conflicting or required options
    =][=
    IF (define def-arg-name (sprintf "z%-27s "
                 (string-append cap-name "DefaultArg" )))
       (define def-arg-array (sprintf "z%-27s "
                 (string-append cap-name "DefaultArg[]" )))
       (exist? "arg-default")   =][=
       CASE arg-type            =][=
       =* num                   =]
#define [=(. def-arg-name)=]((char const*)[= arg-default =])[=

       =* bool                  =][=
          CASE arg-default      =][=
          ~ n.*|f.*|0           =]
#define [=(. def-arg-name)=]((char const*)AG_FALSE)[=
          *                     =]
#define [=(. def-arg-name)=]((char const*)AG_TRUE)[=
          ESAC                  =][=

       =* key                   =]
#define [=(. def-arg-name)=]((char const*)[=
          (emit (if (=* (get "arg-default") enum-pfx) "" enum-pfx))
          (up-c-name "arg-default") =])[=

       =* set                   =]
#define [=(. def-arg-name)=]NULL
#define [=(sprintf "%-28s " (string-append cap-name "CookieBits"))=](void*)([=
         IF (not (exist? "arg-default")) =]0[=
         ELSE =][=
           FOR    arg-default | =][=
             (string->c-name! (string-append UP-prefix UP-name "_"
                   (string-upcase! (get "arg-default")) ))  =][=
           ENDFOR arg-default   =][=
         ENDIF =])[=

       =* str                   =]
tSCC    [=(. def-arg-array)=]= [=(kr-string (get "arg-default"))=];[=

       *                        =][=
          (error (string-append cap-name
                 " has arg-default, but no valid arg-type"))  =][=
       ESAC                     =][=
    ENDIF                       =][=


    IF (exist? "flags-must") =]
static const int
    a[=(. cap-name)=]MustList[] = {[=
      FOR flags-must =]
    [= (index-name "flags-must") =],[=
      ENDFOR flags_must =] NO_EQUIVALENT };[=
    ENDIF =][=


    IF (exist? "flags-cant") =]
static const int
    a[=(. cap-name)=]CantList[] = {[=
      FOR flags-cant =]
    [= (index-name "flags-cant") =],[=
      ENDFOR flags-cant =] NO_EQUIVALENT };[=
    ENDIF =]
#define [=(. UP-name)=]_FLAGS       ([=
         ? enabled      "OPTST_INITENABLED"
                        "OPTST_DISABLED"       =][=
         stack-arg      " | OPTST_STACKED"     =][=
         must-set       " | OPTST_MUST_SET"    =][=
         no-preset      " | OPTST_NO_INIT"     =][=

         CASE immediate =][=
         =    also      =] | OPTST_IMM | OPTST_TWICE[=
         +E             =] | OPTST_IMM[=
         ESAC immediate =][=

         CASE immed-disable  =][=
         =    also           =] | OPTST_DISABLE_IMM | OPTST_DISABLE_TWICE[=
         +E                  =] | OPTST_DISABLE_IMM[=
         ESAC immed-disable  =][=

         IF (exist? "arg-type")                =][=
            CASE arg-type  =][=
            =*  num        =] \
        | OPTST_SET_ARGTYPE(OPARG_TYPE_NUMERIC)[=

            =*  bool       =] \
        | OPTST_SET_ARGTYPE(OPARG_TYPE_BOOLEAN)[=

            =*  key        =] \
        | OPTST_SET_ARGTYPE(OPARG_TYPE_ENUMERATION)[=

            =*  set        =] \
        | OPTST_SET_ARGTYPE(OPARG_TYPE_MEMBERSHIP)[=

            ~*  hier|nest  =] \
        | OPTST_SET_ARGTYPE(OPARG_TYPE_HIERARCHY)[=

            =*  str        =] \
        | OPTST_SET_ARGTYPE(OPARG_TYPE_STRING)[=

            *              =][=
            (error (string-append "unknown arg type '"
                   (get "arg-type") "' for " flg-name)) =][=
            ESAC arg-type  =][=
            (if (exist? "arg-optional") " | OPTST_ARG_OPTIONAL") =][=
         ENDIF arg-type exists  =])[=

ENDDEF   emit-nondoc-option

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Define the arrays of values associated with an option (strings, etc.) =][=

DEFINE   opt-strs

=]
/*
 *  [=(set-flag-names) cap-name=] option description[=
  IF (or (exist? "flags_must") (exist? "flags_cant")) =] with
 *  "Must also have options" and "Incompatible options"[=
  ENDIF =]:
 */[=
  IF (hash-ref ifdef-ed flg-name) =]
#if[=ifndef "n"=]def [= ifdef =][= ifndef =][=
  ENDIF  ifdef-ed                 =]
tSCC    z[=(. cap-name)=]Text[] =
        [=(set! tmp-text (kr-string (get "descrip")))  tmp-text=];[=

  IF (exist? "documentation")     =]
#define [=(. UP-name)=]_FLAGS       (OPTST_DOCUMENT | OPTST_NO_INIT)[=
  ELSE  NOT a doc option:         =][=
     emit-nondoc-option           =][=
  ENDIF  (exist? "documentation") =][=

  IF (hash-ref ifdef-ed flg-name) =]

#else   /* disable [= (. cap-name)=] */
#define [=(string-append VALUE-pfx UP-name)=] NO_EQUIVALENT
#define [=(. UP-name)=]_FLAGS       (OPTST_OMITTED | OPTST_NO_INIT)[=

    IF (exist? "arg-default") =]
#define z[=(. cap-name)=]DefaultArg NULL[=
    ENDIF =][=

    IF (exist? "flags-must")  =]
#define a[=(. cap-name)=]MustList   NULL[=
    ENDIF =][=

    IF (exist? "flags-cant")  =]
#define a[=(. cap-name)=]CantList   NULL[=
    ENDIF =]
#define z[=(. cap-name)=]Text       NULL
#define z[=(. cap-name)=]_NAME      NULL
#define z[=(. cap-name)=]_Name      NULL[=
    IF (> (len "disable") 0) =]
#define zNot[=(. cap-name)=]_Name   NULL
#define zNot[=(. cap-name)=]_Pfx    NULL[=
    ENDIF =]
#endif  /* [= ifdef =][= ifndef =] */[=
  ENDIF  ifdef-ed   =][=

ENDDEF opt-strs

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Define the arrays of values associated with help/version/etc. =][=

DEFINE   help-strs

=]

/*
 *  Help/More_Help[= version "/Version"=] option descriptions:
 */
tSCC zHelpText[]       = "Display usage information and exit";
tSCC zHelp_Name[]      = "help";

tSCC zMore_HelpText[]  = "Extended usage information passed thru pager";
tSCC zMore_Help_Name[] = "more-help";[=

  IF (exist? "version")

=]

tSCC zVersionText[]    = "Output version information and exit";
tSCC zVersion_Name[]   = "version";[=

  ENDIF (exist? "version")  =][=

  IF (exist? "homerc")

=]

/*
 *  Save/Load_Opts option description:
 */
tSCC zSave_OptsText[]     = "Save the option state to a config file";
tSCC zSave_Opts_Name[]    = "save-opts";

tSCC zLoad_OptsText[]     = "Load options from a config file";
tSCC zLoad_Opts_NAME[]    = "LOAD_OPTS";

tSCC zNotLoad_Opts_Name[] = "no-load-opts";
tSCC zNotLoad_Opts_Pfx[]  = "no";
#define zLoad_Opts_Name   (zNotLoad_Opts_Name + 3)[=

  ENDIF (exist? "homerc") =][=

ENDDEF   help-strs

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Define the values for an option descriptor   =][=

DEFINE opt-desc         =][=
  IF
     (set-flag-names)

     (exist? "documentation")

=]
  {  /* entry idx, value */ 0, 0,
     /* equiv idx, value */ 0, 0,
     /* equivalenced to  */ NO_EQUIVALENT,
     /* min, max, act ct */ 0, 0, 0,
     /* opt state flags  */ [=(. UP-name)=]_FLAGS, 0,
     /* last opt argumnt */ { NULL },
     /* arg list/cookie  */ NULL,
     /* must/cannot opts */ NULL, NULL,
     /* option proc      */ [=
         IF   (exist? "call-proc")        =][=call-proc=][=
         ELIF (or (exist? "extract-code")
                  (exist? "flag-code"))   =]doOpt[=(. cap-name)=][=
         ELSE                             =]NULL[=
         ENDIF =],
     /* desc, NAME, name */ z[=(. cap-name)=]Text, NULL, NULL,
     /* disablement strs */ NULL, NULL },[=

  ELSE

=]
  {  /* entry idx, value */ [=(for-index)=], [=
                              (string-append VALUE-pfx UP-name)=],
     /* equiv idx, value */ [=
          IF (== (up-c-name "equivalence") UP-name)
              =]NO_EQUIVALENT, 0[=
          ELIF (or (exist? "equivalence") (exist? "unstack-arg"))
              =]NOLIMIT, NOLIMIT[=
          ELSE
              =][=(for-index)=], [=(string-append VALUE-pfx UP-name)=][=
          ENDIF=],
     /* equivalenced to  */ [=
         (if (exist? "unstack-arg")
             (index-name "unstack-arg")
             (if (and (exist? "equivalence")
                      (not (== (up-c-name "equivalence") UP-name)) )
                 (index-name "equivalence")
                 "NO_EQUIVALENT"
         )   ) =],
     /* min, max, act ct */ [=
         (if (exist? "min") (get "min")
             (if (exist? "must-set") "1" "0" )) =], [=
         (if (=* (get "arg-type") "set") "NOLIMIT"
             (if (exist? "max") (get "max") "1") ) =], 0,
     /* opt state flags  */ [=(. UP-name)=]_FLAGS, 0,
     /* last opt argumnt */ { [=
         IF (exist? "arg-default")
              =]z[=(. cap-name)=]DefaultArg[=
         ELSE =]NULL[= ENDIF =] },
     /* arg list/cookie  */ [=
            (if (and (=* (get "arg-type") "set") (exist? "arg-default"))
                (string-append cap-name "CookieBits") "NULL") =],
     /* must/cannot opts */ [=
         (if (exist? "flags-must")
             (string-append "a" cap-name "MustList, ")
             "NULL, " ) =][=
         (if (exist? "flags-cant")
             (string-append "a" cap-name "CantList")
             "NULL" ) =],
     /* option proc      */ [=

     ;;  If there is a difference between what gets invoked under test and
     ;;  what gets invoked "normally", then there must be a #define name
     ;;  for the procedure.  There will only be such a difference if
     ;;  make-test-main is #t
     ;;
     (if (= (hash-ref cb-proc-name   flg-name)
            (hash-ref test-proc-name flg-name))

         (hash-ref test-proc-name flg-name)
         (string-append UP-name "_OPT_PROC")  )  =],
     /* desc, NAME, name */ [=
     (sprintf "z%1$sText, z%1$s_NAME, z%1$s_Name," cap-name) =]
     /* disablement strs */ [=(hash-ref disable-name   flg-name)=], [=
                              (hash-ref disable-prefix flg-name)=] },[=
  ENDIF =][=

ENDDEF opt-desc

=]
