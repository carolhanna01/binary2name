
/*
 *  $Id: loadPseudo.c,v 4.11 2007/02/04 17:44:12 bkorb Exp $
 *
 *  Time-stamp:        "2007-01-29 16:35:59 bkorb"
 *  Last Committed:    $Date: 2007/02/04 17:44:12 $
 *
 *  This module processes the "pseudo" macro
 */

/*
 *  AutoGen copyright 1992-2007 Bruce Korb
 *
 *  AutoGen is free software.
 *  You may redistribute it and/or modify it under the terms of the
 *  GNU General Public License, as published by the Free Software
 *  Foundation; either version 2, or (at your option) any later version.
 *
 *  AutoGen is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with AutoGen.  See the file "COPYING".  If not,
 *  write to:  The Free Software Foundation, Inc.,
 *             51 Franklin Street, Fifth Floor,
 *             Boston, MA  02110-1301, USA.
 */

/*
 *  loadPseudoMacro
 *
 *  Find the start and end macro markers.  In btween we must find the
 *  "autogen" and "template" keywords, followed by any suffix specs.
 */
#define DEFINE_FSM
#include "pseudo-fsm.h"

tSCC zAgName[] = "autogen5";
tSCC zTpName[] = "template";

/* = = = START-STATIC-FORWARD = = = */
/* static forward declarations maintained by :mkfwd */
static tCC*
doSchemeExpr( tCC* pzData, tCC* pzFileName );

static te_pm_event
findTokenType( tCC**  ppzData, te_pm_state fsm_state );

static tCC*
copyMarker( tCC* pzData, char* pzMark, size_t * pCt );
/* = = = END-STATIC-FORWARD = = = */


/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *
 *  doSchemeExpr
 *
 *  Process a scheme specification
 */
static tCC*
doSchemeExpr( tCC* pzData, tCC* pzFileName )
{
    char*   pzEnd = (char*)pzData + strlen( pzData );
    char    ch;
    tMacro* pCM = pCurMacro;
    tMacro  mac = { (teFuncType)~0, 0, 0, 0, 0, 0, 0, NULL };

    mac.lineNo  = templLineNo;
    pzEnd       = (char*)skipScheme( pzData, pzEnd );
    ch          = *pzEnd;
    *pzEnd      = NUL;
    pCurMacro   = &mac;

    ag_scm_c_eval_string_from_file_line(
          (char*)pzData, pzFileName, templLineNo );

    pCurMacro = pCM;
    *pzEnd    = ch;
    while (pzData < pzEnd)
        if (*(pzData++) == '\n')
            templLineNo++;
    return (tCC*)pzEnd;
}


/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *
 *  skipSuffixSpec
 *
 *  Process a suffix specification
 */
LOCAL tCC*
doSuffixSpec( tCC* pzData, tCC* pzFileName, int lineNo )
{
    /*
     *  The following is the complete list of POSIX required-to-be-legal
     *  file name characters.  These are the only characters we allow to
     *  appear in a suffix.  We do, however, add '=' and '%' because we
     *  also allow a format specification to follow the suffix,
     *  separated by an '=' character.
     */
    tSCC zSuffixSpecChars[] = "=%" "abcdefghijklmnopqrstuvwxyz"
          "ABCDEFGHIJKLMNOPQRSTUVWXYZ" "0123456789" "-_./";

    tOutSpec*  pOS;
    tCC*       pzSfxFmt;
    tCC*       pzResult;
    static tOutSpec**  ppOSList = &pOutSpecList;

    /*
     *  Skip over the suffix construct
     */
    size_t spn = strspn( pzData, zSuffixSpecChars+2 );

    if (pzData[spn] != '=') {
        pzSfxFmt = NULL;
    } else {
        pzSfxFmt = pzData + spn;
        if (pzSfxFmt[1] == '(') {
            tCC *pe  = pzSfxFmt + 1 + strlen( pzSfxFmt + 1 );
            tCC *ptr = skipScheme( pzSfxFmt+1, pe );
            spn = ptr - pzData;
        } else {
            spn += strspn( pzSfxFmt, zSuffixSpecChars );
        }
    }
    pzResult = pzData + spn;

    /*
     *  If pzFileName is NULL, then we are called by --select-suffix.
     *  Otherwise, the suffix construct is saved only for the main template,
     *  and only when the --select-suffix option was not specified.
     */
    if (  (pzFileName != NULL)
       && (  (procState != PROC_STATE_LOAD_TPL)
          || HAVE_OPT( SELECT_SUFFIX )))
        return pzResult;

    /*
     *  Allocate the suffix structure
     */
    pOS = (tOutSpec*)AGALOC( sizeof( *pOS ) + (size_t)spn + 1,
                             "Output Specification" );

    /*
     *  Link it into the global list
     */
    *ppOSList  = pOS;
    ppOSList   = &pOS->pNext;
    pOS->pNext = NULL;

    /*
     *  Copy the data into the suffix field from our input buffer.
     *  IF the suffix contains its own formatting construct,
     *  THEN split it off from the suffix and set the formatting ptr.
     *  ELSE supply a default.
     */
    memcpy( pOS->zSuffix, pzData, spn );
    pOS->zSuffix[ spn ] = NUL;

    if (pzSfxFmt != NULL) {
        spn = pzSfxFmt - pzData;
        pOS->zSuffix[spn] = NUL;
        pzSfxFmt = pOS->zSuffix + spn + 1;

        switch (*pzSfxFmt) {
        case NUL:
        {
            tSCC zFileFmt3[] = "%s";
            pOS->pzFileFmt = zFileFmt3;
            break;
        }

        case '(':
        {
            SCM str =
                ag_scm_c_eval_string_from_file_line(
                    pzSfxFmt, pzFileName, lineNo );
            tCC *pzStr;
            AGDUPSTR( pzStr, resolveSCM( str ), "Scheme suffix format");
            pOS->pzFileFmt = pzStr;
            break;
        }

        default:
            pOS->pzFileFmt = pzSfxFmt;
            break;
        }

    } else {
        tSCC zFileFmt1[] = "%s.%s";
        tSCC zFileFmt2[] = "%s%s";

        /*
         *  IF the suffix does not start with punctuation,
         *  THEN we will insert a '.' of our own.
         */
        if (isalnum( pOS->zSuffix[0] ))
             pOS->pzFileFmt = zFileFmt1;
        else pOS->pzFileFmt = zFileFmt2;
    }

    return pzResult;
}


/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *
 *  findTokenType
 *
 *  Skiping leading white space, figure out what sort of token is under
 *  the scan pointer (pzData).
 */
static te_pm_event
findTokenType( tCC**  ppzData, te_pm_state fsm_state )
{
    tCC* pzData = *ppzData;

    /*
     *  At the start of processing in this function, we can never be at
     *  the "start of a line".  A '#' type comment before the initial
     *  start macro marker is illegal.  Otherwise, our scan pointer is
     *  after some valid token, which won't be the start of a line, either.
     */
    ag_bool line_start = AG_FALSE;

 skipWhiteSpace:
    while (isspace( *pzData )) {
        if (*(pzData++) == '\n') {
            line_start = AG_TRUE;
            templLineNo++;

            /*
             *  IF we are done with the macro markers,
             *  THEN we skip white space only thru the first new line.
             */
            if (fsm_state == PM_ST_END_MARK) {
                *ppzData = pzData;
                return PM_EV_END_PSEUDO;
            }
        }
    }

    if (line_start && (*pzData == '#')) {
        pzData = strchr( pzData, '\n' );
        if (pzData == NULL)
            AG_ABEND( "Invalid template file" );
        goto skipWhiteSpace;
    }

    *ppzData = pzData; /* in case we return */

    /*
     *  After the end marker has been found,
     *  anything else is really the start of the data.
     */
    if (fsm_state == PM_ST_END_MARK)
        return PM_EV_END_PSEUDO;

    /*
     *  IF the token starts with an alphanumeric,
     *  THEN it must be "autogen5" or "template" or a suffix specification
     */
    if (isalnum( *pzData )) {
        if (strneqvcmp( pzData, zAgName, (int)sizeof(zAgName)-1 ) == 0) {
            if (isspace( pzData[ sizeof(zAgName)-1 ] )) {
                *ppzData = pzData + sizeof(zAgName);
                return PM_EV_AUTOGEN;
            }

            return PM_EV_SUFFIX;
        }

        if (strneqvcmp( pzData, zTpName, (int)sizeof(zTpName)-1 ) == 0) {
            if (isspace( pzData[ sizeof(zTpName)-1 ] )) {
                *ppzData = pzData + sizeof(zTpName)-1;
                return PM_EV_TEMPLATE;
            }

            return PM_EV_SUFFIX;
        }

        return PM_EV_SUFFIX;
    }

    /*
     *  Several transition tokens are enabled once
     *  the "template" keyword has been processed.
     */
    if (fsm_state == PM_ST_TEMPL) {
        switch (*pzData) {
        case '-':
            if ((pzData[1] == '*') && (pzData[2] == '-'))
                return PM_EV_ED_MODE;
            /* FALLTHROUGH */

        case '.':
        case '_':
            return PM_EV_SUFFIX;

        case '(':
            return PM_EV_SCHEME;
        }
    }

    /*
     *  IF the next sequence is "-*-"
     *  THEN we found an edit mode marker
     */
    if (strncmp(pzData, "-*-", (size_t)3) == 0)
        return PM_EV_ED_MODE;

    /*
     *  IF it is some other punctuation,
     *  THEN it must be a start/end marker.
     */
    if (ispunct( *pzData ))
        return PM_EV_MARKER;

    /*
     *  Otherwise, it is just junk.
     */
    AG_ABEND( "Invalid template file" );
    /* NOTREACHED */
    return PM_EV_INVALID;
}


/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *
 *  copyMarker
 *
 *  Some sort of marker is under the scan pointer.  Copy it for as long
 *  as we find punctuation characters.
 */
static tCC*
copyMarker( tCC* pzData, char* pzMark, size_t * pCt )
{
    int ct = 0;

    for (;;) {
        char ch = *pzData;
        if (! ispunct( ch ))
            break;
        *(pzMark++) = ch;
        if (++ct >= sizeof( zStartMac ))
            return NULL;

        pzData++;
    }

    *pCt = ct;
    *pzMark = NUL;
    return pzData;
}


/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *
 *  loadPseudoMacro
 *
 *  Using a finite state machine, scan over the tokens that make up the
 *  "pseudo macro" at the start of every template.
 */
LOCAL tCC*
loadPseudoMacro( tCC* pzData, tCC* pzFileName )
{
    tSCC zMarkErr[] = "start/end macro mark too long";
    tSCC zBadMark[] = "bad template marker in %s on line %d:\n\t%s";
    tCC* pzBadness;
#   define BAD_MARKER( t ) { pzBadness = t; goto abort_load; }

    te_pm_state fsm_state  = PM_ST_INIT;

    templLineNo  = 1;

    while (fsm_state != PM_ST_DONE) {
        te_pm_event fsm_tkn = findTokenType( &pzData, fsm_state );
        te_pm_state nxt_state;
        te_pm_trans trans;

        nxt_state  = pm_trans_table[ fsm_state ][ fsm_tkn ].next_state;
        trans      = pm_trans_table[ fsm_state ][ fsm_tkn ].transition;

        /*
         *  There are only so many "PM_TR_<state-name>_<token-name>"
         *  transitions that are legal.  See which one we got.
         *  It is legal to alter "nxt_state" while processing these.
         */
        switch (trans) {
        case PM_TR_SKIP_ED_MODE:
        {
            char* pzEnd = strstr( pzData + 3, "-*-" );
            char* pzNL  = strchr( pzData + 3, '\n' );
            if ((pzEnd == NULL) || (pzNL < pzEnd))
                BAD_MARKER( "invalid edit mode marker" );

            pzData = pzEnd + 3;
            break;
        }

        case PM_TR_INIT_MARKER:
            pzData = copyMarker( pzData, zStartMac, &startMacLen );
            if (pzData == NULL)
                BAD_MARKER( zMarkErr );

            break;

        case PM_TR_TEMPL_MARKER:
            pzData = copyMarker( pzData, zEndMac, &endMacLen );
            if (pzData == NULL)
                BAD_MARKER( zMarkErr );

            /*
             *  IF the end macro seems to end with the start macro and
             *  it is exactly twice as long as the start macro, then
             *  presume that someone ran the two markers together.
             */
            if (  (endMacLen == 2 * startMacLen)
               && (strcmp( zStartMac, zEndMac + startMacLen ) == 0))  {
                pzData -= startMacLen;
                zEndMac[ startMacLen ] = NUL;
                endMacLen = startMacLen;
            }

            if (strstr( zEndMac, zStartMac ) != NULL)
                BAD_MARKER( "start marker contained in end marker" );
            if (strstr( zStartMac, zEndMac ) != NULL)
                BAD_MARKER( "end marker contained in start marker" );
            break;

        case PM_TR_TEMPL_SUFFIX:
            pzData = doSuffixSpec( pzData, pzFileName, templLineNo );
            break;

        case PM_TR_TEMPL_SCHEME:
            pzData = doSchemeExpr( pzData, pzFileName );
            break;

        case PM_TR_INVALID:
            pm_invalid_transition( fsm_state, fsm_tkn );
            switch (fsm_state) {
            case PM_ST_INIT:     BAD_MARKER( "need start marker" );
            case PM_ST_ST_MARK:  BAD_MARKER( "need autogen5 marker" );
            case PM_ST_AGEN:     BAD_MARKER( "need template marker" );
            case PM_ST_TEMPL:    BAD_MARKER( "need end marker" );
            case PM_ST_END_MARK: BAD_MARKER( "need end of line" );
            default:             BAD_MARKER( "BROKEN FSM STATE" );
            }

        case PM_TR_NOOP:
            break;

        default:
            BAD_MARKER( "broken pseudo-macro FSM" );
        }

        fsm_state = nxt_state;
    }

    /*
     *  It is possible that the template writer specified a shell to use.
     *  If the server shell has not already started, we'll catch it later.
     *  If it has started, then check for a shell change & shut it down
     *  if it has been changed.
     */
    if (serverArgs[0] != NULL) {
        char* pz = getenv( zShellEnv );
        if ((pz != NULL) && (strcmp( pz, serverArgs[0] ) != 0)) {
            fprintf( pfTrace, "Changing server shell from %s to %s\n",
                     serverArgs[0], pz );
            closeServer();
            serverArgs[0] = pz;
            pzShellProgram = pz;
        }
    }

    return pzData;

 abort_load:
    AG_ABEND( aprf( zBadMark, pzFileName, templLineNo, pzBadness ));
#   undef BAD_MARKER
    return NULL;
}
/*
 * Local Variables:
 * mode: C
 * c-file-style: "stroustrup"
 * indent-tabs-mode: nil
 * End:
 * end of agen5/loadPseudo.c */
