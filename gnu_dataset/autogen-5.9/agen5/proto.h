/* -*- buffer-read-only: t -*- vi: set ro:
 *
 * Prototypes for agen5
 * Generated Sat Feb 17 12:50:05 PST 2007
 */
#ifndef AGEN5_PROTO_H_GUARD
#define AGEN5_PROTO_H_GUARD 1
#ifndef LOCAL
#  define LOCAL extern
#  define REDEF_LOCAL 1
#else
#  undef  REDEF_LOCAL
#endif
/*\n *  Extracted from agCgi.c\n */
LOCAL void
loadCgi( void );

/*\n *  Extracted from agInit.c\n */
LOCAL void
initialize( int arg_ct, char** arg_vec );

/*\n *  Extracted from agShell.c\n */
LOCAL void
closeServer( void );

LOCAL int
chainOpen( int       stdinFd,
           tCC**     ppArgs,
           pid_t*    pChild  );

LOCAL pid_t
openServer( tFdPair* pPair, tCC** ppArgs );

LOCAL pid_t
openServerFP( tpfPair* pfPair, tCC** ppArgs );

LOCAL char*
runShell( char const*  pzCmd );

/*\n *  Extracted from agUtils.c\n */
LOCAL char*
aprf( char const* pzFmt, ... );

LOCAL char*
mkstempPat( void );

LOCAL void
doOptions( int arg_ct, char** arg_vec );

LOCAL tCC*
getDefine( tCC* pzDefName, ag_bool check_env );

LOCAL char*
spanQuote( char* pzQte );

LOCAL tCC*
skipScheme( tCC* pzSrc,  tCC* pzEnd );

LOCAL tCC*
skipExpression( tCC* pzSrc, size_t len );

/*\n *  Extracted from autogen.c\n */
LOCAL void
ag_abend_at( tCC* pzMsg
#ifdef DEBUG_ENABLED
    , tCC* pzFile, int line
#endif
    );

LOCAL void *
ao_malloc (size_t sz);

LOCAL void *
ao_realloc (void *p, size_t sz);

LOCAL void
ao_free (void *p);

LOCAL char *
ao_strdup (char const * str);

/*\n *  Extracted from defDirect.c\n */
LOCAL char*
processDirective( char* pzScan );

/*\n *  Extracted from defFind.c\n */
LOCAL int
canonicalizeName( char* pzD, char const* pzS, int srcLen );

LOCAL tDefEntry*
findDefEntry( char* pzName, ag_bool* pIsIndexed );

LOCAL tDefEntry**
findEntryList( char* pzName );

/*\n *  Extracted from defLex.c\n */
LOCAL te_dp_event
yylex( void );

LOCAL void
yyerror( char* s );

/*\n *  Extracted from defLoad.c\n */
LOCAL void
freeEntry( tDefEntry* pDE );

LOCAL tDefEntry*
getEntry( void );

LOCAL tDefEntry*
findPlace( char* name, tCC* pzIndex );

LOCAL void
readDefines( void );

LOCAL void
unloadDefs( void );

/*\n *  Extracted from expGuile.c\n */
LOCAL teGuileType
gh_type_e( SCM typ );

LOCAL SCM
ag_scm_c_eval_string_from_file_line( tCC* pzExpr, tCC* pzFile, int line );

/*\n *  Extracted from expOutput.c\n */
LOCAL void
removeWriteAccess( int fd );

/*\n *  Extracted from expPrint.c\n */
LOCAL SCM
run_printf( char* pzFmt, int len, SCM alist );

/*\n *  Extracted from funcDef.c\n */
LOCAL void
parseMacroArgs( tTemplate* pT, tMacro* pMac );

/*\n *  Extracted from funcEval.c\n */
LOCAL tCC*
resolveSCM( SCM s );

LOCAL tCC*
evalExpression( ag_bool* pMustFree );

LOCAL SCM
eval( char const* pzExpr );

/*\n *  Extracted from loadPseudo.c\n */
LOCAL tCC*
doSuffixSpec( tCC* pzData, tCC* pzFileName, int lineNo );

LOCAL tCC*
loadPseudoMacro( tCC* pzData, tCC* pzFileName );

/*\n *  Extracted from tpLoad.c\n */
LOCAL tTemplate*
findTemplate( tCC* pzTemplName );

LOCAL tSuccess
findFile(tCC* pzFName, char* pzFullName, tCC** papSuffixList, tCC * pzReferrer);

LOCAL tTemplate*
loadTemplate(tCC* pzFileName, tCC * pzReferrer);

LOCAL void
unloadTemplate( tTemplate* pT );

LOCAL void
cleanup( tTemplate* pTF );

/*\n *  Extracted from tpParse.c\n */
LOCAL tMacro*
parseTemplate( tMacro* pM, tCC** ppzText );

/*\n *  Extracted from tpProcess.c\n */
LOCAL void
generateBlock( tTemplate*   pT,
               tMacro*      pMac,
               tMacro*      pEnd );

LOCAL void
processTemplate( tTemplate* pTF );

LOCAL void
closeOutput( ag_bool purge );

#ifdef REDEF_LOCAL
#  undef LOCAL
#  define LOCAL
#endif
#endif /* AGEN5_PROTO_H_GUARD */
