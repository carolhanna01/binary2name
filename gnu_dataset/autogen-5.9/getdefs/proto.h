/* -*- buffer-read-only: t -*- vi: set ro:
 *
 * Prototypes for getdefs
 * Generated Sat Feb 17 12:50:08 PST 2007
 */
#ifndef GETDEFS_PROTO_H_GUARD
#define GETDEFS_PROTO_H_GUARD 1
#ifndef LOCAL
#  define LOCAL extern
#  define REDEF_LOCAL 1
#else
#  undef  REDEF_LOCAL
#endif
/*\n *  Extracted from gdemit.c\n */
LOCAL char*
emitDefinition( char* pzDef, char* pzOut );

/*\n *  Extracted from gdinit.c\n */
LOCAL void
processEmbeddedOptions( char* pzText );

LOCAL void
validateOptions( void );

/*\n *  Extracted from getdefs.c\n */
LOCAL char*
loadFile( tCC* pzFname );

#ifdef REDEF_LOCAL
#  undef LOCAL
#  define LOCAL
#endif
#endif /* GETDEFS_PROTO_H_GUARD */
