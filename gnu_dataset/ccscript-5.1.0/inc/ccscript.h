// Copyright (C) 2008-2014 David Sugar, Tycho Softworks.
// Copyright (C) 2015 Cherokees of Idaho.
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

#ifndef _CCSCRIPT_H_
#define _CCSCRIPT_H_

#ifndef UCOMMON_UCOMMON_H_
#include <ucommon/ucommon.h>
#endif

namespace ucommon {

/**
 * Compiled script container.  This class holds the image of a
 * reference counted instance of a compiled script.
 * @author David Sugar <dyfet@gnutelephony.org>
 */
class __EXPORT Script : public CountedObject, public memalloc
{
public:
    class interp;
    class header;
    class checks;

    /**
     * A type for runtime script method invokation.
     */
    typedef bool (Script::interp::*method_t)(void);

    /**
     * Basic compiled statement.  This is a linked list of statement
     * lines, with an array of parsed statement arguments.  An optional
     * dsp resource mask value can be assigned by a check method on a
     * per-line basis as well as at closure time for a script.
     */
    typedef struct line {
        struct line *next;
        union {
            const char *cmd;
            header *sub;
        };
        char **argv;
        unsigned short loop, argc;
        unsigned lnum, mask;
        method_t method;
    } line_t;

    /**
     * A type for compile-time command verification method invokation.
     */
    typedef const char *(*check_t)(Script *img, Script::header *scr, Script::line_t *line);

    /**NAMESPACE_UCOMMON
     * A structure to introduce new core commands to the runtime engine.
     * This is typically passed to assign as an array.  Entries with no
     * runtime method (method = NULL) are only processed at compile-time.
     */
    typedef struct keyword
    {
        const char *name;           /**< name of command */
        method_t method;    /**< runtime method or NULL if c-t only */
        check_t check;              /**< compile-time check routine */
        struct keyword *next;       /**< linked list set by assign() */
    } keyword_t;

    /**
     * Contains defined variables found by scope when strict is used.
     * This is used as a per-script scope list of defined variable objects.
     * This is constructed during check routines and used to validate compile-
     * time symbol references per-argument per-statement.
     * @author David Sugar <dyfet@gnutelephony.org>
     */
    class __EXPORT strict : public LinkedObject
    {
    private:
        __DELETE_DEFAULTS(strict);

    public:
        const char *id;

        static bool find(Script *img, header *scr, const char *id);
        static void createVar(Script *img, header *scr, const char *id);
        static void createSym(Script *img, header *scr, const char *id);
        static void createAny(Script *img, header *scr, const char *id);
        static void createGlobal(Script *img, const char *id);

        void put(FILE *fp, const char *header);
    };

    /**
     * Contains instance of a runtime symbol.  Symbols may be read-only or
     * read-write.  Symbols that refer to data stored elsewhere are
     * considered references, and references are used to pass arguments into
     * defined functions.
     * @author David Sugar <dyfet@gnutelephony.org>
     */
    class __EXPORT symbol : public LinkedObject
    {
    private:
        __DELETE_DEFAULTS(symbol);

    public:
        const char *name;   /**< name of symbol */
        char *data;         /**< content of symbol */
        unsigned size;      /**< size of data buffer or 0 if const */
        header *scope;      /**< scope of symbol definition */
    };

    /**
     * An event block for a script.  Each script can have one or more named
     * event chains.
     * @author David Sugar <dyfet@gnutelephony.org>
     */
    class __EXPORT event : public LinkedObject
    {
    private:
        __DELETE_DEFAULTS(event);

    public:
        line_t *first;
        const char *name;
    };

    /**
     * Convenience typedef to allow use of event name.
     */
    typedef event event_t;

    /**
     * Header describes a script section.  A section might be a named
     * label or a defined function.  Each section has a list of one or
     * more statement lines, and can have events.  Defined sections may
     * also track per-script scoped variables in strict compile mode.
     * Script headers may also have resource masks assigned at closure.
     * @author David Sugar <dyfet@gnutelephony.org>
     */
    class __EXPORT header : public LinkedObject
    {
    private:
        __DELETE_DEFAULTS(header);

    public:
        LinkedObject *scoped;   /**< scoped symbol defs */
        LinkedObject *events;   /**< named events */
        LinkedObject *methods;  /**< named members */
        line_t *first;          /**< first line of section or define */
        const char *file;       /**< filename of script */
        const char *name;       /**< name of script section or define */
        unsigned resmask;       /**< post-compile processing resource mask */

        /**
         * Used to set linked list linkage.
         * @param scr header to link with.
         */
        inline void link(header *scr)
            {Next = scr;}
    };

    /**
     * A container class for compile-time check methods.  Check methods
     * are defined for each script statement.  A derived application
     * server would use this as a base class for the check methods of
     * it's own extensions.
     * @author David Sugar <dyfet@gnutelephony.org>
     */
    class __EXPORT checks
    {
    private:
        __DELETE_DEFAULTS(checks);

    public:
        static bool isValue(const char *text);
        static bool isText(const char *text);

        static const char *chkPush(Script *img, header *scr, line_t *line);
        static const char *chkApply(Script *img, header *scr, line_t *line);
        static const char *chkIgnore(Script *img, header *scr, line_t *line);
        static const char *chkNop(Script *img, header *scr, line_t *line);
        static const char *chkExit(Script *img, header *scr, line_t *line);
        static const char *chkVar(Script *img, header *scr, line_t *line);
        static const char *chkConst(Script *img, header *scr, line_t *line);
        static const char *chkSet(Script *img, header *scr, line_t *line);
        static const char *chkClear(Script *img, header *scr, line_t *line);
        static const char *chkError(Script *img, header *scr, line_t *line);
        static const char *chkPack(Script *img, header *scr, line_t *line);
        static const char *chkExpand(Script *img, header *scr, line_t *line);
        static const char *chkGosub(Script *img, header *src, line_t *line);
        static const char *chkGoto(Script *img, header *scr, line_t *line);
        static const char *chkDo(Script *img, header *scr, line_t *line);
        static const char *chkUntil(Script *img, header *scr, line_t *line);
        static const char *chkWhile(Script *ing, header *scr, line_t *line);
        static const char *chkConditional(Script *img, header *scr, line_t *line);
        static const char *chkContinue(Script *img, header *scr, line_t *line);
        static const char *chkBreak(Script *img, header *scr, line_t *line);
        static const char *chkLoop(Script *img, header *scr, line_t *line);
        static const char *chkPrevious(Script *img, header *scr, line_t *line);
        static const char *chkIndex(Script *img, header *scr, line_t *line);
        static const char *chkForeach(Script *img, header *scr, line_t *line);
        static const char *chkCase(Script *img, header *scr, line_t *line);
        static const char *chkEndcase(Script *img, header *scr, line_t *line);
        static const char *chkOtherwise(Script *img, header *scr, line_t *line);
        static const char *chkIf(Script *img, header *scr, line_t *line);
        static const char *chkElif(Script *img, header *scr, line_t *line);
        static const char *chkElse(Script *img, header *scr, line_t *line);
        static const char *chkEndif(Script *img, header *scr, line_t *line);
        static const char *chkDefine(Script *img, header *scr, line_t *line);
        static const char *chkInvoke(Script *img, header *scr, line_t *line);
        static const char *chkWhen(Script *img, header *scr, line_t *line);
        static const char *chkStrict(Script *img, header *scr, line_t *line);
        static const char *chkExpr(Script *img, header *scr, line_t *line);
        static const char *chkRef(Script *img, header *scr, line_t *line);
        static const char *chkIgnmask(Script *img, header *scr, line_t *line);
    };

    /**
     * Runtime stack for each interpreter instance.  This is used to
     * manage loop and case blocks, as well as subroutine calls.
     */
    typedef struct {
        header *scr;            /**< executing script for stack */
        header *scope;          /**< effective symbol scope */
        event_t *event;         /**< so we don't redo our event */
        line_t *line;           /**< executing line at stack level */
        line_t *ignore;         /**< ignored events */
        unsigned short index;   /**< index marker for loops */
        unsigned short base;    /**< base stack of "@section" */
        unsigned short resmask; /**< effective dsp resource mask */
    } stack_t;

    /**
     * An instance of the runtime interpreter.  Some application servers,
     * like GNU Bayonne, may create an interpreter instance for each
     * telephone call session.  All runtime execution happens through
     * the interpreter class, which executes compiled script images.  This
     * is commonly used as a base class for runtime methods and application
     * specific interpreters.  All interpreter instance data and symbols
     * are allocated off the interpreter instance private heap.
     * @author David Sugar <dyfet@gnutelephony.org>
     */
    class __EXPORT interp : protected memalloc
    {
    public:
        typedef char num_t[16];

        interp();
        virtual ~interp();

        /**
         * Step through an instance of the interpreter.  This can step
         * through multiple lines at once, depending on Script::stepping.
         * @return true if still running, false if exited.
         */
        bool step(void);

        /**
         * Attach a compiled image to the interpreter and start.  Even
         * when different entry points are called, the initialization
         * block is always called.
         * @param image to attach (ref count).
         * @param entry point, NULL for "@main"
         * @return true ifi successful, false if failed to find entry.
         */
        bool attach(Script *image, const char *entry = NULL);

        /**
         * Cleanup after interpreter run.  Releases reference to image.
         */
        void detach(void);

        /**
         * Used to initialize and purge the interpreter between runs.
         * This is used especially if the same interpreter object is kept
         * in memory and directly re-used for multiple executions.
         */
        void initialize(void);

        /**
         * Invoke runtime interpreter error handling.
         * @param text to post into %error symbol.
         */
        bool error(const char *text);

        /**
         * Get current dsp resource mask.
         * @return resource mask.
         */
        unsigned getResource(void);

        /**
         * Get effective filename of base.
         * @return filename.
         */
        const char *getFilename(void);

    protected:
        symbol *find(const char *id);
        void skip(void);
        void push(void);
        bool trylabel(const char *id);
        bool tryexit(void);
        void pullScope(void);
        void pullBase(void);
        void pullLoop(void);
        bool pop(void);
        void setStack(header *scr, event *ev = NULL);

        /**
         * Try to branch to a named event handler.  If successful, the
         * interpreter transfers control to the start of the handler
         * for the next step.
         * @param name of event to request.
         * @return true if found.
         */
        bool scriptEvent(const char *name);

        /**
         * Search for an event object in the method table.
         * @param name to search for.
         * @return method if found.
         */
        event *scriptMethod(const char *name);

        stack_t *stack;
        object_pointer<Script> image;
        LinkedObject **syms;
        unsigned frame;

        char *getTemp(void);
        bool setConst(const char *id, const char *value);
        symbol *createSymbol(const char *id);
        symbol *getVar(const char *id, const char *value = NULL);
        const char *getValue(void);
        const char *getContent(void);
        const char *getContent(const char *text);
        const char *getKeyword(const char *id);
        method_t getLooping(void);
        bool isConditional(unsigned index);
        void setRef(header *scope, const char *id, char *data, unsigned size);
        void getParams(header *scope, line_t *line);
        void startScript(header *scr);

        virtual unsigned getTypesize(const char *type_id);
        virtual const char *getTypeinit(const char *type_id);
        virtual const char *getFormat(symbol *sym, const char *id, char *temp);
        virtual bool getCondition(const char *test, const char *value);
        const char *getIndex(void);

    private:
        __DELETE_COPY(interp);

        bool getExpression(unsigned index);

        char *errmsg;
        char *temps[3];
        unsigned tempindex;
    };

    /**
     * A class to collect compile-time errors.  These are collected as the
     * script is compiled, and can then be examined.  This allows one to
     * push errors into an alternate logging facility.
     * @author David Sugar <dyfet@gnutelephony.org>
     */
    class __EXPORT error : public OrderedObject
    {
    private:
        friend class Script;

        __DELETE_DEFAULTS(error);

        error(Script *img, unsigned line, const char *str);

    public:
        const char *filename;
        char *errmsg;
        unsigned errline;
    };

    /**
     * Runtime methods collection class.  This is used to collect the
     * runtime implimentation of each script command.  Often derived
     * application servers will use sideway inheritance of something
     * derived from interp to collect implimentations for convenience.
     * @author David Sugar <dyfet@gnutelephony.org>
     */
    class __EXPORT methods : public interp
    {
    private:
        __DELETE_DEFAULTS(methods);

    public:
        bool scrPush(void);
        bool scrApply(void);
        bool scrExpr(void);
        bool scrVar(void);
        bool scrSet(void);
        bool scrAdd(void);
        bool scrClear(void);
        bool scrConst(void);
        bool scrPause(void);
        bool scrNop(void);
        bool scrPack(void);
        bool scrExpand(void);
        bool scrExit(void);
        bool scrReturn(void);
        bool scrError(void);
        bool scrRestart(void);
        bool scrGosub(void);
        bool scrGoto(void);
        bool scrDo(void);
        bool scrLoop(void);
        bool scrUntil(void);
        bool scrWhile(void);
        bool scrBreak(void);
        bool scrContinue(void);
        bool scrForeach(void);
        bool scrPrevious(void);
        bool scrRepeat(void);
        bool scrIndex(void);
        bool scrCase(void);
        bool scrEndcase(void);
        bool scrOtherwise(void);
        bool scrIf(void);
        bool scrElif(void);
        bool scrElse(void);
        bool scrEndif(void);
        bool scrDefine(void);
        bool scrInvoke(void);
        bool scrWhen(void);
        bool scrRef(void);
        bool scrIgnore(void);
    };

    ~Script();

    static unsigned stepping;   /**< default stepping increment */
    static unsigned indexing;   /**< default symbol indexing */
    static size_t paging;       /**< default heap paging */
    static unsigned sizing;     /**< default symbol size */
    static unsigned stacking;   /**< stack frames in script runtime */
    static unsigned decimals;   /**< default decimal places */

    LinkedObject *scheduler;    /**< scheduler list */

    /**
     * Compiled a file into an existing image.  A shared config script can be
     * used that holds common definitions.  Multiple script files can also be
     * merged together into a final image.
     * @param merge with prior compiled script.
     * @param filename to compile.
     * @param config image of script with common definitions.
     * @return compiled script object if successful.
     */
    static Script *compile(Script *merge, const char *filename, Script *config = NULL);

    /**
     * Assign new keywords from extensions and derived service.  Must
     * be called before any use.
     * @param list of keywords to add to engine.
     */
    static void assign(keyword_t *list);

    /**
     * Find a keyword from internal command table.  This includes the
     * core runtime engine keywords set through init() and any derived
     * ones added through assign.
     * @param id of command to find.
     * @return keyword object for the command or NULL.
     */
    static keyword_t *find(const char *id);

    /**
     * Initialize entire script engine.  Must be called first and once.
     */
    static void init(void);

    static unsigned offset(const char *list, unsigned index);
    static void copy(const char *list, char *item, unsigned size);
    static unsigned count(const char *list);
    static const char *get(const char *list, unsigned offset);
    static char *get(char *list, unsigned offset);
    static header *find(Script *img, const char *id);
    static bool isEvent(header *scr, const char *id);

    header *first;
    LinkedObject **scripts;

    bool push(line_t *line);
    method_t pull(void);
    method_t looping(void);

    inline unsigned getErrors(void)
        {return errors;}

    inline LinkedObject *getListing(void)
        {return errlist.begin();}

    inline const char *getFilename(void)
        {return filename;}

    inline bool isStrict(void)
        {return global != NULL;}

    inline unsigned getLines(void)
        {return lines;}

private:
    friend class strict;
    friend class checks;
    friend class error;
    friend class interp;
    friend class methods;

    __DELETE_COPY(Script);

    Script();

    void errlog(unsigned line, const char *fmt, ...);

    unsigned long instance;
    unsigned errors;
    unsigned loop;
    unsigned lines;
    bool thencheck;
    line_t **stack;
    LinkedObject *global;
    OrderedIndex errlist;
    object_pointer<Script> shared;
    const char *filename;
    unsigned serial;
    LinkedObject *headers;
};

} // namespace ucommon

#endif

