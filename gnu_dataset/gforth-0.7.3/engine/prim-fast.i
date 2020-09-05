LABEL(paren_docol) /* (docol) ( -- R:a_retaddr ) S1 -- S1  */
/* run-time routine for colon definitions */
NAME("(docol)")
{
DEF_CA
Cell * a_retaddr;
NEXT_P0;
#ifdef VM_DEBUG
if (vm_debug) {
}
#endif
rp += -1;
{
#line 157 "prim"
#ifdef NO_IP
a_retaddr = next_code;
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" a_retaddr=", vm_out); printarg_a_(a_retaddr);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_a_2Cell(a_retaddr,rp[0]);

goto **(Label *)PFA(CFA);
#else /* !defined(NO_IP) */
a_retaddr = (Cell *)IP;
SET_IP((Xt *)PFA(CFA));
#endif /* !defined(NO_IP) */
#line 34 "prim-fast.i"
}
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" a_retaddr=", vm_out); printarg_a_(a_retaddr);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_a_2Cell(a_retaddr,rp[0]);
LABEL2(paren_docol)
NEXT_P1_5;
LABEL3(paren_docol)
DO_GOTO;
}

LABEL(paren_docon) /* (docon) ( -- w ) S1 -- S1  */
/* run-time routine for constants */
NAME("(docon)")
{
DEF_CA
Cell w;
NEXT_P0;
sp[0] = spTOS;
#ifdef VM_DEBUG
if (vm_debug) {
}
#endif
sp += -1;
{
#line 168 "prim"
w = *(Cell *)PFA(CFA);
#ifdef NO_IP

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" w=", vm_out); printarg_w(w);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w,spTOS);

goto *next_code;
#endif /* defined(NO_IP) */
#line 81 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" w=", vm_out); printarg_w(w);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w,spTOS);
LABEL2(paren_docon)
NEXT_P1_5;
LABEL3(paren_docon)
DO_GOTO;
}

LABEL(paren_dovar) /* (dovar) ( -- a_body ) S1 -- S1  */
/* run-time routine for variables and CREATEd words */
NAME("(dovar)")
{
DEF_CA
Cell * a_body;
NEXT_P0;
sp[0] = spTOS;
#ifdef VM_DEBUG
if (vm_debug) {
}
#endif
sp += -1;
{
#line 176 "prim"
a_body = PFA(CFA);
#ifdef NO_IP

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" a_body=", vm_out); printarg_a_(a_body);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_a_2Cell(a_body,spTOS);

goto *next_code;
#endif /* defined(NO_IP) */
#line 127 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" a_body=", vm_out); printarg_a_(a_body);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_a_2Cell(a_body,spTOS);
LABEL2(paren_dovar)
NEXT_P1_5;
LABEL3(paren_dovar)
DO_GOTO;
}

LABEL(paren_douser) /* (douser) ( -- a_user ) S1 -- S1  */
/* run-time routine for constants */
NAME("(douser)")
{
DEF_CA
Cell * a_user;
NEXT_P0;
sp[0] = spTOS;
#ifdef VM_DEBUG
if (vm_debug) {
}
#endif
sp += -1;
{
#line 184 "prim"
a_user = (Cell *)(up+*(Cell *)PFA(CFA));
#ifdef NO_IP

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" a_user=", vm_out); printarg_a_(a_user);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_a_2Cell(a_user,spTOS);

goto *next_code;
#endif /* defined(NO_IP) */
#line 173 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" a_user=", vm_out); printarg_a_(a_user);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_a_2Cell(a_user,spTOS);
LABEL2(paren_douser)
NEXT_P1_5;
LABEL3(paren_douser)
DO_GOTO;
}

LABEL(paren_dodefer) /* (dodefer) ( -- ) S1 -- S1  */
/* run-time routine for deferred words */
NAME("(dodefer)")
{
DEF_CA
NEXT_P0;
#ifdef VM_DEBUG
if (vm_debug) {
}
#endif
{
#line 192 "prim"
#ifndef NO_IP
ip=IP; /* undo any ip updating that may have been performed by NEXT_P0 */
#endif /* !defined(NO_IP) */
SUPER_END; /* !! probably unnecessary and may lead to measurement errors */
VM_JUMP(EXEC1(*(Xt *)PFA(CFA)));
#line 207 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
LABEL2(paren_dodefer)
LABEL3(paren_dodefer)
DO_GOTO;
}

LABEL(paren_field) /* (dofield) ( n1 -- n2 ) S1 -- S1  */
/* run-time routine for fields */
NAME("(dofield)")
{
DEF_CA
MAYBE_UNUSED Cell n1;
Cell n2;
NEXT_P0;
vm_Cell2n(spTOS,n1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n1=", vm_out); printarg_n(n1);
}
#endif
{
#line 200 "prim"
n2 = n1 + *(Cell *)PFA(CFA);
#ifdef NO_IP

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n2=", vm_out); printarg_n(n2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n2,spTOS);

goto *next_code;
#endif /* defined(NO_IP) */
#line 250 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n2=", vm_out); printarg_n(n2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n2,spTOS);
LABEL2(paren_field)
NEXT_P1_5;
LABEL3(paren_field)
DO_GOTO;
}

LABEL(paren_doval) /* (dovalue) ( -- w ) S1 -- S1  */
/* run-time routine for constants */
NAME("(dovalue)")
{
DEF_CA
Cell w;
NEXT_P0;
sp[0] = spTOS;
#ifdef VM_DEBUG
if (vm_debug) {
}
#endif
sp += -1;
{
#line 208 "prim"
w = *(Cell *)PFA(CFA);
#ifdef NO_IP

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" w=", vm_out); printarg_w(w);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w,spTOS);

goto *next_code;
#endif /* defined(NO_IP) */
#line 296 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" w=", vm_out); printarg_w(w);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w,spTOS);
LABEL2(paren_doval)
NEXT_P1_5;
LABEL3(paren_doval)
DO_GOTO;
}

LABEL(paren_dodoes) /* (dodoes) ( -- a_body R:a_retaddr ) S1 -- S1  */
/* run-time routine for @code{does>}-defined words */
NAME("(dodoes)")
{
DEF_CA
Cell * a_body;
Cell * a_retaddr;
NEXT_P0;
sp[0] = spTOS;
#ifdef VM_DEBUG
if (vm_debug) {
}
#endif
sp += -1;
rp += -1;
{
#line 216 "prim"
#ifdef NO_IP
a_retaddr = next_code;
a_body = PFA(CFA);
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" a_body=", vm_out); printarg_a_(a_body);
fputs(" a_retaddr=", vm_out); printarg_a_(a_retaddr);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_a_2Cell(a_body,spTOS);
vm_a_2Cell(a_retaddr,rp[0]);

#ifdef DEBUG
fprintf(stderr, "dodoes to %x, push %x\n", a_retaddr, a_body);
#endif
goto **(Label *)DOES_CODE1(CFA);
#else /* !defined(NO_IP) */
a_retaddr = (Cell *)IP;
a_body = PFA(CFA);
#ifdef DEBUG
fprintf(stderr, "dodoes to %x, push %x\n", a_retaddr, a_body);
#endif
SET_IP(DOES_CODE1(CFA));
#endif /* !defined(NO_IP) */
#line 358 "prim-fast.i"
}
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" a_body=", vm_out); printarg_a_(a_body);
fputs(" a_retaddr=", vm_out); printarg_a_(a_retaddr);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_a_2Cell(a_body,spTOS);
vm_a_2Cell(a_retaddr,rp[0]);
LABEL2(paren_dodoes)
NEXT_P1_5;
LABEL3(paren_dodoes)
DO_GOTO;
}

LABEL(paren_does_handler) /* (does-handler) ( -- ) S1 -- S1  */
/* just a slot to have an encoding for the DOESJUMP, 
which is no longer used anyway (!! eliminate this) */
NAME("(does-handler)")
{
DEF_CA
NEXT_P0;
#ifdef VM_DEBUG
if (vm_debug) {
}
#endif
{
#line 236 "prim"
#line 391 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
LABEL2(paren_does_handler)
NEXT_P1_5;
LABEL3(paren_does_handler)
DO_GOTO;
}

GROUPADD(9)
GROUP( control, 9)
LABEL(noop) /* noop ( -- ) S1 -- S1  */
/*  */
NAME("noop")
{
DEF_CA
NEXT_P0;
#ifdef VM_DEBUG
if (vm_debug) {
}
#endif
{
#line 242 "prim"
#line 420 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
LABEL2(noop)
NEXT_P1_5;
LABEL3(noop)
DO_GOTO;
}

LABEL(call) /* call ( #a_callee -- R:a_retaddr ) S1 -- S1  */
/* Call callee (a variant of docol with inline argument). */
NAME("call")
{
DEF_CA
MAYBE_UNUSED Cell * a_callee;
Cell * a_retaddr;
NEXT_P0;
vm_Cell2a_(IMM_ARG(IPTOS,305397760 ),a_callee);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_callee=", vm_out); printarg_a_(a_callee);
}
#endif
INC_IP(1);
rp += -1;
{
#line 247 "prim"
#ifdef NO_IP
assert(0);
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" a_retaddr=", vm_out); printarg_a_(a_retaddr);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_a_2Cell(a_retaddr,rp[0]);

JUMP(a_callee);
#else
#ifdef DEBUG
    {
      CFA_TO_NAME((((Cell *)a_callee)-2));
      fprintf(stderr,"%08lx: call %08lx %.*s\n",(Cell)ip,(Cell)a_callee,
	      len,name);
    }
#endif
a_retaddr = (Cell *)IP;
SET_IP((Xt *)a_callee);
#endif
#line 478 "prim-fast.i"
}
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" a_retaddr=", vm_out); printarg_a_(a_retaddr);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_a_2Cell(a_retaddr,rp[0]);
LABEL2(call)
NEXT_P1_5;
LABEL3(call)
DO_GOTO;
}

LABEL(execute) /* execute ( xt -- ) S1 -- S1  */
/* Perform the semantics represented by the execution token, @i{xt}. */
NAME("execute")
{
DEF_CA
MAYBE_UNUSED Xt xt;
NEXT_P0;
vm_Cell2xt(spTOS,xt);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" xt=", vm_out); printarg_xt(xt);
}
#endif
sp += 1;
{
#line 265 "prim"
#ifdef DEBUG
fprintf(stderr, "execute %08x\n", xt);
#endif
#ifndef NO_IP
ip=IP;
#endif
SUPER_END;
VM_JUMP(EXEC1(xt));
#line 520 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
spTOS = sp[0];
LABEL2(execute)
LABEL3(execute)
DO_GOTO;
}

LABEL(perform) /* perform ( a_addr -- ) S1 -- S1  */
/* @code{@@ execute}. */
NAME("perform")
{
DEF_CA
MAYBE_UNUSED Cell * a_addr;
NEXT_P0;
vm_Cell2a_(spTOS,a_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_addr=", vm_out); printarg_a_(a_addr);
}
#endif
sp += 1;
{
#line 276 "prim"
/* and pfe */
#ifndef NO_IP
ip=IP;
#endif
SUPER_END;
VM_JUMP(EXEC1(*(Xt *)a_addr));
#line 556 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
spTOS = sp[0];
LABEL2(perform)
LABEL3(perform)
DO_GOTO;
}

LABEL(semis) /* ;s ( R:w -- ) S1 -- S1  */
/* The primitive compiled by @code{EXIT}. */
NAME(";s")
{
DEF_CA
MAYBE_UNUSED Cell w;
NEXT_P0;
vm_Cell2w(rp[0],w);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w=", vm_out); printarg_w(w);
}
#endif
rp += 1;
{
#line 287 "prim"
#ifdef NO_IP
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;

goto *(void *)w;
#else
SET_IP((Xt *)w);
#endif
#line 600 "prim-fast.i"
}
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
LABEL2(semis)
NEXT_P1_5;
LABEL3(semis)
DO_GOTO;
}

LABEL(unloop) /* unloop ( R:w1 R:w2 -- ) S1 -- S1  */
/*  */
NAME("unloop")
{
DEF_CA
MAYBE_UNUSED Cell w1;
MAYBE_UNUSED Cell w2;
NEXT_P0;
vm_Cell2w(rp[1],w1);
vm_Cell2w(rp[0],w2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w1=", vm_out); printarg_w(w1);
fputs(" w2=", vm_out); printarg_w(w2);
}
#endif
rp += 2;
{
#line 295 "prim"
/* !! alias for 2rdrop */
#line 636 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
LABEL2(unloop)
NEXT_P1_5;
LABEL3(unloop)
DO_GOTO;
}

LABEL(lit_perform) /* lit-perform ( #a_addr -- ) S1 -- S1  */
/*  */
NAME("lit-perform")
{
DEF_CA
MAYBE_UNUSED Cell * a_addr;
NEXT_P0;
vm_Cell2a_(IMM_ARG(IPTOS,305397761 ),a_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_addr=", vm_out); printarg_a_(a_addr);
}
#endif
INC_IP(1);
{
#line 300 "prim"
#ifndef NO_IP
ip=IP;
#endif
SUPER_END;
VM_JUMP(EXEC1(*(Xt *)a_addr));
#line 672 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
LABEL2(lit_perform)
LABEL3(lit_perform)
DO_GOTO;
}

LABEL(does_exec) /* does-exec ( #a_cfa -- R:nest a_pfa ) S1 -- S1  */
/*  */
NAME("does-exec")
{
DEF_CA
MAYBE_UNUSED Cell * a_cfa;
Cell nest;
Cell * a_pfa;
NEXT_P0;
sp[0] = spTOS;
vm_Cell2a_(IMM_ARG(IPTOS,305397762 ),a_cfa);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_cfa=", vm_out); printarg_a_(a_cfa);
}
#endif
INC_IP(1);
sp += -1;
rp += -1;
{
#line 307 "prim"
#ifdef NO_IP
/* compiled to LIT CALL by compile_prim */
assert(0);
#else
a_pfa = PFA(a_cfa);
nest = (Cell)IP;
#ifdef DEBUG
    {
      CFA_TO_NAME(a_cfa);
      fprintf(stderr,"%08lx: does %08lx %.*s\n",
	      (Cell)ip,(Cell)a_cfa,len,name);
    }
#endif
SET_IP(DOES_CODE1(a_cfa));
#endif
#line 721 "prim-fast.i"
}
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" nest=", vm_out); printarg_n(nest);
fputs(" a_pfa=", vm_out); printarg_a_(a_pfa);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(nest,rp[0]);
vm_a_2Cell(a_pfa,spTOS);
LABEL2(does_exec)
NEXT_P1_5;
LABEL3(does_exec)
DO_GOTO;
}

GROUPADD(8)
#ifdef HAS_GLOCALS
LABEL(branch_lp_plus_store_number) /* branch-lp+!# ( #a_target #nlocals -- ) S1 -- S1  */
/*  */
NAME("branch-lp+!#")
{
DEF_CA
MAYBE_UNUSED Cell * a_target;
MAYBE_UNUSED Cell nlocals;
NEXT_P0;
vm_Cell2a_(IMM_ARG(IPTOS,305397763 ),a_target);
vm_Cell2n(IMM_ARG(IP[1],305397764 ),nlocals);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_target=", vm_out); printarg_a_(a_target);
fputs(" nlocals=", vm_out); printarg_n(nlocals);
}
#endif
INC_IP(2);
{
#line 326 "prim"
/* this will probably not be used */
lp += nlocals;
#ifdef NO_IP
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;

JUMP(a_target);
#else
SET_IP((Xt *)a_target);
#endif
#line 778 "prim-fast.i"
}
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
LABEL2(branch_lp_plus_store_number)
NEXT_P1_5;
LABEL3(branch_lp_plus_store_number)
DO_GOTO;
}

GROUPADD(1)
#endif
LABEL(branch) /* branch ( #a_target -- ) S1 -- S1  */
/*  */
NAME("branch")
{
DEF_CA
MAYBE_UNUSED Cell * a_target;
NEXT_P0;
vm_Cell2a_(IMM_ARG(IPTOS,305397765 ),a_target);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_target=", vm_out); printarg_a_(a_target);
}
#endif
INC_IP(1);
{
#line 338 "prim"
#ifdef NO_IP
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;

JUMP(a_target);
#else
SET_IP((Xt *)a_target);
#endif
#line 826 "prim-fast.i"
}
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
LABEL2(branch)
NEXT_P1_5;
LABEL3(branch)
DO_GOTO;
}

LABEL(question_branch) /* ?branch ( #a_target f -- ) S1 -- S1  */
/*  */
NAME("?branch")
{
DEF_CA
MAYBE_UNUSED Cell * a_target;
MAYBE_UNUSED Bool f;
NEXT_P0;
vm_Cell2a_(IMM_ARG(IPTOS,305397766 ),a_target);
vm_Cell2f(spTOS,f);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_target=", vm_out); printarg_a_(a_target);
fputs(" f=", vm_out); printarg_f(f);
}
#endif
INC_IP(1);
sp += 1;
{
#line 383 "prim"
	#ifdef NO_IP
#line 382
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
spTOS = sp[0];

#line 382
#endif
#line 382
if (f==0) {
#line 382
	#ifdef NO_IP
#line 382
JUMP(a_target);
#line 382
#else
#line 382
SET_IP((Xt *)a_target);
#line 382
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
spTOS = sp[0];
 NEXT_P2;
#line 382
#endif
#line 382
}
#line 382
SUPER_CONTINUE;
#line 382
#line 904 "prim-fast.i"
}
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
spTOS = sp[0];
LABEL2(question_branch)
NEXT_P1_5;
LABEL3(question_branch)
DO_GOTO;
}

GROUPADD(2)
#ifdef HAS_GLOCALS
LABEL(question_branch_lp_plus_store_number) /* ?branch-lp+!# ( #a_target #nlocals f -- ) S1 -- S1  */
/*  */
NAME("?branch-lp+!#")
{
DEF_CA
MAYBE_UNUSED Cell * a_target;
MAYBE_UNUSED Cell nlocals;
MAYBE_UNUSED Bool f;
NEXT_P0;
vm_Cell2a_(IMM_ARG(IPTOS,305397767 ),a_target);
vm_Cell2n(IMM_ARG(IP[1],305397768 ),nlocals);
vm_Cell2f(spTOS,f);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_target=", vm_out); printarg_a_(a_target);
fputs(" nlocals=", vm_out); printarg_n(nlocals);
fputs(" f=", vm_out); printarg_f(f);
}
#endif
INC_IP(2);
sp += 1;
{
#line 383 "prim"
	#ifdef NO_IP
#line 382
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
spTOS = sp[0];

#line 382
#endif
#line 382
if (f==0) {
#line 382
	lp += nlocals;
#line 382
#ifdef NO_IP
#line 382
JUMP(a_target);
#line 382
#else
#line 382
SET_IP((Xt *)a_target);
#line 382
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
spTOS = sp[0];
 NEXT_P2;
#line 382
#endif
#line 382
}
#line 382
SUPER_CONTINUE;
#line 382
#line 990 "prim-fast.i"
}
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
spTOS = sp[0];
LABEL2(question_branch_lp_plus_store_number)
NEXT_P1_5;
LABEL3(question_branch_lp_plus_store_number)
DO_GOTO;
}

GROUPADD(1)
#endif
GROUPADD(0)
#ifdef HAS_XCONDS
LABEL(question_dupe_question_branch) /* ?dup-?branch ( #a_target f -- S:... ) S1 -- S1  */
/* The run-time procedure compiled by @code{?DUP-IF}. */
NAME("?dup-?branch")
{
DEF_CA
MAYBE_UNUSED Cell * a_target;
MAYBE_UNUSED Bool f;
NEXT_P0;
vm_Cell2a_(IMM_ARG(IPTOS,305397769 ),a_target);
vm_Cell2f(spTOS,f);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_target=", vm_out); printarg_a_(a_target);
fputs(" f=", vm_out); printarg_f(f);
}
#endif
INC_IP(1);
sp += 1;
{
#line 398 "prim"
if (f==0) {
#ifdef NO_IP
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
spTOS = sp[0];

JUMP(a_target);
#else
SET_IP((Xt *)a_target);
#endif
} else {
sp--;
sp[0]=f;
}
#line 1051 "prim-fast.i"
}
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
spTOS = sp[0];
LABEL2(question_dupe_question_branch)
NEXT_P1_5;
LABEL3(question_dupe_question_branch)
DO_GOTO;
}

LABEL(question_dupe_zero_equals_question_branch) /* ?dup-0=-?branch ( #a_target f -- S:... ) S1 -- S1  */
/* The run-time procedure compiled by @code{?DUP-0=-IF}. */
NAME("?dup-0=-?branch")
{
DEF_CA
MAYBE_UNUSED Cell * a_target;
MAYBE_UNUSED Bool f;
NEXT_P0;
vm_Cell2a_(IMM_ARG(IPTOS,305397770 ),a_target);
vm_Cell2f(spTOS,f);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_target=", vm_out); printarg_a_(a_target);
fputs(" f=", vm_out); printarg_f(f);
}
#endif
INC_IP(1);
sp += 1;
{
#line 412 "prim"
if (f!=0) {
  sp--;
  sp[0]=f;
#ifdef NO_IP
  JUMP(a_target);
#else
  SET_IP((Xt *)a_target);
#endif
}
#line 1097 "prim-fast.i"
}
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
spTOS = sp[0];
LABEL2(question_dupe_zero_equals_question_branch)
NEXT_P1_5;
LABEL3(question_dupe_zero_equals_question_branch)
DO_GOTO;
}

GROUPADD(2)
#endif
LABEL(paren_next) /* (next) ( #a_target R:n1 -- R:n2 ) S1 -- S1  */
/*  */
NAME("(next)")
{
DEF_CA
MAYBE_UNUSED Cell * a_target;
MAYBE_UNUSED Cell n1;
Cell n2;
NEXT_P0;
vm_Cell2a_(IMM_ARG(IPTOS,305397771 ),a_target);
vm_Cell2n(rp[0],n1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_target=", vm_out); printarg_a_(a_target);
fputs(" n1=", vm_out); printarg_n(n1);
}
#endif
INC_IP(1);
{
#line 425 "prim"
n2=n1-1;
#line 424
	#ifdef NO_IP
#line 424
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n2=", vm_out); printarg_n(n2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n2,rp[0]);

#line 424
#endif
#line 424
if (n1) {
#line 424
	#ifdef NO_IP
#line 424
JUMP(a_target);
#line 424
#else
#line 424
SET_IP((Xt *)a_target);
#line 424
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n2=", vm_out); printarg_n(n2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n2,rp[0]);
 NEXT_P2;
#line 424
#endif
#line 424
}
#line 424
SUPER_CONTINUE;
#line 424
#line 1182 "prim-fast.i"
}
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n2=", vm_out); printarg_n(n2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n2,rp[0]);
LABEL2(paren_next)
NEXT_P1_5;
LABEL3(paren_next)
DO_GOTO;
}

GROUPADD(1)
#ifdef HAS_GLOCALS
LABEL(paren_next_lp_plus_store_number) /* (next)-lp+!# ( #a_target #nlocals R:n1 -- R:n2 ) S1 -- S1  */
/*  */
NAME("(next)-lp+!#")
{
DEF_CA
MAYBE_UNUSED Cell * a_target;
MAYBE_UNUSED Cell nlocals;
MAYBE_UNUSED Cell n1;
Cell n2;
NEXT_P0;
vm_Cell2a_(IMM_ARG(IPTOS,305397772 ),a_target);
vm_Cell2n(IMM_ARG(IP[1],305397773 ),nlocals);
vm_Cell2n(rp[0],n1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_target=", vm_out); printarg_a_(a_target);
fputs(" nlocals=", vm_out); printarg_n(nlocals);
fputs(" n1=", vm_out); printarg_n(n1);
}
#endif
INC_IP(2);
{
#line 425 "prim"
n2=n1-1;
#line 424
	#ifdef NO_IP
#line 424
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n2=", vm_out); printarg_n(n2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n2,rp[0]);

#line 424
#endif
#line 424
if (n1) {
#line 424
	lp += nlocals;
#line 424
#ifdef NO_IP
#line 424
JUMP(a_target);
#line 424
#else
#line 424
SET_IP((Xt *)a_target);
#line 424
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n2=", vm_out); printarg_n(n2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n2,rp[0]);
 NEXT_P2;
#line 424
#endif
#line 424
}
#line 424
SUPER_CONTINUE;
#line 424
#line 1273 "prim-fast.i"
}
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n2=", vm_out); printarg_n(n2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n2,rp[0]);
LABEL2(paren_next_lp_plus_store_number)
NEXT_P1_5;
LABEL3(paren_next_lp_plus_store_number)
DO_GOTO;
}

GROUPADD(1)
#endif
LABEL(paren_loop) /* (loop) ( #a_target R:nlimit R:n1 -- R:nlimit R:n2 ) S1 -- S1  */
/*  */
NAME("(loop)")
{
DEF_CA
MAYBE_UNUSED Cell * a_target;
MAYBE_UNUSED Cell nlimit;
MAYBE_UNUSED Cell n1;
Cell n2;
NEXT_P0;
vm_Cell2a_(IMM_ARG(IPTOS,305397774 ),a_target);
vm_Cell2n(rp[1],nlimit);
vm_Cell2n(rp[0],n1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_target=", vm_out); printarg_a_(a_target);
fputs(" nlimit=", vm_out); printarg_n(nlimit);
fputs(" n1=", vm_out); printarg_n(n1);
}
#endif
INC_IP(1);
{
#line 432 "prim"
n2=n1+1;
#line 431
	#ifdef NO_IP
#line 431
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n2=", vm_out); printarg_n(n2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n2,rp[0]);

#line 431
#endif
#line 431
if (n2 != nlimit) {
#line 431
	#ifdef NO_IP
#line 431
JUMP(a_target);
#line 431
#else
#line 431
SET_IP((Xt *)a_target);
#line 431
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n2=", vm_out); printarg_n(n2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n2,rp[0]);
 NEXT_P2;
#line 431
#endif
#line 431
}
#line 431
SUPER_CONTINUE;
#line 431
#line 1362 "prim-fast.i"
}
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n2=", vm_out); printarg_n(n2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n2,rp[0]);
LABEL2(paren_loop)
NEXT_P1_5;
LABEL3(paren_loop)
DO_GOTO;
}

GROUPADD(1)
#ifdef HAS_GLOCALS
LABEL(paren_loop_lp_plus_store_number) /* (loop)-lp+!# ( #a_target #nlocals R:nlimit R:n1 -- R:nlimit R:n2 ) S1 -- S1  */
/*  */
NAME("(loop)-lp+!#")
{
DEF_CA
MAYBE_UNUSED Cell * a_target;
MAYBE_UNUSED Cell nlocals;
MAYBE_UNUSED Cell nlimit;
MAYBE_UNUSED Cell n1;
Cell n2;
NEXT_P0;
vm_Cell2a_(IMM_ARG(IPTOS,305397775 ),a_target);
vm_Cell2n(IMM_ARG(IP[1],305397776 ),nlocals);
vm_Cell2n(rp[1],nlimit);
vm_Cell2n(rp[0],n1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_target=", vm_out); printarg_a_(a_target);
fputs(" nlocals=", vm_out); printarg_n(nlocals);
fputs(" nlimit=", vm_out); printarg_n(nlimit);
fputs(" n1=", vm_out); printarg_n(n1);
}
#endif
INC_IP(2);
{
#line 432 "prim"
n2=n1+1;
#line 431
	#ifdef NO_IP
#line 431
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n2=", vm_out); printarg_n(n2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n2,rp[0]);

#line 431
#endif
#line 431
if (n2 != nlimit) {
#line 431
	lp += nlocals;
#line 431
#ifdef NO_IP
#line 431
JUMP(a_target);
#line 431
#else
#line 431
SET_IP((Xt *)a_target);
#line 431
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n2=", vm_out); printarg_n(n2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n2,rp[0]);
 NEXT_P2;
#line 431
#endif
#line 431
}
#line 431
SUPER_CONTINUE;
#line 431
#line 1456 "prim-fast.i"
}
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n2=", vm_out); printarg_n(n2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n2,rp[0]);
LABEL2(paren_loop_lp_plus_store_number)
NEXT_P1_5;
LABEL3(paren_loop_lp_plus_store_number)
DO_GOTO;
}

GROUPADD(1)
#endif
LABEL(paren_plus_loop) /* (+loop) ( #a_target n R:nlimit R:n1 -- R:nlimit R:n2 ) S1 -- S1  */
/*  */
NAME("(+loop)")
{
DEF_CA
MAYBE_UNUSED Cell * a_target;
MAYBE_UNUSED Cell n;
MAYBE_UNUSED Cell nlimit;
MAYBE_UNUSED Cell n1;
Cell n2;
NEXT_P0;
vm_Cell2a_(IMM_ARG(IPTOS,305397777 ),a_target);
vm_Cell2n(spTOS,n);
vm_Cell2n(rp[1],nlimit);
vm_Cell2n(rp[0],n1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_target=", vm_out); printarg_a_(a_target);
fputs(" n=", vm_out); printarg_n(n);
fputs(" nlimit=", vm_out); printarg_n(nlimit);
fputs(" n1=", vm_out); printarg_n(n1);
}
#endif
INC_IP(1);
sp += 1;
{
#line 440 "prim"
/* !! check this thoroughly */
#line 439
/* sign bit manipulation and test: (x^y)<0 is equivalent to (x<0) != (y<0) */
#line 439
/* dependent upon two's complement arithmetic */
#line 439
Cell olddiff = n1-nlimit;
#line 439
n2=n1+n;	
#line 439
	#ifdef NO_IP
#line 439
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n2=", vm_out); printarg_n(n2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n2,rp[0]);
spTOS = sp[0];

#line 439
#endif
#line 439
if (((olddiff^(olddiff+n))    /* the limit is not crossed */
#line 439
     &(olddiff^n))	       /* OR it is a wrap-around effect */
#line 439
    >=0) { /* & is used to avoid having two branches for gforth-native */
#line 439
	#ifdef NO_IP
#line 439
JUMP(a_target);
#line 439
#else
#line 439
SET_IP((Xt *)a_target);
#line 439
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n2=", vm_out); printarg_n(n2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n2,rp[0]);
spTOS = sp[0];
 NEXT_P2;
#line 439
#endif
#line 439
}
#line 439
SUPER_CONTINUE;
#line 439
#line 1563 "prim-fast.i"
}
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n2=", vm_out); printarg_n(n2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n2,rp[0]);
spTOS = sp[0];
LABEL2(paren_plus_loop)
NEXT_P1_5;
LABEL3(paren_plus_loop)
DO_GOTO;
}

GROUPADD(1)
#ifdef HAS_GLOCALS
LABEL(paren_plus_loop_lp_plus_store_number) /* (+loop)-lp+!# ( #a_target #nlocals n R:nlimit R:n1 -- R:nlimit R:n2 ) S1 -- S1  */
/*  */
NAME("(+loop)-lp+!#")
{
DEF_CA
MAYBE_UNUSED Cell * a_target;
MAYBE_UNUSED Cell nlocals;
MAYBE_UNUSED Cell n;
MAYBE_UNUSED Cell nlimit;
MAYBE_UNUSED Cell n1;
Cell n2;
NEXT_P0;
vm_Cell2a_(IMM_ARG(IPTOS,305397778 ),a_target);
vm_Cell2n(IMM_ARG(IP[1],305397779 ),nlocals);
vm_Cell2n(spTOS,n);
vm_Cell2n(rp[1],nlimit);
vm_Cell2n(rp[0],n1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_target=", vm_out); printarg_a_(a_target);
fputs(" nlocals=", vm_out); printarg_n(nlocals);
fputs(" n=", vm_out); printarg_n(n);
fputs(" nlimit=", vm_out); printarg_n(nlimit);
fputs(" n1=", vm_out); printarg_n(n1);
}
#endif
INC_IP(2);
sp += 1;
{
#line 440 "prim"
/* !! check this thoroughly */
#line 439
/* sign bit manipulation and test: (x^y)<0 is equivalent to (x<0) != (y<0) */
#line 439
/* dependent upon two's complement arithmetic */
#line 439
Cell olddiff = n1-nlimit;
#line 439
n2=n1+n;	
#line 439
	#ifdef NO_IP
#line 439
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n2=", vm_out); printarg_n(n2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n2,rp[0]);
spTOS = sp[0];

#line 439
#endif
#line 439
if (((olddiff^(olddiff+n))    /* the limit is not crossed */
#line 439
     &(olddiff^n))	       /* OR it is a wrap-around effect */
#line 439
    >=0) { /* & is used to avoid having two branches for gforth-native */
#line 439
	lp += nlocals;
#line 439
#ifdef NO_IP
#line 439
JUMP(a_target);
#line 439
#else
#line 439
SET_IP((Xt *)a_target);
#line 439
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n2=", vm_out); printarg_n(n2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n2,rp[0]);
spTOS = sp[0];
 NEXT_P2;
#line 439
#endif
#line 439
}
#line 439
SUPER_CONTINUE;
#line 439
#line 1676 "prim-fast.i"
}
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n2=", vm_out); printarg_n(n2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n2,rp[0]);
spTOS = sp[0];
LABEL2(paren_plus_loop_lp_plus_store_number)
NEXT_P1_5;
LABEL3(paren_plus_loop_lp_plus_store_number)
DO_GOTO;
}

GROUPADD(1)
#endif
GROUPADD(0)
#ifdef HAS_XCONDS
LABEL(paren_minus_loop) /* (-loop) ( #a_target u R:nlimit R:n1 -- R:nlimit R:n2 ) S1 -- S1  */
/*  */
NAME("(-loop)")
{
DEF_CA
MAYBE_UNUSED Cell * a_target;
MAYBE_UNUSED UCell u;
MAYBE_UNUSED Cell nlimit;
MAYBE_UNUSED Cell n1;
Cell n2;
NEXT_P0;
vm_Cell2a_(IMM_ARG(IPTOS,305397780 ),a_target);
vm_Cell2u(spTOS,u);
vm_Cell2n(rp[1],nlimit);
vm_Cell2n(rp[0],n1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_target=", vm_out); printarg_a_(a_target);
fputs(" u=", vm_out); printarg_u(u);
fputs(" nlimit=", vm_out); printarg_n(nlimit);
fputs(" n1=", vm_out); printarg_n(n1);
}
#endif
INC_IP(1);
sp += 1;
{
#line 459 "prim"
UCell olddiff = n1-nlimit;
#line 458
n2=n1-u;
#line 458
	#ifdef NO_IP
#line 458
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n2=", vm_out); printarg_n(n2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n2,rp[0]);
spTOS = sp[0];

#line 458
#endif
#line 458
if (olddiff>u) {
#line 458
	#ifdef NO_IP
#line 458
JUMP(a_target);
#line 458
#else
#line 458
SET_IP((Xt *)a_target);
#line 458
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n2=", vm_out); printarg_n(n2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n2,rp[0]);
spTOS = sp[0];
 NEXT_P2;
#line 458
#endif
#line 458
}
#line 458
SUPER_CONTINUE;
#line 458
#line 1776 "prim-fast.i"
}
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n2=", vm_out); printarg_n(n2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n2,rp[0]);
spTOS = sp[0];
LABEL2(paren_minus_loop)
NEXT_P1_5;
LABEL3(paren_minus_loop)
DO_GOTO;
}

GROUPADD(1)
#ifdef HAS_GLOCALS
LABEL(paren_minus_loop_lp_plus_store_number) /* (-loop)-lp+!# ( #a_target #nlocals u R:nlimit R:n1 -- R:nlimit R:n2 ) S1 -- S1  */
/*  */
NAME("(-loop)-lp+!#")
{
DEF_CA
MAYBE_UNUSED Cell * a_target;
MAYBE_UNUSED Cell nlocals;
MAYBE_UNUSED UCell u;
MAYBE_UNUSED Cell nlimit;
MAYBE_UNUSED Cell n1;
Cell n2;
NEXT_P0;
vm_Cell2a_(IMM_ARG(IPTOS,305397781 ),a_target);
vm_Cell2n(IMM_ARG(IP[1],305397782 ),nlocals);
vm_Cell2u(spTOS,u);
vm_Cell2n(rp[1],nlimit);
vm_Cell2n(rp[0],n1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_target=", vm_out); printarg_a_(a_target);
fputs(" nlocals=", vm_out); printarg_n(nlocals);
fputs(" u=", vm_out); printarg_u(u);
fputs(" nlimit=", vm_out); printarg_n(nlimit);
fputs(" n1=", vm_out); printarg_n(n1);
}
#endif
INC_IP(2);
sp += 1;
{
#line 459 "prim"
UCell olddiff = n1-nlimit;
#line 458
n2=n1-u;
#line 458
	#ifdef NO_IP
#line 458
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n2=", vm_out); printarg_n(n2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n2,rp[0]);
spTOS = sp[0];

#line 458
#endif
#line 458
if (olddiff>u) {
#line 458
	lp += nlocals;
#line 458
#ifdef NO_IP
#line 458
JUMP(a_target);
#line 458
#else
#line 458
SET_IP((Xt *)a_target);
#line 458
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n2=", vm_out); printarg_n(n2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n2,rp[0]);
spTOS = sp[0];
 NEXT_P2;
#line 458
#endif
#line 458
}
#line 458
SUPER_CONTINUE;
#line 458
#line 1879 "prim-fast.i"
}
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n2=", vm_out); printarg_n(n2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n2,rp[0]);
spTOS = sp[0];
LABEL2(paren_minus_loop_lp_plus_store_number)
NEXT_P1_5;
LABEL3(paren_minus_loop_lp_plus_store_number)
DO_GOTO;
}

GROUPADD(1)
#endif
LABEL(paren_symmetric_plus_loop) /* (s+loop) ( #a_target n R:nlimit R:n1 -- R:nlimit R:n2 ) S1 -- S1  */
/* The run-time procedure compiled by S+LOOP. It loops until the index
crosses the boundary between limit and limit-sign(n). I.e. a symmetric
version of (+LOOP). */
NAME("(s+loop)")
{
DEF_CA
MAYBE_UNUSED Cell * a_target;
MAYBE_UNUSED Cell n;
MAYBE_UNUSED Cell nlimit;
MAYBE_UNUSED Cell n1;
Cell n2;
NEXT_P0;
vm_Cell2a_(IMM_ARG(IPTOS,305397783 ),a_target);
vm_Cell2n(spTOS,n);
vm_Cell2n(rp[1],nlimit);
vm_Cell2n(rp[0],n1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_target=", vm_out); printarg_a_(a_target);
fputs(" n=", vm_out); printarg_n(n);
fputs(" nlimit=", vm_out); printarg_n(nlimit);
fputs(" n1=", vm_out); printarg_n(n1);
}
#endif
INC_IP(1);
sp += 1;
{
#line 465 "prim"
/* !! check this thoroughly */
#line 464
Cell diff = n1-nlimit;
#line 464
Cell newdiff = diff+n;
#line 464
if (n<0) {
#line 464
    diff = -diff;
#line 464
    newdiff = -newdiff;
#line 464
}
#line 464
n2=n1+n;
#line 464
	#ifdef NO_IP
#line 464
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n2=", vm_out); printarg_n(n2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n2,rp[0]);
spTOS = sp[0];

#line 464
#endif
#line 464
if (((~diff)|newdiff)<0) { /* use | to avoid two branches for gforth-native */
#line 464
	#ifdef NO_IP
#line 464
JUMP(a_target);
#line 464
#else
#line 464
SET_IP((Xt *)a_target);
#line 464
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n2=", vm_out); printarg_n(n2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n2,rp[0]);
spTOS = sp[0];
 NEXT_P2;
#line 464
#endif
#line 464
}
#line 464
SUPER_CONTINUE;
#line 464
#line 1991 "prim-fast.i"
}
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n2=", vm_out); printarg_n(n2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n2,rp[0]);
spTOS = sp[0];
LABEL2(paren_symmetric_plus_loop)
NEXT_P1_5;
LABEL3(paren_symmetric_plus_loop)
DO_GOTO;
}

GROUPADD(1)
#ifdef HAS_GLOCALS
LABEL(paren_symmetric_plus_loop_lp_plus_store_number) /* (s+loop)-lp+!# ( #a_target #nlocals n R:nlimit R:n1 -- R:nlimit R:n2 ) S1 -- S1  */
/* The run-time procedure compiled by S+LOOP. It loops until the index
crosses the boundary between limit and limit-sign(n). I.e. a symmetric
version of (+LOOP). */
NAME("(s+loop)-lp+!#")
{
DEF_CA
MAYBE_UNUSED Cell * a_target;
MAYBE_UNUSED Cell nlocals;
MAYBE_UNUSED Cell n;
MAYBE_UNUSED Cell nlimit;
MAYBE_UNUSED Cell n1;
Cell n2;
NEXT_P0;
vm_Cell2a_(IMM_ARG(IPTOS,305397784 ),a_target);
vm_Cell2n(IMM_ARG(IP[1],305397785 ),nlocals);
vm_Cell2n(spTOS,n);
vm_Cell2n(rp[1],nlimit);
vm_Cell2n(rp[0],n1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_target=", vm_out); printarg_a_(a_target);
fputs(" nlocals=", vm_out); printarg_n(nlocals);
fputs(" n=", vm_out); printarg_n(n);
fputs(" nlimit=", vm_out); printarg_n(nlimit);
fputs(" n1=", vm_out); printarg_n(n1);
}
#endif
INC_IP(2);
sp += 1;
{
#line 465 "prim"
/* !! check this thoroughly */
#line 464
Cell diff = n1-nlimit;
#line 464
Cell newdiff = diff+n;
#line 464
if (n<0) {
#line 464
    diff = -diff;
#line 464
    newdiff = -newdiff;
#line 464
}
#line 464
n2=n1+n;
#line 464
	#ifdef NO_IP
#line 464
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n2=", vm_out); printarg_n(n2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n2,rp[0]);
spTOS = sp[0];

#line 464
#endif
#line 464
if (((~diff)|newdiff)<0) { /* use | to avoid two branches for gforth-native */
#line 464
	lp += nlocals;
#line 464
#ifdef NO_IP
#line 464
JUMP(a_target);
#line 464
#else
#line 464
SET_IP((Xt *)a_target);
#line 464
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n2=", vm_out); printarg_n(n2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n2,rp[0]);
spTOS = sp[0];
 NEXT_P2;
#line 464
#endif
#line 464
}
#line 464
SUPER_CONTINUE;
#line 464
#line 2108 "prim-fast.i"
}
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n2=", vm_out); printarg_n(n2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n2,rp[0]);
spTOS = sp[0];
LABEL2(paren_symmetric_plus_loop_lp_plus_store_number)
NEXT_P1_5;
LABEL3(paren_symmetric_plus_loop_lp_plus_store_number)
DO_GOTO;
}

GROUPADD(1)
#endif
GROUPADD(0)
#endif
LABEL(paren_for) /* (for) ( ncount -- R:nlimit R:ncount ) S1 -- S1  */
/*  */
NAME("(for)")
{
DEF_CA
MAYBE_UNUSED Cell ncount;
Cell nlimit;
NEXT_P0;
vm_Cell2n(spTOS,ncount);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" ncount=", vm_out); printarg_n(ncount);
}
#endif
sp += 1;
rp += -2;
{
#line 483 "prim"
/* or (for) = >r -- collides with unloop! */
nlimit=0;
#line 2151 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" nlimit=", vm_out); printarg_n(nlimit);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(nlimit,rp[1]);
vm_n2Cell(ncount,rp[0]);
spTOS = sp[0];
LABEL2(paren_for)
NEXT_P1_5;
LABEL3(paren_for)
DO_GOTO;
}

LABEL(paren_do) /* (do) ( nlimit nstart -- R:nlimit R:nstart ) S1 -- S1  */
/*  */
NAME("(do)")
{
DEF_CA
MAYBE_UNUSED Cell nlimit;
MAYBE_UNUSED Cell nstart;
NEXT_P0;
vm_Cell2n(sp[1],nlimit);
vm_Cell2n(spTOS,nstart);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" nlimit=", vm_out); printarg_n(nlimit);
fputs(" nstart=", vm_out); printarg_n(nstart);
}
#endif
sp += 2;
rp += -2;
{
#line 489 "prim"
#line 2190 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(nlimit,rp[1]);
vm_n2Cell(nstart,rp[0]);
spTOS = sp[0];
LABEL2(paren_do)
NEXT_P1_5;
LABEL3(paren_do)
DO_GOTO;
}

LABEL(paren_question_do) /* (?do) ( #a_target nlimit nstart -- R:nlimit R:nstart ) S1 -- S1  */
/*  */
NAME("(?do)")
{
DEF_CA
MAYBE_UNUSED Cell * a_target;
MAYBE_UNUSED Cell nlimit;
MAYBE_UNUSED Cell nstart;
NEXT_P0;
vm_Cell2a_(IMM_ARG(IPTOS,305397786 ),a_target);
vm_Cell2n(sp[1],nlimit);
vm_Cell2n(spTOS,nstart);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_target=", vm_out); printarg_a_(a_target);
fputs(" nlimit=", vm_out); printarg_n(nlimit);
fputs(" nstart=", vm_out); printarg_n(nstart);
}
#endif
INC_IP(1);
sp += 2;
rp += -2;
{
#line 493 "prim"
#ifdef NO_IP
    SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(nlimit,rp[1]);
vm_n2Cell(nstart,rp[0]);
spTOS = sp[0];

#endif
if (nstart == nlimit) {
#ifdef NO_IP
    JUMP(a_target);
#else
    SET_IP((Xt *)a_target);
#endif
}
#line 2253 "prim-fast.i"
}
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(nlimit,rp[1]);
vm_n2Cell(nstart,rp[0]);
spTOS = sp[0];
LABEL2(paren_question_do)
NEXT_P1_5;
LABEL3(paren_question_do)
DO_GOTO;
}

GROUPADD(3)
#ifdef HAS_XCONDS
LABEL(paren_plus_do) /* (+do) ( #a_target nlimit nstart -- R:nlimit R:nstart ) S1 -- S1  */
/*  */
NAME("(+do)")
{
DEF_CA
MAYBE_UNUSED Cell * a_target;
MAYBE_UNUSED Cell nlimit;
MAYBE_UNUSED Cell nstart;
NEXT_P0;
vm_Cell2a_(IMM_ARG(IPTOS,305397787 ),a_target);
vm_Cell2n(sp[1],nlimit);
vm_Cell2n(spTOS,nstart);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_target=", vm_out); printarg_a_(a_target);
fputs(" nlimit=", vm_out); printarg_n(nlimit);
fputs(" nstart=", vm_out); printarg_n(nstart);
}
#endif
INC_IP(1);
sp += 2;
rp += -2;
{
#line 514 "prim"
#ifdef NO_IP
    SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(nlimit,rp[1]);
vm_n2Cell(nstart,rp[0]);
spTOS = sp[0];

#endif
if (nstart >= nlimit) {
#ifdef NO_IP
    JUMP(a_target);
#else
    SET_IP((Xt *)a_target);
#endif
}
#line 2319 "prim-fast.i"
}
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(nlimit,rp[1]);
vm_n2Cell(nstart,rp[0]);
spTOS = sp[0];
LABEL2(paren_plus_do)
NEXT_P1_5;
LABEL3(paren_plus_do)
DO_GOTO;
}

LABEL(paren_u_plus_do) /* (u+do) ( #a_target ulimit ustart -- R:ulimit R:ustart ) S1 -- S1  */
/*  */
NAME("(u+do)")
{
DEF_CA
MAYBE_UNUSED Cell * a_target;
MAYBE_UNUSED UCell ulimit;
MAYBE_UNUSED UCell ustart;
NEXT_P0;
vm_Cell2a_(IMM_ARG(IPTOS,305397788 ),a_target);
vm_Cell2u(sp[1],ulimit);
vm_Cell2u(spTOS,ustart);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_target=", vm_out); printarg_a_(a_target);
fputs(" ulimit=", vm_out); printarg_u(ulimit);
fputs(" ustart=", vm_out); printarg_u(ustart);
}
#endif
INC_IP(1);
sp += 2;
rp += -2;
{
#line 535 "prim"
#ifdef NO_IP
    SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_u2Cell(ulimit,rp[1]);
vm_u2Cell(ustart,rp[0]);
spTOS = sp[0];

#endif
if (ustart >= ulimit) {
#ifdef NO_IP
JUMP(a_target);
#else
SET_IP((Xt *)a_target);
#endif
}
#line 2383 "prim-fast.i"
}
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_u2Cell(ulimit,rp[1]);
vm_u2Cell(ustart,rp[0]);
spTOS = sp[0];
LABEL2(paren_u_plus_do)
NEXT_P1_5;
LABEL3(paren_u_plus_do)
DO_GOTO;
}

LABEL(paren_minus_do) /* (-do) ( #a_target nlimit nstart -- R:nlimit R:nstart ) S1 -- S1  */
/*  */
NAME("(-do)")
{
DEF_CA
MAYBE_UNUSED Cell * a_target;
MAYBE_UNUSED Cell nlimit;
MAYBE_UNUSED Cell nstart;
NEXT_P0;
vm_Cell2a_(IMM_ARG(IPTOS,305397789 ),a_target);
vm_Cell2n(sp[1],nlimit);
vm_Cell2n(spTOS,nstart);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_target=", vm_out); printarg_a_(a_target);
fputs(" nlimit=", vm_out); printarg_n(nlimit);
fputs(" nstart=", vm_out); printarg_n(nstart);
}
#endif
INC_IP(1);
sp += 2;
rp += -2;
{
#line 556 "prim"
#ifdef NO_IP
    SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(nlimit,rp[1]);
vm_n2Cell(nstart,rp[0]);
spTOS = sp[0];

#endif
if (nstart <= nlimit) {
#ifdef NO_IP
JUMP(a_target);
#else
SET_IP((Xt *)a_target);
#endif
}
#line 2447 "prim-fast.i"
}
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(nlimit,rp[1]);
vm_n2Cell(nstart,rp[0]);
spTOS = sp[0];
LABEL2(paren_minus_do)
NEXT_P1_5;
LABEL3(paren_minus_do)
DO_GOTO;
}

LABEL(paren_u_minus_do) /* (u-do) ( #a_target ulimit ustart -- R:ulimit R:ustart ) S1 -- S1  */
/*  */
NAME("(u-do)")
{
DEF_CA
MAYBE_UNUSED Cell * a_target;
MAYBE_UNUSED UCell ulimit;
MAYBE_UNUSED UCell ustart;
NEXT_P0;
vm_Cell2a_(IMM_ARG(IPTOS,305397790 ),a_target);
vm_Cell2u(sp[1],ulimit);
vm_Cell2u(spTOS,ustart);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_target=", vm_out); printarg_a_(a_target);
fputs(" ulimit=", vm_out); printarg_u(ulimit);
fputs(" ustart=", vm_out); printarg_u(ustart);
}
#endif
INC_IP(1);
sp += 2;
rp += -2;
{
#line 577 "prim"
#ifdef NO_IP
    SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_u2Cell(ulimit,rp[1]);
vm_u2Cell(ustart,rp[0]);
spTOS = sp[0];

#endif
if (ustart <= ulimit) {
#ifdef NO_IP
JUMP(a_target);
#else
SET_IP((Xt *)a_target);
#endif
}
#line 2511 "prim-fast.i"
}
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_u2Cell(ulimit,rp[1]);
vm_u2Cell(ustart,rp[0]);
spTOS = sp[0];
LABEL2(paren_u_minus_do)
NEXT_P1_5;
LABEL3(paren_u_minus_do)
DO_GOTO;
}

GROUPADD(4)
#endif
LABEL(i) /* i ( R:n -- R:n n ) S1 -- S1  */
/*  */
NAME("i")
{
DEF_CA
MAYBE_UNUSED Cell n;
NEXT_P0;
sp[0] = spTOS;
vm_Cell2n(rp[0],n);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n=", vm_out); printarg_n(n);
}
#endif
sp += -1;
{
#line 603 "prim"
#line 2549 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n,spTOS);
LABEL2(i)
NEXT_P1_5;
LABEL3(i)
DO_GOTO;
}

LABEL(i_tick) /* i' ( R:w R:w2 -- R:w R:w2 w ) S1 -- S1  */
/*  */
NAME("i'")
{
DEF_CA
MAYBE_UNUSED Cell w;
MAYBE_UNUSED Cell w2;
NEXT_P0;
sp[0] = spTOS;
vm_Cell2w(rp[1],w);
vm_Cell2w(rp[0],w2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w=", vm_out); printarg_w(w);
fputs(" w2=", vm_out); printarg_w(w2);
}
#endif
sp += -1;
{
#line 608 "prim"
#line 2585 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w,spTOS);
LABEL2(i_tick)
NEXT_P1_5;
LABEL3(i_tick)
DO_GOTO;
}

LABEL(j) /* j ( R:w R:w1 R:w2 -- w R:w R:w1 R:w2 ) S1 -- S1  */
/*  */
NAME("j")
{
DEF_CA
MAYBE_UNUSED Cell w;
MAYBE_UNUSED Cell w1;
MAYBE_UNUSED Cell w2;
NEXT_P0;
sp[0] = spTOS;
vm_Cell2w(rp[2],w);
vm_Cell2w(rp[1],w1);
vm_Cell2w(rp[0],w2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w=", vm_out); printarg_w(w);
fputs(" w1=", vm_out); printarg_w(w1);
fputs(" w2=", vm_out); printarg_w(w2);
}
#endif
sp += -1;
{
#line 614 "prim"
#line 2624 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w,spTOS);
LABEL2(j)
NEXT_P1_5;
LABEL3(j)
DO_GOTO;
}

LABEL(k) /* k ( R:w R:w1 R:w2 R:w3 R:w4 -- w R:w R:w1 R:w2 R:w3 R:w4 ) S1 -- S1  */
/*  */
NAME("k")
{
DEF_CA
MAYBE_UNUSED Cell w;
MAYBE_UNUSED Cell w1;
MAYBE_UNUSED Cell w2;
MAYBE_UNUSED Cell w3;
MAYBE_UNUSED Cell w4;
NEXT_P0;
sp[0] = spTOS;
vm_Cell2w(rp[4],w);
vm_Cell2w(rp[3],w1);
vm_Cell2w(rp[2],w2);
vm_Cell2w(rp[1],w3);
vm_Cell2w(rp[0],w4);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w=", vm_out); printarg_w(w);
fputs(" w1=", vm_out); printarg_w(w1);
fputs(" w2=", vm_out); printarg_w(w2);
fputs(" w3=", vm_out); printarg_w(w3);
fputs(" w4=", vm_out); printarg_w(w4);
}
#endif
sp += -1;
{
#line 620 "prim"
#line 2669 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w,spTOS);
LABEL2(k)
NEXT_P1_5;
LABEL3(k)
DO_GOTO;
}

GROUPADD(4)
GROUP( strings, 44)
LABEL(move) /* move ( c_from c_to ucount -- ) S1 -- S1  */
/* Copy the contents of @i{ucount} aus at @i{c-from} to
@i{c-to}. @code{move} works correctly even if the two areas overlap. */
NAME("move")
{
DEF_CA
MAYBE_UNUSED Char * c_from;
MAYBE_UNUSED Char * c_to;
MAYBE_UNUSED UCell ucount;
NEXT_P0;
vm_Cell2c_(sp[2],c_from);
vm_Cell2c_(sp[1],c_to);
vm_Cell2u(spTOS,ucount);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_from=", vm_out); printarg_c_(c_from);
fputs(" c_to=", vm_out); printarg_c_(c_to);
fputs(" ucount=", vm_out); printarg_u(ucount);
}
#endif
sp += 3;
{
#line 634 "prim"
/* !! note that the standard specifies addr, not c-addr */
memmove(c_to,c_from,ucount);
/* make an Ifdef for bsd and others? */
#line 2713 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
spTOS = sp[0];
LABEL2(move)
NEXT_P1_5;
LABEL3(move)
DO_GOTO;
}

LABEL(c_move) /* cmove ( c_from c_to u -- ) S1 -- S1  */
/* Copy the contents of @i{ucount} characters from data space at
@i{c-from} to @i{c-to}. The copy proceeds @code{char}-by-@code{char}
from low address to high address; i.e., for overlapping areas it is
safe if @i{c-to}=<@i{c-from}. */
NAME("cmove")
{
DEF_CA
MAYBE_UNUSED Char * c_from;
MAYBE_UNUSED Char * c_to;
MAYBE_UNUSED UCell u;
NEXT_P0;
vm_Cell2c_(sp[2],c_from);
vm_Cell2c_(sp[1],c_to);
vm_Cell2u(spTOS,u);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_from=", vm_out); printarg_c_(c_from);
fputs(" c_to=", vm_out); printarg_c_(c_to);
fputs(" u=", vm_out); printarg_u(u);
}
#endif
sp += 3;
{
#line 645 "prim"
cmove(c_from,c_to,u);
#line 2755 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
spTOS = sp[0];
LABEL2(c_move)
NEXT_P1_5;
LABEL3(c_move)
DO_GOTO;
}

LABEL(c_move_up) /* cmove> ( c_from c_to u -- ) S1 -- S1  */
/* Copy the contents of @i{ucount} characters from data space at
@i{c-from} to @i{c-to}. The copy proceeds @code{char}-by-@code{char}
from high address to low address; i.e., for overlapping areas it is
safe if @i{c-to}>=@i{c-from}. */
NAME("cmove>")
{
DEF_CA
MAYBE_UNUSED Char * c_from;
MAYBE_UNUSED Char * c_to;
MAYBE_UNUSED UCell u;
NEXT_P0;
vm_Cell2c_(sp[2],c_from);
vm_Cell2c_(sp[1],c_to);
vm_Cell2u(spTOS,u);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_from=", vm_out); printarg_c_(c_from);
fputs(" c_to=", vm_out); printarg_c_(c_to);
fputs(" u=", vm_out); printarg_u(u);
}
#endif
sp += 3;
{
#line 654 "prim"
cmove_up(c_from,c_to,u);
#line 2797 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
spTOS = sp[0];
LABEL2(c_move_up)
NEXT_P1_5;
LABEL3(c_move_up)
DO_GOTO;
}

LABEL(fill) /* fill ( c_addr u c -- ) S1 -- S1  */
/* Store @i{c} in @i{u} chars starting at @i{c-addr}. */
NAME("fill")
{
DEF_CA
MAYBE_UNUSED Char * c_addr;
MAYBE_UNUSED UCell u;
MAYBE_UNUSED Char c;
NEXT_P0;
vm_Cell2c_(sp[2],c_addr);
vm_Cell2u(sp[1],u);
vm_Cell2c(spTOS,c);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_addr=", vm_out); printarg_c_(c_addr);
fputs(" u=", vm_out); printarg_u(u);
fputs(" c=", vm_out); printarg_c(c);
}
#endif
sp += 3;
{
#line 662 "prim"
memset(c_addr,c,u);
#line 2836 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
spTOS = sp[0];
LABEL2(fill)
NEXT_P1_5;
LABEL3(fill)
DO_GOTO;
}

LABEL(compare) /* compare ( c_addr1 u1 c_addr2 u2 -- n ) S1 -- S1  */
/* Compare two strings lexicographically. If they are equal, @i{n} is 0; if
the first string is smaller, @i{n} is -1; if the first string is larger, @i{n}
is 1. Currently this is based on the machine's character
comparison. In the future, this may change to consider the current
locale and its collation order. */
NAME("compare")
{
DEF_CA
MAYBE_UNUSED Char * c_addr1;
MAYBE_UNUSED UCell u1;
MAYBE_UNUSED Char * c_addr2;
MAYBE_UNUSED UCell u2;
Cell n;
NEXT_P0;
vm_Cell2c_(sp[3],c_addr1);
vm_Cell2u(sp[2],u1);
vm_Cell2c_(sp[1],c_addr2);
vm_Cell2u(spTOS,u2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_addr1=", vm_out); printarg_c_(c_addr1);
fputs(" u1=", vm_out); printarg_u(u1);
fputs(" c_addr2=", vm_out); printarg_c_(c_addr2);
fputs(" u2=", vm_out); printarg_u(u2);
}
#endif
sp += 3;
{
#line 673 "prim"
/* close ' to keep fontify happy */ 
n = compare(c_addr1, u1, c_addr2, u2);
#line 2884 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n=", vm_out); printarg_n(n);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n,spTOS);
LABEL2(compare)
NEXT_P1_5;
LABEL3(compare)
DO_GOTO;
}

LABEL(toupper) /* toupper ( c1 -- c2 ) S1 -- S1  */
/* If @i{c1} is a lower-case character (in the current locale), @i{c2}
is the equivalent upper-case character. All other characters are unchanged. */
NAME("toupper")
{
DEF_CA
MAYBE_UNUSED Char c1;
Char c2;
NEXT_P0;
vm_Cell2c(spTOS,c1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c1=", vm_out); printarg_c(c1);
}
#endif
{
#line 702 "prim"
c2 = toupper(c1);
#line 2919 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" c2=", vm_out); printarg_c(c2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_c2Cell(c2,spTOS);
LABEL2(toupper)
NEXT_P1_5;
LABEL3(toupper)
DO_GOTO;
}

LABEL(capscompare) /* capscompare ( c_addr1 u1 c_addr2 u2 -- n ) S1 -- S1  */
/* Compare two strings lexicographically. If they are equal, @i{n} is 0; if
the first string is smaller, @i{n} is -1; if the first string is larger, @i{n}
is 1. Currently this is based on the machine's character
comparison. In the future, this may change to consider the current
locale and its collation order. */
NAME("capscompare")
{
DEF_CA
MAYBE_UNUSED Char * c_addr1;
MAYBE_UNUSED UCell u1;
MAYBE_UNUSED Char * c_addr2;
MAYBE_UNUSED UCell u2;
Cell n;
NEXT_P0;
vm_Cell2c_(sp[3],c_addr1);
vm_Cell2u(sp[2],u1);
vm_Cell2c_(sp[1],c_addr2);
vm_Cell2u(spTOS,u2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_addr1=", vm_out); printarg_c_(c_addr1);
fputs(" u1=", vm_out); printarg_u(u1);
fputs(" c_addr2=", vm_out); printarg_c_(c_addr2);
fputs(" u2=", vm_out); printarg_u(u2);
}
#endif
sp += 3;
{
#line 712 "prim"
/* close ' to keep fontify happy */ 
n = capscompare(c_addr1, u1, c_addr2, u2);
#line 2968 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n=", vm_out); printarg_n(n);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n,spTOS);
LABEL2(capscompare)
NEXT_P1_5;
LABEL3(capscompare)
DO_GOTO;
}

LABEL(slash_string) /* /string ( c_addr1 u1 n -- c_addr2 u2 ) S1 -- S1  */
/* Adjust the string specified by @i{c-addr1, u1} to remove @i{n}
characters from the start of the string. */
NAME("/string")
{
DEF_CA
MAYBE_UNUSED Char * c_addr1;
MAYBE_UNUSED UCell u1;
MAYBE_UNUSED Cell n;
Char * c_addr2;
UCell u2;
NEXT_P0;
vm_Cell2c_(sp[2],c_addr1);
vm_Cell2u(sp[1],u1);
vm_Cell2n(spTOS,n);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_addr1=", vm_out); printarg_c_(c_addr1);
fputs(" u1=", vm_out); printarg_u(u1);
fputs(" n=", vm_out); printarg_n(n);
}
#endif
sp += 1;
{
#line 718 "prim"
c_addr2 = c_addr1+n;
u2 = u1-n;
#line 3012 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" c_addr2=", vm_out); printarg_c_(c_addr2);
fputs(" u2=", vm_out); printarg_u(u2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_c_2Cell(c_addr2,sp[1]);
vm_u2Cell(u2,spTOS);
LABEL2(slash_string)
NEXT_P1_5;
LABEL3(slash_string)
DO_GOTO;
}

GROUPADD(8)
GROUP( arith, 52)
LABEL(lit) /* lit ( #w -- w ) S1 -- S1  */
/*  */
NAME("lit")
{
DEF_CA
MAYBE_UNUSED Cell w;
NEXT_P0;
sp[0] = spTOS;
vm_Cell2w(IMM_ARG(IPTOS,305397791 ),w);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w=", vm_out); printarg_w(w);
}
#endif
INC_IP(1);
sp += -1;
{
#line 726 "prim"
#line 3051 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w,spTOS);
LABEL2(lit)
NEXT_P1_5;
LABEL3(lit)
DO_GOTO;
}

LABEL(plus) /* + ( n1 n2 -- n ) S1 -- S1  */
/*  */
NAME("+")
{
DEF_CA
MAYBE_UNUSED Cell n1;
MAYBE_UNUSED Cell n2;
Cell n;
NEXT_P0;
vm_Cell2n(sp[1],n1);
vm_Cell2n(spTOS,n2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n1=", vm_out); printarg_n(n1);
fputs(" n2=", vm_out); printarg_n(n2);
}
#endif
sp += 1;
{
#line 730 "prim"
n = n1+n2;
#line 3088 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n=", vm_out); printarg_n(n);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n,spTOS);
LABEL2(plus)
NEXT_P1_5;
LABEL3(plus)
DO_GOTO;
}

LABEL(lit_plus) /* lit+ ( n1 #n2 -- n ) S1 -- S1  */
/*  */
NAME("lit+")
{
DEF_CA
MAYBE_UNUSED Cell n1;
MAYBE_UNUSED Cell n2;
Cell n;
NEXT_P0;
vm_Cell2n(spTOS,n1);
vm_Cell2n(IMM_ARG(IPTOS,305397792 ),n2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n1=", vm_out); printarg_n(n1);
fputs(" n2=", vm_out); printarg_n(n2);
}
#endif
INC_IP(1);
{
#line 735 "prim"
#ifdef DEBUG
fprintf(stderr, "lit+ %08x\n", n2);
#endif
n=n1+n2;
#line 3129 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n=", vm_out); printarg_n(n);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n,spTOS);
LABEL2(lit_plus)
NEXT_P1_5;
LABEL3(lit_plus)
DO_GOTO;
}

LABEL(under_plus) /* under+ ( n1 n2 n3 -- n n2 ) S1 -- S1  */
/* add @i{n3} to @i{n1} (giving @i{n}) */
NAME("under+")
{
DEF_CA
MAYBE_UNUSED Cell n1;
MAYBE_UNUSED Cell n2;
MAYBE_UNUSED Cell n3;
Cell n;
NEXT_P0;
vm_Cell2n(sp[2],n1);
vm_Cell2n(sp[1],n2);
vm_Cell2n(spTOS,n3);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n1=", vm_out); printarg_n(n1);
fputs(" n2=", vm_out); printarg_n(n2);
fputs(" n3=", vm_out); printarg_n(n3);
}
#endif
sp += 1;
{
#line 743 "prim"
n = n1+n3;
#line 3170 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n=", vm_out); printarg_n(n);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n,sp[1]);
vm_n2Cell(n2,spTOS);
LABEL2(under_plus)
NEXT_P1_5;
LABEL3(under_plus)
DO_GOTO;
}

LABEL(minus) /* - ( n1 n2 -- n ) S1 -- S1  */
/*  */
NAME("-")
{
DEF_CA
MAYBE_UNUSED Cell n1;
MAYBE_UNUSED Cell n2;
Cell n;
NEXT_P0;
vm_Cell2n(sp[1],n1);
vm_Cell2n(spTOS,n2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n1=", vm_out); printarg_n(n1);
fputs(" n2=", vm_out); printarg_n(n2);
}
#endif
sp += 1;
{
#line 748 "prim"
n = n1-n2;
#line 3209 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n=", vm_out); printarg_n(n);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n,spTOS);
LABEL2(minus)
NEXT_P1_5;
LABEL3(minus)
DO_GOTO;
}

LABEL(negate) /* negate ( n1 -- n2 ) S1 -- S1  */
/*  */
NAME("negate")
{
DEF_CA
MAYBE_UNUSED Cell n1;
Cell n2;
NEXT_P0;
vm_Cell2n(spTOS,n1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n1=", vm_out); printarg_n(n1);
}
#endif
{
#line 753 "prim"
/* use minus as alias */
n2 = -n1;
#line 3244 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n2=", vm_out); printarg_n(n2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n2,spTOS);
LABEL2(negate)
NEXT_P1_5;
LABEL3(negate)
DO_GOTO;
}

LABEL(one_plus) /* 1+ ( n1 -- n2 ) S1 -- S1  */
/*  */
NAME("1+")
{
DEF_CA
MAYBE_UNUSED Cell n1;
Cell n2;
NEXT_P0;
vm_Cell2n(spTOS,n1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n1=", vm_out); printarg_n(n1);
}
#endif
{
#line 759 "prim"
n2 = n1+1;
#line 3278 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n2=", vm_out); printarg_n(n2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n2,spTOS);
LABEL2(one_plus)
NEXT_P1_5;
LABEL3(one_plus)
DO_GOTO;
}

LABEL(one_minus) /* 1- ( n1 -- n2 ) S1 -- S1  */
/*  */
NAME("1-")
{
DEF_CA
MAYBE_UNUSED Cell n1;
Cell n2;
NEXT_P0;
vm_Cell2n(spTOS,n1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n1=", vm_out); printarg_n(n1);
}
#endif
{
#line 764 "prim"
n2 = n1-1;
#line 3312 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n2=", vm_out); printarg_n(n2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n2,spTOS);
LABEL2(one_minus)
NEXT_P1_5;
LABEL3(one_minus)
DO_GOTO;
}

LABEL(max) /* max ( n1 n2 -- n ) S1 -- S1  */
/*  */
NAME("max")
{
DEF_CA
MAYBE_UNUSED Cell n1;
MAYBE_UNUSED Cell n2;
Cell n;
NEXT_P0;
vm_Cell2n(sp[1],n1);
vm_Cell2n(spTOS,n2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n1=", vm_out); printarg_n(n1);
fputs(" n2=", vm_out); printarg_n(n2);
}
#endif
sp += 1;
{
#line 769 "prim"
if (n1<n2)
  n = n2;
else
  n = n1;
#line 3353 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n=", vm_out); printarg_n(n);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n,spTOS);
LABEL2(max)
NEXT_P1_5;
LABEL3(max)
DO_GOTO;
}

LABEL(min) /* min ( n1 n2 -- n ) S1 -- S1  */
/*  */
NAME("min")
{
DEF_CA
MAYBE_UNUSED Cell n1;
MAYBE_UNUSED Cell n2;
Cell n;
NEXT_P0;
vm_Cell2n(sp[1],n1);
vm_Cell2n(spTOS,n2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n1=", vm_out); printarg_n(n1);
fputs(" n2=", vm_out); printarg_n(n2);
}
#endif
sp += 1;
{
#line 777 "prim"
if (n1<n2)
  n = n1;
else
  n = n2;
#line 3394 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n=", vm_out); printarg_n(n);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n,spTOS);
LABEL2(min)
NEXT_P1_5;
LABEL3(min)
DO_GOTO;
}

LABEL(abs) /* abs ( n -- u ) S1 -- S1  */
/*  */
NAME("abs")
{
DEF_CA
MAYBE_UNUSED Cell n;
UCell u;
NEXT_P0;
vm_Cell2n(spTOS,n);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n=", vm_out); printarg_n(n);
}
#endif
{
#line 785 "prim"
if (n<0)
  u = -n;
else
  u = n;
#line 3431 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" u=", vm_out); printarg_u(u);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_u2Cell(u,spTOS);
LABEL2(abs)
NEXT_P1_5;
LABEL3(abs)
DO_GOTO;
}

LABEL(star) /* * ( n1 n2 -- n ) S1 -- S1  */
/*  */
NAME("*")
{
DEF_CA
MAYBE_UNUSED Cell n1;
MAYBE_UNUSED Cell n2;
Cell n;
NEXT_P0;
vm_Cell2n(sp[1],n1);
vm_Cell2n(spTOS,n2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n1=", vm_out); printarg_n(n1);
fputs(" n2=", vm_out); printarg_n(n2);
}
#endif
sp += 1;
{
#line 793 "prim"
n = n1*n2;
#line 3469 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n=", vm_out); printarg_n(n);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n,spTOS);
LABEL2(star)
NEXT_P1_5;
LABEL3(star)
DO_GOTO;
}

LABEL(slash) /* / ( n1 n2 -- n ) S1 -- S1  */
/*  */
NAME("/")
{
DEF_CA
MAYBE_UNUSED Cell n1;
MAYBE_UNUSED Cell n2;
Cell n;
NEXT_P0;
vm_Cell2n(sp[1],n1);
vm_Cell2n(spTOS,n2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n1=", vm_out); printarg_n(n1);
fputs(" n2=", vm_out); printarg_n(n2);
}
#endif
sp += 1;
{
#line 798 "prim"
n = n1/n2;
if (CHECK_DIVISION_SW && n2 == 0)
  throw(BALL_DIVZERO);
if (CHECK_DIVISION_SW && n2 == -1 && n1 == CELL_MIN)
  throw(BALL_RESULTRANGE);
if (FLOORED_DIV && ((n1^n2) < 0) && (n1%n2 != 0))
  n--;
#line 3513 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n=", vm_out); printarg_n(n);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n,spTOS);
LABEL2(slash)
NEXT_P1_5;
LABEL3(slash)
DO_GOTO;
}

LABEL(mod) /* mod ( n1 n2 -- n ) S1 -- S1  */
/*  */
NAME("mod")
{
DEF_CA
MAYBE_UNUSED Cell n1;
MAYBE_UNUSED Cell n2;
Cell n;
NEXT_P0;
vm_Cell2n(sp[1],n1);
vm_Cell2n(spTOS,n2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n1=", vm_out); printarg_n(n1);
fputs(" n2=", vm_out); printarg_n(n2);
}
#endif
sp += 1;
{
#line 809 "prim"
n = n1%n2;
if (CHECK_DIVISION_SW && n2 == 0)
  throw(BALL_DIVZERO);
if (CHECK_DIVISION_SW && n2 == -1 && n1 == CELL_MIN)
  throw(BALL_RESULTRANGE);
if(FLOORED_DIV && ((n1^n2) < 0) && n!=0) n += n2;
#line 3556 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n=", vm_out); printarg_n(n);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n,spTOS);
LABEL2(mod)
NEXT_P1_5;
LABEL3(mod)
DO_GOTO;
}

LABEL(slash_mod) /* /mod ( n1 n2 -- n3 n4 ) S1 -- S1  */
/*  */
NAME("/mod")
{
DEF_CA
MAYBE_UNUSED Cell n1;
MAYBE_UNUSED Cell n2;
Cell n3;
Cell n4;
NEXT_P0;
vm_Cell2n(sp[1],n1);
vm_Cell2n(spTOS,n2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n1=", vm_out); printarg_n(n1);
fputs(" n2=", vm_out); printarg_n(n2);
}
#endif
{
#line 819 "prim"
n4 = n1/n2;
n3 = n1%n2; /* !! is this correct? look into C standard! */
if (CHECK_DIVISION_SW && n2 == 0)
  throw(BALL_DIVZERO);
if (CHECK_DIVISION_SW && n2 == -1 && n1 == CELL_MIN)
  throw(BALL_RESULTRANGE);
if (FLOORED_DIV && ((n1^n2) < 0) && n3!=0) {
  n4--;
  n3+=n2;
}
#line 3603 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n3=", vm_out); printarg_n(n3);
fputs(" n4=", vm_out); printarg_n(n4);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n3,sp[1]);
vm_n2Cell(n4,spTOS);
LABEL2(slash_mod)
NEXT_P1_5;
LABEL3(slash_mod)
DO_GOTO;
}

LABEL(star_slash_mod) /* x/mod ( n1 n2 n3 -- n4 n5 ) S1 -- S1  */
/* n1*n2=n3*n5+n4, with the intermediate result (n1*n2) being double. */
NAME("*/mod")
{
DEF_CA
MAYBE_UNUSED Cell n1;
MAYBE_UNUSED Cell n2;
MAYBE_UNUSED Cell n3;
Cell n4;
Cell n5;
NEXT_P0;
vm_Cell2n(sp[2],n1);
vm_Cell2n(sp[1],n2);
vm_Cell2n(spTOS,n3);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n1=", vm_out); printarg_n(n1);
fputs(" n2=", vm_out); printarg_n(n2);
fputs(" n3=", vm_out); printarg_n(n3);
}
#endif
sp += 1;
{
#line 834 "prim"
#ifdef BUGGY_LL_MUL
DCell d = mmul(n1,n2);
#else
DCell d = (DCell)n1 * (DCell)n2;
#endif
#ifdef ASM_SM_SLASH_REM
ASM_SM_SLASH_REM(DLO(d), DHI(d), n3, n4, n5);
if (FLOORED_DIV && ((DHI(d)^n3)<0) && n4!=0) {
  if (CHECK_DIVISION && n5 == CELL_MIN)
    throw(BALL_RESULTRANGE);
  n5--;
  n4+=n3;
}
#else
DCell r = FLOORED_DIV ? fmdiv(d,n3) : smdiv(d,n3);
n4=DHI(r);
n5=DLO(r);
#endif
#line 3664 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n4=", vm_out); printarg_n(n4);
fputs(" n5=", vm_out); printarg_n(n5);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n4,sp[1]);
vm_n2Cell(n5,spTOS);
LABEL2(star_slash_mod)
NEXT_P1_5;
LABEL3(star_slash_mod)
DO_GOTO;
}

LABEL(star_slash) /* x/ ( n1 n2 n3 -- n4 ) S1 -- S1  */
/* n4=(n1*n2)/n3, with the intermediate result being double. */
NAME("*/")
{
DEF_CA
MAYBE_UNUSED Cell n1;
MAYBE_UNUSED Cell n2;
MAYBE_UNUSED Cell n3;
Cell n4;
NEXT_P0;
vm_Cell2n(sp[2],n1);
vm_Cell2n(sp[1],n2);
vm_Cell2n(spTOS,n3);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n1=", vm_out); printarg_n(n1);
fputs(" n2=", vm_out); printarg_n(n2);
fputs(" n3=", vm_out); printarg_n(n3);
}
#endif
sp += 2;
{
#line 857 "prim"
#ifdef BUGGY_LL_MUL
DCell d = mmul(n1,n2);
#else
DCell d = (DCell)n1 * (DCell)n2;
#endif
#ifdef ASM_SM_SLASH_REM
Cell remainder;
ASM_SM_SLASH_REM(DLO(d), DHI(d), n3, remainder, n4);
if (FLOORED_DIV && ((DHI(d)^n3)<0) && remainder!=0) {
  if (CHECK_DIVISION && n4 == CELL_MIN)
    throw(BALL_RESULTRANGE);
  n4--;
}
#else
DCell r = FLOORED_DIV ? fmdiv(d,n3) : smdiv(d,n3);
n4=DLO(r);
#endif
#line 3723 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n4=", vm_out); printarg_n(n4);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n4,spTOS);
LABEL2(star_slash)
NEXT_P1_5;
LABEL3(star_slash)
DO_GOTO;
}

LABEL(two_star) /* 2* ( n1 -- n2 ) S1 -- S1  */
/* Shift left by 1; also works on unsigned numbers */
NAME("2*")
{
DEF_CA
MAYBE_UNUSED Cell n1;
Cell n2;
NEXT_P0;
vm_Cell2n(spTOS,n1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n1=", vm_out); printarg_n(n1);
}
#endif
{
#line 879 "prim"
n2 = 2*n1;
#line 3757 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n2=", vm_out); printarg_n(n2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n2,spTOS);
LABEL2(two_star)
NEXT_P1_5;
LABEL3(two_star)
DO_GOTO;
}

LABEL(two_slash) /* 2/ ( n1 -- n2 ) S1 -- S1  */
/* Arithmetic shift right by 1.  For signed numbers this is a floored
division by 2 (note that @code{/} not necessarily floors). */
NAME("2/")
{
DEF_CA
MAYBE_UNUSED Cell n1;
Cell n2;
NEXT_P0;
vm_Cell2n(spTOS,n1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n1=", vm_out); printarg_n(n1);
}
#endif
{
#line 886 "prim"
n2 = n1>>1;
#line 3792 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n2=", vm_out); printarg_n(n2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n2,spTOS);
LABEL2(two_slash)
NEXT_P1_5;
LABEL3(two_slash)
DO_GOTO;
}

LABEL(f_m_slash_mod) /* fm/mod ( d1 n1 -- n2 n3 ) S1 -- S1  */
/* Floored division: @i{d1} = @i{n3}*@i{n1}+@i{n2}, @i{n1}>@i{n2}>=0 or 0>=@i{n2}>@i{n1}. */
NAME("fm/mod")
{
DEF_CA
MAYBE_UNUSED DCell d1;
MAYBE_UNUSED Cell n1;
Cell n2;
Cell n3;
NEXT_P0;
vm_twoCell2d(sp[2], sp[1], d1)
vm_Cell2n(spTOS,n1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" d1=", vm_out); printarg_d(d1);
fputs(" n1=", vm_out); printarg_n(n1);
}
#endif
sp += 1;
{
#line 896 "prim"
#ifdef ASM_SM_SLASH_REM
ASM_SM_SLASH_REM(DLO(d1), DHI(d1), n1, n2, n3);
if (((DHI(d1)^n1)<0) && n2!=0) {
  if (CHECK_DIVISION && n3 == CELL_MIN)
    throw(BALL_RESULTRANGE);
  n3--;
  n2+=n1;
}
#else /* !defined(ASM_SM_SLASH_REM) */
DCell r = fmdiv(d1,n1);
n2=DHI(r);
n3=DLO(r);
#endif /* !defined(ASM_SM_SLASH_REM) */
#line 3843 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n2=", vm_out); printarg_n(n2);
fputs(" n3=", vm_out); printarg_n(n3);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n2,sp[1]);
vm_n2Cell(n3,spTOS);
LABEL2(f_m_slash_mod)
NEXT_P1_5;
LABEL3(f_m_slash_mod)
DO_GOTO;
}

LABEL(s_m_slash_rem) /* sm/rem ( d1 n1 -- n2 n3 ) S1 -- S1  */
/* Symmetric division: @i{d1} = @i{n3}*@i{n1}+@i{n2}, sign(@i{n2})=sign(@i{d1}) or 0. */
NAME("sm/rem")
{
DEF_CA
MAYBE_UNUSED DCell d1;
MAYBE_UNUSED Cell n1;
Cell n2;
Cell n3;
NEXT_P0;
vm_twoCell2d(sp[2], sp[1], d1)
vm_Cell2n(spTOS,n1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" d1=", vm_out); printarg_d(d1);
fputs(" n1=", vm_out); printarg_n(n1);
}
#endif
sp += 1;
{
#line 917 "prim"
#ifdef ASM_SM_SLASH_REM
ASM_SM_SLASH_REM(DLO(d1), DHI(d1), n1, n2, n3);
#else /* !defined(ASM_SM_SLASH_REM) */
DCell r = smdiv(d1,n1);
n2=DHI(r);
n3=DLO(r);
#endif /* !defined(ASM_SM_SLASH_REM) */
#line 3890 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n2=", vm_out); printarg_n(n2);
fputs(" n3=", vm_out); printarg_n(n3);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n2,sp[1]);
vm_n2Cell(n3,spTOS);
LABEL2(s_m_slash_rem)
NEXT_P1_5;
LABEL3(s_m_slash_rem)
DO_GOTO;
}

LABEL(m_star) /* m* ( n1 n2 -- d ) S1 -- S1  */
/*  */
NAME("m*")
{
DEF_CA
MAYBE_UNUSED Cell n1;
MAYBE_UNUSED Cell n2;
DCell d;
NEXT_P0;
vm_Cell2n(sp[1],n1);
vm_Cell2n(spTOS,n2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n1=", vm_out); printarg_n(n1);
fputs(" n2=", vm_out); printarg_n(n2);
}
#endif
{
#line 931 "prim"
#ifdef BUGGY_LL_MUL
d = mmul(n1,n2);
#else
d = (DCell)n1 * (DCell)n2;
#endif
#line 3933 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" d=", vm_out); printarg_d(d);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_d2twoCell(d, sp[1], spTOS)
LABEL2(m_star)
NEXT_P1_5;
LABEL3(m_star)
DO_GOTO;
}

LABEL(u_m_star) /* um* ( u1 u2 -- ud ) S1 -- S1  */
/*  */
NAME("um*")
{
DEF_CA
MAYBE_UNUSED UCell u1;
MAYBE_UNUSED UCell u2;
UDCell ud;
NEXT_P0;
vm_Cell2u(sp[1],u1);
vm_Cell2u(spTOS,u2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" u1=", vm_out); printarg_u(u1);
fputs(" u2=", vm_out); printarg_u(u2);
}
#endif
{
#line 942 "prim"
/* use u* as alias */
#ifdef BUGGY_LL_MUL
ud = ummul(u1,u2);
#else
ud = (UDCell)u1 * (UDCell)u2;
#endif
#line 3975 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" ud=", vm_out); printarg_ud(ud);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_ud2twoCell(ud, sp[1], spTOS)
LABEL2(u_m_star)
NEXT_P1_5;
LABEL3(u_m_star)
DO_GOTO;
}

LABEL(u_m_slash_mod) /* um/mod ( ud u1 -- u2 u3 ) S1 -- S1  */
/* ud=u3*u1+u2, u1>u2>=0 */
NAME("um/mod")
{
DEF_CA
MAYBE_UNUSED UDCell ud;
MAYBE_UNUSED UCell u1;
UCell u2;
UCell u3;
NEXT_P0;
vm_twoCell2ud(sp[2], sp[1], ud)
vm_Cell2u(spTOS,u1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" ud=", vm_out); printarg_ud(ud);
fputs(" u1=", vm_out); printarg_u(u1);
}
#endif
sp += 1;
{
#line 959 "prim"
#ifdef ASM_UM_SLASH_MOD
ASM_UM_SLASH_MOD(DLO(ud), DHI(ud), u1, u2, u3);
#else /* !defined(ASM_UM_SLASH_MOD) */
UDCell r = umdiv(ud,u1);
u2=DHI(r);
u3=DLO(r);
#endif /* !defined(ASM_UM_SLASH_MOD) */
#line 4020 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" u2=", vm_out); printarg_u(u2);
fputs(" u3=", vm_out); printarg_u(u3);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_u2Cell(u2,sp[1]);
vm_u2Cell(u3,spTOS);
LABEL2(u_m_slash_mod)
NEXT_P1_5;
LABEL3(u_m_slash_mod)
DO_GOTO;
}

LABEL(m_plus) /* m+ ( d1 n -- d2 ) S1 -- S1  */
/*  */
NAME("m+")
{
DEF_CA
MAYBE_UNUSED DCell d1;
MAYBE_UNUSED Cell n;
DCell d2;
NEXT_P0;
vm_twoCell2d(sp[2], sp[1], d1)
vm_Cell2n(spTOS,n);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" d1=", vm_out); printarg_d(d1);
fputs(" n=", vm_out); printarg_n(n);
}
#endif
sp += 1;
{
#line 977 "prim"
#ifdef BUGGY_LL_ADD
DLO_IS(d2, DLO(d1)+n);
DHI_IS(d2, DHI(d1) - (n<0) + (DLO(d2)<DLO(d1)));
#else
d2 = d1+n;
#endif
#line 4065 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" d2=", vm_out); printarg_d(d2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_d2twoCell(d2, sp[1], spTOS)
LABEL2(m_plus)
NEXT_P1_5;
LABEL3(m_plus)
DO_GOTO;
}

LABEL(d_plus) /* d+ ( d1 d2 -- d ) S1 -- S1  */
/*  */
NAME("d+")
{
DEF_CA
MAYBE_UNUSED DCell d1;
MAYBE_UNUSED DCell d2;
DCell d;
NEXT_P0;
vm_twoCell2d(sp[3], sp[2], d1)
vm_twoCell2d(sp[1], spTOS, d2)
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" d1=", vm_out); printarg_d(d1);
fputs(" d2=", vm_out); printarg_d(d2);
}
#endif
sp += 2;
{
#line 987 "prim"
#ifdef BUGGY_LL_ADD
DLO_IS(d, DLO(d1) + DLO(d2));
DHI_IS(d, DHI(d1) + DHI(d2) + (d.lo<DLO(d1)));
#else
d = d1+d2;
#endif
#line 4108 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" d=", vm_out); printarg_d(d);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_d2twoCell(d, sp[1], spTOS)
LABEL2(d_plus)
NEXT_P1_5;
LABEL3(d_plus)
DO_GOTO;
}

LABEL(d_minus) /* d- ( d1 d2 -- d ) S1 -- S1  */
/*  */
NAME("d-")
{
DEF_CA
MAYBE_UNUSED DCell d1;
MAYBE_UNUSED DCell d2;
DCell d;
NEXT_P0;
vm_twoCell2d(sp[3], sp[2], d1)
vm_twoCell2d(sp[1], spTOS, d2)
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" d1=", vm_out); printarg_d(d1);
fputs(" d2=", vm_out); printarg_d(d2);
}
#endif
sp += 2;
{
#line 997 "prim"
#ifdef BUGGY_LL_ADD
DLO_IS(d, DLO(d1) - DLO(d2));
DHI_IS(d, DHI(d1)-DHI(d2)-(DLO(d1)<DLO(d2)));
#else
d = d1-d2;
#endif
#line 4151 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" d=", vm_out); printarg_d(d);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_d2twoCell(d, sp[1], spTOS)
LABEL2(d_minus)
NEXT_P1_5;
LABEL3(d_minus)
DO_GOTO;
}

LABEL(d_negate) /* dnegate ( d1 -- d2 ) S1 -- S1  */
/*  */
NAME("dnegate")
{
DEF_CA
MAYBE_UNUSED DCell d1;
DCell d2;
NEXT_P0;
vm_twoCell2d(sp[1], spTOS, d1)
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" d1=", vm_out); printarg_d(d1);
}
#endif
{
#line 1007 "prim"
/* use dminus as alias */
#ifdef BUGGY_LL_ADD
d2 = dnegate(d1);
#else
d2 = -d1;
#endif
#line 4190 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" d2=", vm_out); printarg_d(d2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_d2twoCell(d2, sp[1], spTOS)
LABEL2(d_negate)
NEXT_P1_5;
LABEL3(d_negate)
DO_GOTO;
}

LABEL(d_two_star) /* d2* ( d1 -- d2 ) S1 -- S1  */
/* Shift left by 1; also works on unsigned numbers */
NAME("d2*")
{
DEF_CA
MAYBE_UNUSED DCell d1;
DCell d2;
NEXT_P0;
vm_twoCell2d(sp[1], spTOS, d1)
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" d1=", vm_out); printarg_d(d1);
}
#endif
{
#line 1018 "prim"
d2 = DLSHIFT(d1,1);
#line 4224 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" d2=", vm_out); printarg_d(d2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_d2twoCell(d2, sp[1], spTOS)
LABEL2(d_two_star)
NEXT_P1_5;
LABEL3(d_two_star)
DO_GOTO;
}

LABEL(d_two_slash) /* d2/ ( d1 -- d2 ) S1 -- S1  */
/* Arithmetic shift right by 1.  For signed numbers this is a floored
division by 2. */
NAME("d2/")
{
DEF_CA
MAYBE_UNUSED DCell d1;
DCell d2;
NEXT_P0;
vm_twoCell2d(sp[1], spTOS, d1)
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" d1=", vm_out); printarg_d(d1);
}
#endif
{
#line 1025 "prim"
#ifdef BUGGY_LL_SHIFT
DHI_IS(d2, DHI(d1)>>1);
DLO_IS(d2, (DLO(d1)>>1) | (DHI(d1)<<(CELL_BITS-1)));
#else
d2 = d1>>1;
#endif
#line 4264 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" d2=", vm_out); printarg_d(d2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_d2twoCell(d2, sp[1], spTOS)
LABEL2(d_two_slash)
NEXT_P1_5;
LABEL3(d_two_slash)
DO_GOTO;
}

LABEL(and) /* and ( w1 w2 -- w ) S1 -- S1  */
/*  */
NAME("and")
{
DEF_CA
MAYBE_UNUSED Cell w1;
MAYBE_UNUSED Cell w2;
Cell w;
NEXT_P0;
vm_Cell2w(sp[1],w1);
vm_Cell2w(spTOS,w2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w1=", vm_out); printarg_w(w1);
fputs(" w2=", vm_out); printarg_w(w2);
}
#endif
sp += 1;
{
#line 1036 "prim"
w = w1&w2;
#line 4302 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" w=", vm_out); printarg_w(w);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w,spTOS);
LABEL2(and)
NEXT_P1_5;
LABEL3(and)
DO_GOTO;
}

LABEL(or) /* or ( w1 w2 -- w ) S1 -- S1  */
/*  */
NAME("or")
{
DEF_CA
MAYBE_UNUSED Cell w1;
MAYBE_UNUSED Cell w2;
Cell w;
NEXT_P0;
vm_Cell2w(sp[1],w1);
vm_Cell2w(spTOS,w2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w1=", vm_out); printarg_w(w1);
fputs(" w2=", vm_out); printarg_w(w2);
}
#endif
sp += 1;
{
#line 1039 "prim"
w = w1|w2;
#line 4340 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" w=", vm_out); printarg_w(w);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w,spTOS);
LABEL2(or)
NEXT_P1_5;
LABEL3(or)
DO_GOTO;
}

LABEL(x_or) /* xor ( w1 w2 -- w ) S1 -- S1  */
/*  */
NAME("xor")
{
DEF_CA
MAYBE_UNUSED Cell w1;
MAYBE_UNUSED Cell w2;
Cell w;
NEXT_P0;
vm_Cell2w(sp[1],w1);
vm_Cell2w(spTOS,w2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w1=", vm_out); printarg_w(w1);
fputs(" w2=", vm_out); printarg_w(w2);
}
#endif
sp += 1;
{
#line 1044 "prim"
w = w1^w2;
#line 4378 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" w=", vm_out); printarg_w(w);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w,spTOS);
LABEL2(x_or)
NEXT_P1_5;
LABEL3(x_or)
DO_GOTO;
}

LABEL(invert) /* invert ( w1 -- w2 ) S1 -- S1  */
/*  */
NAME("invert")
{
DEF_CA
MAYBE_UNUSED Cell w1;
Cell w2;
NEXT_P0;
vm_Cell2w(spTOS,w1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w1=", vm_out); printarg_w(w1);
}
#endif
{
#line 1047 "prim"
w2 = ~w1;
#line 4412 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" w2=", vm_out); printarg_w(w2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w2,spTOS);
LABEL2(invert)
NEXT_P1_5;
LABEL3(invert)
DO_GOTO;
}

LABEL(r_shift) /* rshift ( u1 n -- u2 ) S1 -- S1  */
/* Logical shift right by @i{n} bits. */
NAME("rshift")
{
DEF_CA
MAYBE_UNUSED UCell u1;
MAYBE_UNUSED Cell n;
UCell u2;
NEXT_P0;
vm_Cell2u(sp[1],u1);
vm_Cell2n(spTOS,n);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" u1=", vm_out); printarg_u(u1);
fputs(" n=", vm_out); printarg_n(n);
}
#endif
sp += 1;
{
#line 1053 "prim"
#ifdef BROKEN_SHIFT
  u2 = rshift(u1, n);
#else
  u2 = u1 >> n;
#endif
#line 4454 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" u2=", vm_out); printarg_u(u2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_u2Cell(u2,spTOS);
LABEL2(r_shift)
NEXT_P1_5;
LABEL3(r_shift)
DO_GOTO;
}

LABEL(l_shift) /* lshift ( u1 n -- u2 ) S1 -- S1  */
/*  */
NAME("lshift")
{
DEF_CA
MAYBE_UNUSED UCell u1;
MAYBE_UNUSED Cell n;
UCell u2;
NEXT_P0;
vm_Cell2u(sp[1],u1);
vm_Cell2n(spTOS,n);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" u1=", vm_out); printarg_u(u1);
fputs(" n=", vm_out); printarg_n(n);
}
#endif
sp += 1;
{
#line 1062 "prim"
#ifdef BROKEN_SHIFT
  u2 = lshift(u1, n);
#else
  u2 = u1 << n;
#endif
#line 4496 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" u2=", vm_out); printarg_u(u2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_u2Cell(u2,spTOS);
LABEL2(l_shift)
NEXT_P1_5;
LABEL3(l_shift)
DO_GOTO;
}

GROUPADD(36)
GROUP( compare, 88)
LABEL(zero_equals) /* 0= ( n -- f ) S1 -- S1  */
/*  */
NAME("0=")
{
DEF_CA
MAYBE_UNUSED Cell n;
Bool f;
NEXT_P0;
vm_Cell2n(spTOS,n);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n=", vm_out); printarg_n(n);
}
#endif
{
#line 1123 "prim"
f = FLAG(n==0);
#line 1122
#line 4533 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
LABEL2(zero_equals)
NEXT_P1_5;
LABEL3(zero_equals)
DO_GOTO;
}

LABEL(zero_not_equals) /* 0<> ( n -- f ) S1 -- S1  */
/*  */
NAME("0<>")
{
DEF_CA
MAYBE_UNUSED Cell n;
Bool f;
NEXT_P0;
vm_Cell2n(spTOS,n);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n=", vm_out); printarg_n(n);
}
#endif
{
#line 1123 "prim"
f = FLAG(n!=0);
#line 1122
#line 4568 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
LABEL2(zero_not_equals)
NEXT_P1_5;
LABEL3(zero_not_equals)
DO_GOTO;
}

LABEL(zero_less_than) /* 0< ( n -- f ) S1 -- S1  */
/*  */
NAME("0<")
{
DEF_CA
MAYBE_UNUSED Cell n;
Bool f;
NEXT_P0;
vm_Cell2n(spTOS,n);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n=", vm_out); printarg_n(n);
}
#endif
{
#line 1123 "prim"
f = FLAG(n<0);
#line 1122
#line 4603 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
LABEL2(zero_less_than)
NEXT_P1_5;
LABEL3(zero_less_than)
DO_GOTO;
}

LABEL(zero_greater_than) /* 0> ( n -- f ) S1 -- S1  */
/*  */
NAME("0>")
{
DEF_CA
MAYBE_UNUSED Cell n;
Bool f;
NEXT_P0;
vm_Cell2n(spTOS,n);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n=", vm_out); printarg_n(n);
}
#endif
{
#line 1123 "prim"
f = FLAG(n>0);
#line 1122
#line 4638 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
LABEL2(zero_greater_than)
NEXT_P1_5;
LABEL3(zero_greater_than)
DO_GOTO;
}

LABEL(zero_less_or_equal) /* 0<= ( n -- f ) S1 -- S1  */
/*  */
NAME("0<=")
{
DEF_CA
MAYBE_UNUSED Cell n;
Bool f;
NEXT_P0;
vm_Cell2n(spTOS,n);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n=", vm_out); printarg_n(n);
}
#endif
{
#line 1123 "prim"
f = FLAG(n<=0);
#line 1122
#line 4673 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
LABEL2(zero_less_or_equal)
NEXT_P1_5;
LABEL3(zero_less_or_equal)
DO_GOTO;
}

LABEL(zero_greater_or_equal) /* 0>= ( n -- f ) S1 -- S1  */
/*  */
NAME("0>=")
{
DEF_CA
MAYBE_UNUSED Cell n;
Bool f;
NEXT_P0;
vm_Cell2n(spTOS,n);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n=", vm_out); printarg_n(n);
}
#endif
{
#line 1123 "prim"
f = FLAG(n>=0);
#line 1122
#line 4708 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
LABEL2(zero_greater_or_equal)
NEXT_P1_5;
LABEL3(zero_greater_or_equal)
DO_GOTO;
}

LABEL(equals) /* = ( n1 n2 -- f ) S1 -- S1  */
/*  */
NAME("=")
{
DEF_CA
MAYBE_UNUSED Cell n1;
MAYBE_UNUSED Cell n2;
Bool f;
NEXT_P0;
vm_Cell2n(sp[1],n1);
vm_Cell2n(spTOS,n2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n1=", vm_out); printarg_n(n1);
fputs(" n2=", vm_out); printarg_n(n2);
}
#endif
sp += 1;
{
#line 1124 "prim"
f = FLAG(n1==n2);
#line 1123
#line 4747 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
LABEL2(equals)
NEXT_P1_5;
LABEL3(equals)
DO_GOTO;
}

LABEL(not_equals) /* <> ( n1 n2 -- f ) S1 -- S1  */
/*  */
NAME("<>")
{
DEF_CA
MAYBE_UNUSED Cell n1;
MAYBE_UNUSED Cell n2;
Bool f;
NEXT_P0;
vm_Cell2n(sp[1],n1);
vm_Cell2n(spTOS,n2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n1=", vm_out); printarg_n(n1);
fputs(" n2=", vm_out); printarg_n(n2);
}
#endif
sp += 1;
{
#line 1124 "prim"
f = FLAG(n1!=n2);
#line 1123
#line 4786 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
LABEL2(not_equals)
NEXT_P1_5;
LABEL3(not_equals)
DO_GOTO;
}

LABEL(less_than) /* < ( n1 n2 -- f ) S1 -- S1  */
/*  */
NAME("<")
{
DEF_CA
MAYBE_UNUSED Cell n1;
MAYBE_UNUSED Cell n2;
Bool f;
NEXT_P0;
vm_Cell2n(sp[1],n1);
vm_Cell2n(spTOS,n2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n1=", vm_out); printarg_n(n1);
fputs(" n2=", vm_out); printarg_n(n2);
}
#endif
sp += 1;
{
#line 1124 "prim"
f = FLAG(n1<n2);
#line 1123
#line 4825 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
LABEL2(less_than)
NEXT_P1_5;
LABEL3(less_than)
DO_GOTO;
}

LABEL(greater_than) /* > ( n1 n2 -- f ) S1 -- S1  */
/*  */
NAME(">")
{
DEF_CA
MAYBE_UNUSED Cell n1;
MAYBE_UNUSED Cell n2;
Bool f;
NEXT_P0;
vm_Cell2n(sp[1],n1);
vm_Cell2n(spTOS,n2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n1=", vm_out); printarg_n(n1);
fputs(" n2=", vm_out); printarg_n(n2);
}
#endif
sp += 1;
{
#line 1124 "prim"
f = FLAG(n1>n2);
#line 1123
#line 4864 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
LABEL2(greater_than)
NEXT_P1_5;
LABEL3(greater_than)
DO_GOTO;
}

LABEL(less_or_equal) /* <= ( n1 n2 -- f ) S1 -- S1  */
/*  */
NAME("<=")
{
DEF_CA
MAYBE_UNUSED Cell n1;
MAYBE_UNUSED Cell n2;
Bool f;
NEXT_P0;
vm_Cell2n(sp[1],n1);
vm_Cell2n(spTOS,n2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n1=", vm_out); printarg_n(n1);
fputs(" n2=", vm_out); printarg_n(n2);
}
#endif
sp += 1;
{
#line 1124 "prim"
f = FLAG(n1<=n2);
#line 1123
#line 4903 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
LABEL2(less_or_equal)
NEXT_P1_5;
LABEL3(less_or_equal)
DO_GOTO;
}

LABEL(greater_or_equal) /* >= ( n1 n2 -- f ) S1 -- S1  */
/*  */
NAME(">=")
{
DEF_CA
MAYBE_UNUSED Cell n1;
MAYBE_UNUSED Cell n2;
Bool f;
NEXT_P0;
vm_Cell2n(sp[1],n1);
vm_Cell2n(spTOS,n2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n1=", vm_out); printarg_n(n1);
fputs(" n2=", vm_out); printarg_n(n2);
}
#endif
sp += 1;
{
#line 1124 "prim"
f = FLAG(n1>=n2);
#line 1123
#line 4942 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
LABEL2(greater_or_equal)
NEXT_P1_5;
LABEL3(greater_or_equal)
DO_GOTO;
}

LABEL(u_equals) /* u= ( u1 u2 -- f ) S1 -- S1  */
/*  */
NAME("u=")
{
DEF_CA
MAYBE_UNUSED UCell u1;
MAYBE_UNUSED UCell u2;
Bool f;
NEXT_P0;
vm_Cell2u(sp[1],u1);
vm_Cell2u(spTOS,u2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" u1=", vm_out); printarg_u(u1);
fputs(" u2=", vm_out); printarg_u(u2);
}
#endif
sp += 1;
{
#line 1125 "prim"
f = FLAG(u1==u2);
#line 1124
#line 4981 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
LABEL2(u_equals)
NEXT_P1_5;
LABEL3(u_equals)
DO_GOTO;
}

LABEL(u_not_equals) /* u<> ( u1 u2 -- f ) S1 -- S1  */
/*  */
NAME("u<>")
{
DEF_CA
MAYBE_UNUSED UCell u1;
MAYBE_UNUSED UCell u2;
Bool f;
NEXT_P0;
vm_Cell2u(sp[1],u1);
vm_Cell2u(spTOS,u2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" u1=", vm_out); printarg_u(u1);
fputs(" u2=", vm_out); printarg_u(u2);
}
#endif
sp += 1;
{
#line 1125 "prim"
f = FLAG(u1!=u2);
#line 1124
#line 5020 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
LABEL2(u_not_equals)
NEXT_P1_5;
LABEL3(u_not_equals)
DO_GOTO;
}

LABEL(u_less_than) /* u< ( u1 u2 -- f ) S1 -- S1  */
/*  */
NAME("u<")
{
DEF_CA
MAYBE_UNUSED UCell u1;
MAYBE_UNUSED UCell u2;
Bool f;
NEXT_P0;
vm_Cell2u(sp[1],u1);
vm_Cell2u(spTOS,u2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" u1=", vm_out); printarg_u(u1);
fputs(" u2=", vm_out); printarg_u(u2);
}
#endif
sp += 1;
{
#line 1125 "prim"
f = FLAG(u1<u2);
#line 1124
#line 5059 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
LABEL2(u_less_than)
NEXT_P1_5;
LABEL3(u_less_than)
DO_GOTO;
}

LABEL(u_greater_than) /* u> ( u1 u2 -- f ) S1 -- S1  */
/*  */
NAME("u>")
{
DEF_CA
MAYBE_UNUSED UCell u1;
MAYBE_UNUSED UCell u2;
Bool f;
NEXT_P0;
vm_Cell2u(sp[1],u1);
vm_Cell2u(spTOS,u2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" u1=", vm_out); printarg_u(u1);
fputs(" u2=", vm_out); printarg_u(u2);
}
#endif
sp += 1;
{
#line 1125 "prim"
f = FLAG(u1>u2);
#line 1124
#line 5098 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
LABEL2(u_greater_than)
NEXT_P1_5;
LABEL3(u_greater_than)
DO_GOTO;
}

LABEL(u_less_or_equal) /* u<= ( u1 u2 -- f ) S1 -- S1  */
/*  */
NAME("u<=")
{
DEF_CA
MAYBE_UNUSED UCell u1;
MAYBE_UNUSED UCell u2;
Bool f;
NEXT_P0;
vm_Cell2u(sp[1],u1);
vm_Cell2u(spTOS,u2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" u1=", vm_out); printarg_u(u1);
fputs(" u2=", vm_out); printarg_u(u2);
}
#endif
sp += 1;
{
#line 1125 "prim"
f = FLAG(u1<=u2);
#line 1124
#line 5137 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
LABEL2(u_less_or_equal)
NEXT_P1_5;
LABEL3(u_less_or_equal)
DO_GOTO;
}

LABEL(u_greater_or_equal) /* u>= ( u1 u2 -- f ) S1 -- S1  */
/*  */
NAME("u>=")
{
DEF_CA
MAYBE_UNUSED UCell u1;
MAYBE_UNUSED UCell u2;
Bool f;
NEXT_P0;
vm_Cell2u(sp[1],u1);
vm_Cell2u(spTOS,u2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" u1=", vm_out); printarg_u(u1);
fputs(" u2=", vm_out); printarg_u(u2);
}
#endif
sp += 1;
{
#line 1125 "prim"
f = FLAG(u1>=u2);
#line 1124
#line 5176 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
LABEL2(u_greater_or_equal)
NEXT_P1_5;
LABEL3(u_greater_or_equal)
DO_GOTO;
}

GROUPADD(18)
#ifdef HAS_DCOMPS
LABEL(d_equals) /* d= ( d1 d2 -- f ) S1 -- S1  */
/*  */
NAME("d=")
{
DEF_CA
MAYBE_UNUSED DCell d1;
MAYBE_UNUSED DCell d2;
Bool f;
NEXT_P0;
vm_twoCell2d(sp[3], sp[2], d1)
vm_twoCell2d(sp[1], spTOS, d2)
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" d1=", vm_out); printarg_d(d1);
fputs(" d2=", vm_out); printarg_d(d2);
}
#endif
sp += 3;
{
#line 1175 "prim"
#ifdef BUGGY_LL_CMP
#line 1174
f = FLAG(d1.lo==d2.lo && d1.hi==d2.hi);
#line 1174
#else
#line 1174
f = FLAG(d1==d2);
#line 1174
#endif
#line 1174
#line 5225 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
LABEL2(d_equals)
NEXT_P1_5;
LABEL3(d_equals)
DO_GOTO;
}

LABEL(d_not_equals) /* d<> ( d1 d2 -- f ) S1 -- S1  */
/*  */
NAME("d<>")
{
DEF_CA
MAYBE_UNUSED DCell d1;
MAYBE_UNUSED DCell d2;
Bool f;
NEXT_P0;
vm_twoCell2d(sp[3], sp[2], d1)
vm_twoCell2d(sp[1], spTOS, d2)
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" d1=", vm_out); printarg_d(d1);
fputs(" d2=", vm_out); printarg_d(d2);
}
#endif
sp += 3;
{
#line 1175 "prim"
#ifdef BUGGY_LL_CMP
#line 1174
f = FLAG(d1.lo!=d2.lo || d1.hi!=d2.hi);
#line 1174
#else
#line 1174
f = FLAG(d1!=d2);
#line 1174
#endif
#line 1174
#line 5272 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
LABEL2(d_not_equals)
NEXT_P1_5;
LABEL3(d_not_equals)
DO_GOTO;
}

LABEL(d_less_than) /* d< ( d1 d2 -- f ) S1 -- S1  */
/*  */
NAME("d<")
{
DEF_CA
MAYBE_UNUSED DCell d1;
MAYBE_UNUSED DCell d2;
Bool f;
NEXT_P0;
vm_twoCell2d(sp[3], sp[2], d1)
vm_twoCell2d(sp[1], spTOS, d2)
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" d1=", vm_out); printarg_d(d1);
fputs(" d2=", vm_out); printarg_d(d2);
}
#endif
sp += 3;
{
#line 1175 "prim"
#ifdef BUGGY_LL_CMP
#line 1174
f = FLAG(d1.hi==d2.hi ? d1.lo<d2.lo : d1.hi<d2.hi);
#line 1174
#else
#line 1174
f = FLAG(d1<d2);
#line 1174
#endif
#line 1174
#line 5319 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
LABEL2(d_less_than)
NEXT_P1_5;
LABEL3(d_less_than)
DO_GOTO;
}

LABEL(d_greater_than) /* d> ( d1 d2 -- f ) S1 -- S1  */
/*  */
NAME("d>")
{
DEF_CA
MAYBE_UNUSED DCell d1;
MAYBE_UNUSED DCell d2;
Bool f;
NEXT_P0;
vm_twoCell2d(sp[3], sp[2], d1)
vm_twoCell2d(sp[1], spTOS, d2)
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" d1=", vm_out); printarg_d(d1);
fputs(" d2=", vm_out); printarg_d(d2);
}
#endif
sp += 3;
{
#line 1175 "prim"
#ifdef BUGGY_LL_CMP
#line 1174
f = FLAG(d1.hi==d2.hi ? d1.lo>d2.lo : d1.hi>d2.hi);
#line 1174
#else
#line 1174
f = FLAG(d1>d2);
#line 1174
#endif
#line 1174
#line 5366 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
LABEL2(d_greater_than)
NEXT_P1_5;
LABEL3(d_greater_than)
DO_GOTO;
}

LABEL(d_less_or_equal) /* d<= ( d1 d2 -- f ) S1 -- S1  */
/*  */
NAME("d<=")
{
DEF_CA
MAYBE_UNUSED DCell d1;
MAYBE_UNUSED DCell d2;
Bool f;
NEXT_P0;
vm_twoCell2d(sp[3], sp[2], d1)
vm_twoCell2d(sp[1], spTOS, d2)
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" d1=", vm_out); printarg_d(d1);
fputs(" d2=", vm_out); printarg_d(d2);
}
#endif
sp += 3;
{
#line 1175 "prim"
#ifdef BUGGY_LL_CMP
#line 1174
f = FLAG(d1.hi==d2.hi ? d1.lo<=d2.lo : d1.hi<=d2.hi);
#line 1174
#else
#line 1174
f = FLAG(d1<=d2);
#line 1174
#endif
#line 1174
#line 5413 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
LABEL2(d_less_or_equal)
NEXT_P1_5;
LABEL3(d_less_or_equal)
DO_GOTO;
}

LABEL(d_greater_or_equal) /* d>= ( d1 d2 -- f ) S1 -- S1  */
/*  */
NAME("d>=")
{
DEF_CA
MAYBE_UNUSED DCell d1;
MAYBE_UNUSED DCell d2;
Bool f;
NEXT_P0;
vm_twoCell2d(sp[3], sp[2], d1)
vm_twoCell2d(sp[1], spTOS, d2)
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" d1=", vm_out); printarg_d(d1);
fputs(" d2=", vm_out); printarg_d(d2);
}
#endif
sp += 3;
{
#line 1175 "prim"
#ifdef BUGGY_LL_CMP
#line 1174
f = FLAG(d1.hi==d2.hi ? d1.lo>=d2.lo : d1.hi>=d2.hi);
#line 1174
#else
#line 1174
f = FLAG(d1>=d2);
#line 1174
#endif
#line 1174
#line 5460 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
LABEL2(d_greater_or_equal)
NEXT_P1_5;
LABEL3(d_greater_or_equal)
DO_GOTO;
}

LABEL(d_zero_equals) /* d0= ( d -- f ) S1 -- S1  */
/*  */
NAME("d0=")
{
DEF_CA
MAYBE_UNUSED DCell d;
Bool f;
NEXT_P0;
vm_twoCell2d(sp[1], spTOS, d)
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" d=", vm_out); printarg_d(d);
}
#endif
sp += 1;
{
#line 1176 "prim"
#ifdef BUGGY_LL_CMP
#line 1175
f = FLAG(d.lo==DZERO.lo && d.hi==DZERO.hi);
#line 1175
#else
#line 1175
f = FLAG(d==DZERO);
#line 1175
#endif
#line 1175
#line 5504 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
LABEL2(d_zero_equals)
NEXT_P1_5;
LABEL3(d_zero_equals)
DO_GOTO;
}

LABEL(d_zero_not_equals) /* d0<> ( d -- f ) S1 -- S1  */
/*  */
NAME("d0<>")
{
DEF_CA
MAYBE_UNUSED DCell d;
Bool f;
NEXT_P0;
vm_twoCell2d(sp[1], spTOS, d)
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" d=", vm_out); printarg_d(d);
}
#endif
sp += 1;
{
#line 1176 "prim"
#ifdef BUGGY_LL_CMP
#line 1175
f = FLAG(d.lo!=DZERO.lo || d.hi!=DZERO.hi);
#line 1175
#else
#line 1175
f = FLAG(d!=DZERO);
#line 1175
#endif
#line 1175
#line 5548 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
LABEL2(d_zero_not_equals)
NEXT_P1_5;
LABEL3(d_zero_not_equals)
DO_GOTO;
}

LABEL(d_zero_less_than) /* d0< ( d -- f ) S1 -- S1  */
/*  */
NAME("d0<")
{
DEF_CA
MAYBE_UNUSED DCell d;
Bool f;
NEXT_P0;
vm_twoCell2d(sp[1], spTOS, d)
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" d=", vm_out); printarg_d(d);
}
#endif
sp += 1;
{
#line 1176 "prim"
#ifdef BUGGY_LL_CMP
#line 1175
f = FLAG(d.hi==DZERO.hi ? d.lo<DZERO.lo : d.hi<DZERO.hi);
#line 1175
#else
#line 1175
f = FLAG(d<DZERO);
#line 1175
#endif
#line 1175
#line 5592 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
LABEL2(d_zero_less_than)
NEXT_P1_5;
LABEL3(d_zero_less_than)
DO_GOTO;
}

LABEL(d_zero_greater_than) /* d0> ( d -- f ) S1 -- S1  */
/*  */
NAME("d0>")
{
DEF_CA
MAYBE_UNUSED DCell d;
Bool f;
NEXT_P0;
vm_twoCell2d(sp[1], spTOS, d)
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" d=", vm_out); printarg_d(d);
}
#endif
sp += 1;
{
#line 1176 "prim"
#ifdef BUGGY_LL_CMP
#line 1175
f = FLAG(d.hi==DZERO.hi ? d.lo>DZERO.lo : d.hi>DZERO.hi);
#line 1175
#else
#line 1175
f = FLAG(d>DZERO);
#line 1175
#endif
#line 1175
#line 5636 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
LABEL2(d_zero_greater_than)
NEXT_P1_5;
LABEL3(d_zero_greater_than)
DO_GOTO;
}

LABEL(d_zero_less_or_equal) /* d0<= ( d -- f ) S1 -- S1  */
/*  */
NAME("d0<=")
{
DEF_CA
MAYBE_UNUSED DCell d;
Bool f;
NEXT_P0;
vm_twoCell2d(sp[1], spTOS, d)
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" d=", vm_out); printarg_d(d);
}
#endif
sp += 1;
{
#line 1176 "prim"
#ifdef BUGGY_LL_CMP
#line 1175
f = FLAG(d.hi==DZERO.hi ? d.lo<=DZERO.lo : d.hi<=DZERO.hi);
#line 1175
#else
#line 1175
f = FLAG(d<=DZERO);
#line 1175
#endif
#line 1175
#line 5680 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
LABEL2(d_zero_less_or_equal)
NEXT_P1_5;
LABEL3(d_zero_less_or_equal)
DO_GOTO;
}

LABEL(d_zero_greater_or_equal) /* d0>= ( d -- f ) S1 -- S1  */
/*  */
NAME("d0>=")
{
DEF_CA
MAYBE_UNUSED DCell d;
Bool f;
NEXT_P0;
vm_twoCell2d(sp[1], spTOS, d)
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" d=", vm_out); printarg_d(d);
}
#endif
sp += 1;
{
#line 1176 "prim"
#ifdef BUGGY_LL_CMP
#line 1175
f = FLAG(d.hi==DZERO.hi ? d.lo>=DZERO.lo : d.hi>=DZERO.hi);
#line 1175
#else
#line 1175
f = FLAG(d>=DZERO);
#line 1175
#endif
#line 1175
#line 5724 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
LABEL2(d_zero_greater_or_equal)
NEXT_P1_5;
LABEL3(d_zero_greater_or_equal)
DO_GOTO;
}

LABEL(d_u_equals) /* du= ( ud1 ud2 -- f ) S1 -- S1  */
/*  */
NAME("du=")
{
DEF_CA
MAYBE_UNUSED UDCell ud1;
MAYBE_UNUSED UDCell ud2;
Bool f;
NEXT_P0;
vm_twoCell2ud(sp[3], sp[2], ud1)
vm_twoCell2ud(sp[1], spTOS, ud2)
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" ud1=", vm_out); printarg_ud(ud1);
fputs(" ud2=", vm_out); printarg_ud(ud2);
}
#endif
sp += 3;
{
#line 1177 "prim"
#ifdef BUGGY_LL_CMP
#line 1176
f = FLAG(ud1.lo==ud2.lo && ud1.hi==ud2.hi);
#line 1176
#else
#line 1176
f = FLAG(ud1==ud2);
#line 1176
#endif
#line 1176
#line 5771 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
LABEL2(d_u_equals)
NEXT_P1_5;
LABEL3(d_u_equals)
DO_GOTO;
}

LABEL(d_u_not_equals) /* du<> ( ud1 ud2 -- f ) S1 -- S1  */
/*  */
NAME("du<>")
{
DEF_CA
MAYBE_UNUSED UDCell ud1;
MAYBE_UNUSED UDCell ud2;
Bool f;
NEXT_P0;
vm_twoCell2ud(sp[3], sp[2], ud1)
vm_twoCell2ud(sp[1], spTOS, ud2)
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" ud1=", vm_out); printarg_ud(ud1);
fputs(" ud2=", vm_out); printarg_ud(ud2);
}
#endif
sp += 3;
{
#line 1177 "prim"
#ifdef BUGGY_LL_CMP
#line 1176
f = FLAG(ud1.lo!=ud2.lo || ud1.hi!=ud2.hi);
#line 1176
#else
#line 1176
f = FLAG(ud1!=ud2);
#line 1176
#endif
#line 1176
#line 5818 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
LABEL2(d_u_not_equals)
NEXT_P1_5;
LABEL3(d_u_not_equals)
DO_GOTO;
}

LABEL(d_u_less_than) /* du< ( ud1 ud2 -- f ) S1 -- S1  */
/*  */
NAME("du<")
{
DEF_CA
MAYBE_UNUSED UDCell ud1;
MAYBE_UNUSED UDCell ud2;
Bool f;
NEXT_P0;
vm_twoCell2ud(sp[3], sp[2], ud1)
vm_twoCell2ud(sp[1], spTOS, ud2)
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" ud1=", vm_out); printarg_ud(ud1);
fputs(" ud2=", vm_out); printarg_ud(ud2);
}
#endif
sp += 3;
{
#line 1177 "prim"
#ifdef BUGGY_LL_CMP
#line 1176
f = FLAG(ud1.hi==ud2.hi ? ud1.lo<ud2.lo : ud1.hi<ud2.hi);
#line 1176
#else
#line 1176
f = FLAG(ud1<ud2);
#line 1176
#endif
#line 1176
#line 5865 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
LABEL2(d_u_less_than)
NEXT_P1_5;
LABEL3(d_u_less_than)
DO_GOTO;
}

LABEL(d_u_greater_than) /* du> ( ud1 ud2 -- f ) S1 -- S1  */
/*  */
NAME("du>")
{
DEF_CA
MAYBE_UNUSED UDCell ud1;
MAYBE_UNUSED UDCell ud2;
Bool f;
NEXT_P0;
vm_twoCell2ud(sp[3], sp[2], ud1)
vm_twoCell2ud(sp[1], spTOS, ud2)
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" ud1=", vm_out); printarg_ud(ud1);
fputs(" ud2=", vm_out); printarg_ud(ud2);
}
#endif
sp += 3;
{
#line 1177 "prim"
#ifdef BUGGY_LL_CMP
#line 1176
f = FLAG(ud1.hi==ud2.hi ? ud1.lo>ud2.lo : ud1.hi>ud2.hi);
#line 1176
#else
#line 1176
f = FLAG(ud1>ud2);
#line 1176
#endif
#line 1176
#line 5912 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
LABEL2(d_u_greater_than)
NEXT_P1_5;
LABEL3(d_u_greater_than)
DO_GOTO;
}

LABEL(d_u_less_or_equal) /* du<= ( ud1 ud2 -- f ) S1 -- S1  */
/*  */
NAME("du<=")
{
DEF_CA
MAYBE_UNUSED UDCell ud1;
MAYBE_UNUSED UDCell ud2;
Bool f;
NEXT_P0;
vm_twoCell2ud(sp[3], sp[2], ud1)
vm_twoCell2ud(sp[1], spTOS, ud2)
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" ud1=", vm_out); printarg_ud(ud1);
fputs(" ud2=", vm_out); printarg_ud(ud2);
}
#endif
sp += 3;
{
#line 1177 "prim"
#ifdef BUGGY_LL_CMP
#line 1176
f = FLAG(ud1.hi==ud2.hi ? ud1.lo<=ud2.lo : ud1.hi<=ud2.hi);
#line 1176
#else
#line 1176
f = FLAG(ud1<=ud2);
#line 1176
#endif
#line 1176
#line 5959 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
LABEL2(d_u_less_or_equal)
NEXT_P1_5;
LABEL3(d_u_less_or_equal)
DO_GOTO;
}

LABEL(d_u_greater_or_equal) /* du>= ( ud1 ud2 -- f ) S1 -- S1  */
/*  */
NAME("du>=")
{
DEF_CA
MAYBE_UNUSED UDCell ud1;
MAYBE_UNUSED UDCell ud2;
Bool f;
NEXT_P0;
vm_twoCell2ud(sp[3], sp[2], ud1)
vm_twoCell2ud(sp[1], spTOS, ud2)
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" ud1=", vm_out); printarg_ud(ud1);
fputs(" ud2=", vm_out); printarg_ud(ud2);
}
#endif
sp += 3;
{
#line 1177 "prim"
#ifdef BUGGY_LL_CMP
#line 1176
f = FLAG(ud1.hi==ud2.hi ? ud1.lo>=ud2.lo : ud1.hi>=ud2.hi);
#line 1176
#else
#line 1176
f = FLAG(ud1>=ud2);
#line 1176
#endif
#line 1176
#line 6006 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
LABEL2(d_u_greater_or_equal)
NEXT_P1_5;
LABEL3(d_u_greater_or_equal)
DO_GOTO;
}

GROUPADD(18)
#endif
LABEL(within) /* within ( u1 u2 u3 -- f ) S1 -- S1  */
/* u2=<u1<u3 or: u3=<u2 and u1 is not in [u3,u2).  This works for
unsigned and signed numbers (but not a mixture).  Another way to think
about this word is to consider the numbers as a circle (wrapping
around from @code{max-u} to 0 for unsigned, and from @code{max-n} to
min-n for signed numbers); now consider the range from u2 towards
increasing numbers up to and excluding u3 (giving an empty range if
u2=u3); if u1 is in this range, @code{within} returns true. */
NAME("within")
{
DEF_CA
MAYBE_UNUSED UCell u1;
MAYBE_UNUSED UCell u2;
MAYBE_UNUSED UCell u3;
Bool f;
NEXT_P0;
vm_Cell2u(sp[2],u1);
vm_Cell2u(sp[1],u2);
vm_Cell2u(spTOS,u3);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" u1=", vm_out); printarg_u(u1);
fputs(" u2=", vm_out); printarg_u(u2);
fputs(" u3=", vm_out); printarg_u(u3);
}
#endif
sp += 2;
{
#line 1189 "prim"
f = FLAG(u1-u2 < u3-u2);
#line 6055 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
LABEL2(within)
NEXT_P1_5;
LABEL3(within)
DO_GOTO;
}

GROUPADD(1)
GROUP( stack, 125)
LABEL(useraddr) /* useraddr ( #u -- a_addr ) S1 -- S1  */
/*  */
NAME("useraddr")
{
DEF_CA
MAYBE_UNUSED UCell u;
Cell * a_addr;
NEXT_P0;
sp[0] = spTOS;
vm_Cell2u(IMM_ARG(IPTOS,305397793 ),u);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" u=", vm_out); printarg_u(u);
}
#endif
INC_IP(1);
sp += -1;
{
#line 1196 "prim"
a_addr = (Cell *)(up+u);
#line 6094 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" a_addr=", vm_out); printarg_a_(a_addr);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_a_2Cell(a_addr,spTOS);
LABEL2(useraddr)
NEXT_P1_5;
LABEL3(useraddr)
DO_GOTO;
}

LABEL(up_store) /* up! ( a_addr -- ) S1 -- S1  */
/*  */
NAME("up!")
{
DEF_CA
MAYBE_UNUSED Cell * a_addr;
NEXT_P0;
vm_Cell2a_(spTOS,a_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_addr=", vm_out); printarg_a_(a_addr);
}
#endif
sp += 1;
{
#line 1199 "prim"
gforth_UP=up=(Address)a_addr;
#line 6128 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
spTOS = sp[0];
LABEL2(up_store)
NEXT_P1_5;
LABEL3(up_store)
DO_GOTO;
}

LABEL(sp_fetch) /* sp@ ( S:... -- a_addr ) S1 -- S1  */
/*  */
NAME("sp@")
{
DEF_CA
Cell * a_addr;
NEXT_P0;
sp[0] = spTOS;
#ifdef VM_DEBUG
if (vm_debug) {
}
#endif
{
#line 1205 "prim"
a_addr = sp;
#line 6159 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" a_addr=", vm_out); printarg_a_(a_addr);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
sp += -1;
vm_a_2Cell(a_addr,spTOS);
LABEL2(sp_fetch)
NEXT_P1_5;
LABEL3(sp_fetch)
DO_GOTO;
}

LABEL(sp_store) /* sp! ( a_addr -- S:... ) S1 -- S1  */
/*  */
NAME("sp!")
{
DEF_CA
MAYBE_UNUSED Cell * a_addr;
NEXT_P0;
vm_Cell2a_(spTOS,a_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_addr=", vm_out); printarg_a_(a_addr);
}
#endif
sp += 1;
{
#line 1208 "prim"
sp = a_addr;
#line 6194 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
spTOS = sp[0];
LABEL2(sp_store)
NEXT_P1_5;
LABEL3(sp_store)
DO_GOTO;
}

LABEL(rp_fetch) /* rp@ ( -- a_addr ) S1 -- S1  */
/*  */
NAME("rp@")
{
DEF_CA
Cell * a_addr;
NEXT_P0;
sp[0] = spTOS;
#ifdef VM_DEBUG
if (vm_debug) {
}
#endif
sp += -1;
{
#line 1211 "prim"
a_addr = rp;
#line 6226 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" a_addr=", vm_out); printarg_a_(a_addr);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_a_2Cell(a_addr,spTOS);
LABEL2(rp_fetch)
NEXT_P1_5;
LABEL3(rp_fetch)
DO_GOTO;
}

LABEL(rp_store) /* rp! ( a_addr -- ) S1 -- S1  */
/*  */
NAME("rp!")
{
DEF_CA
MAYBE_UNUSED Cell * a_addr;
NEXT_P0;
vm_Cell2a_(spTOS,a_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_addr=", vm_out); printarg_a_(a_addr);
}
#endif
sp += 1;
{
#line 1214 "prim"
rp = a_addr;
#line 6260 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
spTOS = sp[0];
LABEL2(rp_store)
NEXT_P1_5;
LABEL3(rp_store)
DO_GOTO;
}

GROUPADD(6)
#ifdef HAS_FLOATING
LABEL(fp_fetch) /* fp@ ( f:... -- f_addr ) S1 -- S1  */
/*  */
NAME("fp@")
{
DEF_CA
Float * f_addr;
NEXT_P0;
sp[0] = spTOS;
fp[0] = fpTOS;
#ifdef VM_DEBUG
if (vm_debug) {
}
#endif
sp += -1;
{
#line 1219 "prim"
f_addr = fp;
#line 6295 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f_addr=", vm_out); printarg_f_(f_addr);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f_2Cell(f_addr,spTOS);
fpTOS = fp[0];
LABEL2(fp_fetch)
NEXT_P1_5;
LABEL3(fp_fetch)
DO_GOTO;
}

LABEL(fp_store) /* fp! ( f_addr -- f:... ) S1 -- S1  */
/*  */
NAME("fp!")
{
DEF_CA
MAYBE_UNUSED Float * f_addr;
NEXT_P0;
fp[0] = fpTOS;
vm_Cell2f_(spTOS,f_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" f_addr=", vm_out); printarg_f_(f_addr);
}
#endif
sp += 1;
{
#line 1222 "prim"
fp = f_addr;
#line 6331 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
spTOS = sp[0];
fpTOS = fp[0];
LABEL2(fp_store)
NEXT_P1_5;
LABEL3(fp_store)
DO_GOTO;
}

GROUPADD(2)
#endif
LABEL(to_r) /* >r ( w -- R:w ) S1 -- S1  */
/*  */
NAME(">r")
{
DEF_CA
MAYBE_UNUSED Cell w;
NEXT_P0;
vm_Cell2w(spTOS,w);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w=", vm_out); printarg_w(w);
}
#endif
sp += 1;
rp += -1;
{
#line 1227 "prim"
#line 6367 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w,rp[0]);
spTOS = sp[0];
LABEL2(to_r)
NEXT_P1_5;
LABEL3(to_r)
DO_GOTO;
}

LABEL(r_from) /* r> ( R:w -- w ) S1 -- S1  */
/*  */
NAME("r>")
{
DEF_CA
MAYBE_UNUSED Cell w;
NEXT_P0;
sp[0] = spTOS;
vm_Cell2w(rp[0],w);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w=", vm_out); printarg_w(w);
}
#endif
sp += -1;
rp += 1;
{
#line 1232 "prim"
#line 6402 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w,spTOS);
LABEL2(r_from)
NEXT_P1_5;
LABEL3(r_from)
DO_GOTO;
}

LABEL(rdrop) /* rdrop ( R:w -- ) S1 -- S1  */
/*  */
NAME("rdrop")
{
DEF_CA
MAYBE_UNUSED Cell w;
NEXT_P0;
vm_Cell2w(rp[0],w);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w=", vm_out); printarg_w(w);
}
#endif
rp += 1;
{
#line 1237 "prim"
#line 6434 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
LABEL2(rdrop)
NEXT_P1_5;
LABEL3(rdrop)
DO_GOTO;
}

LABEL(two_to_r) /* 2>r ( d -- R:d ) S1 -- S1  */
/*  */
NAME("2>r")
{
DEF_CA
MAYBE_UNUSED DCell d;
NEXT_P0;
vm_twoCell2d(sp[1], spTOS, d)
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" d=", vm_out); printarg_d(d);
}
#endif
sp += 2;
rp += -2;
{
#line 1241 "prim"
#line 6466 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_d2twoCell(d, rp[1], rp[0])
spTOS = sp[0];
LABEL2(two_to_r)
NEXT_P1_5;
LABEL3(two_to_r)
DO_GOTO;
}

LABEL(two_r_from) /* 2r> ( R:d -- d ) S1 -- S1  */
/*  */
NAME("2r>")
{
DEF_CA
MAYBE_UNUSED DCell d;
NEXT_P0;
sp[0] = spTOS;
vm_twoCell2d(rp[1], rp[0], d)
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" d=", vm_out); printarg_d(d);
}
#endif
sp += -2;
rp += 2;
{
#line 1245 "prim"
#line 6501 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_d2twoCell(d, sp[1], spTOS)
LABEL2(two_r_from)
NEXT_P1_5;
LABEL3(two_r_from)
DO_GOTO;
}

LABEL(two_r_fetch) /* 2r@ ( R:d -- R:d d ) S1 -- S1  */
/*  */
NAME("2r@")
{
DEF_CA
MAYBE_UNUSED DCell d;
NEXT_P0;
sp[0] = spTOS;
vm_twoCell2d(rp[1], rp[0], d)
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" d=", vm_out); printarg_d(d);
}
#endif
sp += -2;
{
#line 1249 "prim"
#line 6534 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_d2twoCell(d, rp[1], rp[0])
vm_d2twoCell(d, sp[1], spTOS)
LABEL2(two_r_fetch)
NEXT_P1_5;
LABEL3(two_r_fetch)
DO_GOTO;
}

LABEL(two_r_drop) /* 2rdrop ( R:d -- ) S1 -- S1  */
/*  */
NAME("2rdrop")
{
DEF_CA
MAYBE_UNUSED DCell d;
NEXT_P0;
vm_twoCell2d(rp[1], rp[0], d)
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" d=", vm_out); printarg_d(d);
}
#endif
rp += 2;
{
#line 1253 "prim"
#line 6567 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
LABEL2(two_r_drop)
NEXT_P1_5;
LABEL3(two_r_drop)
DO_GOTO;
}

LABEL(over) /* over ( w1 w2 -- w1 w2 w1 ) S1 -- S1  */
/*  */
NAME("over")
{
DEF_CA
MAYBE_UNUSED Cell w1;
MAYBE_UNUSED Cell w2;
NEXT_P0;
vm_Cell2w(sp[1],w1);
vm_Cell2w(spTOS,w2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w1=", vm_out); printarg_w(w1);
fputs(" w2=", vm_out); printarg_w(w2);
}
#endif
sp += -1;
{
#line 1257 "prim"
#line 6601 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w2,sp[1]);
vm_w2Cell(w1,spTOS);
LABEL2(over)
NEXT_P1_5;
LABEL3(over)
DO_GOTO;
}

LABEL(drop) /* drop ( w -- ) S1 -- S1  */
/*  */
NAME("drop")
{
DEF_CA
MAYBE_UNUSED Cell w;
NEXT_P0;
vm_Cell2w(spTOS,w);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w=", vm_out); printarg_w(w);
}
#endif
sp += 1;
{
#line 1261 "prim"
#line 6634 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
spTOS = sp[0];
LABEL2(drop)
NEXT_P1_5;
LABEL3(drop)
DO_GOTO;
}

LABEL(swap) /* swap ( w1 w2 -- w2 w1 ) S1 -- S1  */
/*  */
NAME("swap")
{
DEF_CA
MAYBE_UNUSED Cell w1;
MAYBE_UNUSED Cell w2;
NEXT_P0;
vm_Cell2w(sp[1],w1);
vm_Cell2w(spTOS,w2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w1=", vm_out); printarg_w(w1);
fputs(" w2=", vm_out); printarg_w(w2);
}
#endif
{
#line 1265 "prim"
#line 6668 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w2,sp[1]);
vm_w2Cell(w1,spTOS);
LABEL2(swap)
NEXT_P1_5;
LABEL3(swap)
DO_GOTO;
}

LABEL(dupe) /* dup ( w -- w w ) S1 -- S1  */
/*  */
NAME("dup")
{
DEF_CA
MAYBE_UNUSED Cell w;
NEXT_P0;
vm_Cell2w(spTOS,w);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w=", vm_out); printarg_w(w);
}
#endif
sp += -1;
{
#line 1270 "prim"
#line 6701 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w,sp[1]);
vm_w2Cell(w,spTOS);
LABEL2(dupe)
NEXT_P1_5;
LABEL3(dupe)
DO_GOTO;
}

LABEL(rote) /* rot ( w1 w2 w3 -- w2 w3 w1 ) S1 -- S1  */
/*  */
NAME("rot")
{
DEF_CA
MAYBE_UNUSED Cell w1;
MAYBE_UNUSED Cell w2;
MAYBE_UNUSED Cell w3;
NEXT_P0;
vm_Cell2w(sp[2],w1);
vm_Cell2w(sp[1],w2);
vm_Cell2w(spTOS,w3);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w1=", vm_out); printarg_w(w1);
fputs(" w2=", vm_out); printarg_w(w2);
fputs(" w3=", vm_out); printarg_w(w3);
}
#endif
{
#line 1274 "prim"
#line 6739 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w2,sp[2]);
vm_w2Cell(w3,sp[1]);
vm_w2Cell(w1,spTOS);
LABEL2(rote)
NEXT_P1_5;
LABEL3(rote)
DO_GOTO;
}

LABEL(not_rote) /* -rot ( w1 w2 w3 -- w3 w1 w2 ) S1 -- S1  */
/*  */
NAME("-rot")
{
DEF_CA
MAYBE_UNUSED Cell w1;
MAYBE_UNUSED Cell w2;
MAYBE_UNUSED Cell w3;
NEXT_P0;
vm_Cell2w(sp[2],w1);
vm_Cell2w(sp[1],w2);
vm_Cell2w(spTOS,w3);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w1=", vm_out); printarg_w(w1);
fputs(" w2=", vm_out); printarg_w(w2);
fputs(" w3=", vm_out); printarg_w(w3);
}
#endif
{
#line 1283 "prim"
#line 6778 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w3,sp[2]);
vm_w2Cell(w1,sp[1]);
vm_w2Cell(w2,spTOS);
LABEL2(not_rote)
NEXT_P1_5;
LABEL3(not_rote)
DO_GOTO;
}

LABEL(nip) /* nip ( w1 w2 -- w2 ) S1 -- S1  */
/*  */
NAME("nip")
{
DEF_CA
MAYBE_UNUSED Cell w1;
MAYBE_UNUSED Cell w2;
NEXT_P0;
vm_Cell2w(sp[1],w1);
vm_Cell2w(spTOS,w2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w1=", vm_out); printarg_w(w1);
fputs(" w2=", vm_out); printarg_w(w2);
}
#endif
sp += 1;
{
#line 1287 "prim"
#line 6815 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w2,spTOS);
LABEL2(nip)
NEXT_P1_5;
LABEL3(nip)
DO_GOTO;
}

LABEL(tuck) /* tuck ( w1 w2 -- w2 w1 w2 ) S1 -- S1  */
/*  */
NAME("tuck")
{
DEF_CA
MAYBE_UNUSED Cell w1;
MAYBE_UNUSED Cell w2;
NEXT_P0;
vm_Cell2w(sp[1],w1);
vm_Cell2w(spTOS,w2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w1=", vm_out); printarg_w(w1);
fputs(" w2=", vm_out); printarg_w(w2);
}
#endif
sp += -1;
{
#line 1291 "prim"
#line 6850 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w2,sp[2]);
vm_w2Cell(w1,sp[1]);
vm_w2Cell(w2,spTOS);
LABEL2(tuck)
NEXT_P1_5;
LABEL3(tuck)
DO_GOTO;
}

LABEL(question_dupe) /* ?dup ( w -- S:... w ) S1 -- S1  */
/* Actually the stack effect is: @code{( w -- 0 | w w )}.  It performs a
@code{dup} if w is nonzero. */
NAME("?dup")
{
DEF_CA
MAYBE_UNUSED Cell w;
NEXT_P0;
vm_Cell2w(spTOS,w);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w=", vm_out); printarg_w(w);
}
#endif
sp += 1;
{
#line 1297 "prim"
if (w!=0) {
  *--sp = w;
}
#line 6888 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
sp += -1;
vm_w2Cell(w,spTOS);
LABEL2(question_dupe)
NEXT_P1_5;
LABEL3(question_dupe)
DO_GOTO;
}

LABEL(pick) /* pick ( S:... u -- S:... w ) S1 -- S1  */
/* Actually the stack effect is @code{ x0 ... xu u -- x0 ... xu x0 }. */
NAME("pick")
{
DEF_CA
MAYBE_UNUSED UCell u;
Cell w;
NEXT_P0;
vm_Cell2u(spTOS,u);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" u=", vm_out); printarg_u(u);
}
#endif
sp += 1;
{
#line 1305 "prim"
w = sp[u];
#line 6923 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" w=", vm_out); printarg_w(w);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
sp += -1;
vm_w2Cell(w,spTOS);
LABEL2(pick)
NEXT_P1_5;
LABEL3(pick)
DO_GOTO;
}

LABEL(two_drop) /* 2drop ( w1 w2 -- ) S1 -- S1  */
/*  */
NAME("2drop")
{
DEF_CA
MAYBE_UNUSED Cell w1;
MAYBE_UNUSED Cell w2;
NEXT_P0;
vm_Cell2w(sp[1],w1);
vm_Cell2w(spTOS,w2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w1=", vm_out); printarg_w(w1);
fputs(" w2=", vm_out); printarg_w(w2);
}
#endif
sp += 2;
{
#line 1310 "prim"
#line 6960 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
spTOS = sp[0];
LABEL2(two_drop)
NEXT_P1_5;
LABEL3(two_drop)
DO_GOTO;
}

LABEL(two_dupe) /* 2dup ( w1 w2 -- w1 w2 w1 w2 ) S1 -- S1  */
/*  */
NAME("2dup")
{
DEF_CA
MAYBE_UNUSED Cell w1;
MAYBE_UNUSED Cell w2;
NEXT_P0;
vm_Cell2w(sp[1],w1);
vm_Cell2w(spTOS,w2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w1=", vm_out); printarg_w(w1);
fputs(" w2=", vm_out); printarg_w(w2);
}
#endif
sp += -2;
{
#line 1314 "prim"
#line 6995 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w2,sp[2]);
vm_w2Cell(w1,sp[1]);
vm_w2Cell(w2,spTOS);
LABEL2(two_dupe)
NEXT_P1_5;
LABEL3(two_dupe)
DO_GOTO;
}

LABEL(two_over) /* 2over ( w1 w2 w3 w4 -- w1 w2 w3 w4 w1 w2 ) S1 -- S1  */
/*  */
NAME("2over")
{
DEF_CA
MAYBE_UNUSED Cell w1;
MAYBE_UNUSED Cell w2;
MAYBE_UNUSED Cell w3;
MAYBE_UNUSED Cell w4;
NEXT_P0;
vm_Cell2w(sp[3],w1);
vm_Cell2w(sp[2],w2);
vm_Cell2w(sp[1],w3);
vm_Cell2w(spTOS,w4);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w1=", vm_out); printarg_w(w1);
fputs(" w2=", vm_out); printarg_w(w2);
fputs(" w3=", vm_out); printarg_w(w3);
fputs(" w4=", vm_out); printarg_w(w4);
}
#endif
sp += -2;
{
#line 1318 "prim"
#line 7038 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w4,sp[2]);
vm_w2Cell(w1,sp[1]);
vm_w2Cell(w2,spTOS);
LABEL2(two_over)
NEXT_P1_5;
LABEL3(two_over)
DO_GOTO;
}

LABEL(two_swap) /* 2swap ( w1 w2 w3 w4 -- w3 w4 w1 w2 ) S1 -- S1  */
/*  */
NAME("2swap")
{
DEF_CA
MAYBE_UNUSED Cell w1;
MAYBE_UNUSED Cell w2;
MAYBE_UNUSED Cell w3;
MAYBE_UNUSED Cell w4;
NEXT_P0;
vm_Cell2w(sp[3],w1);
vm_Cell2w(sp[2],w2);
vm_Cell2w(sp[1],w3);
vm_Cell2w(spTOS,w4);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w1=", vm_out); printarg_w(w1);
fputs(" w2=", vm_out); printarg_w(w2);
fputs(" w3=", vm_out); printarg_w(w3);
fputs(" w4=", vm_out); printarg_w(w4);
}
#endif
{
#line 1322 "prim"
#line 7080 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w3,sp[3]);
vm_w2Cell(w4,sp[2]);
vm_w2Cell(w1,sp[1]);
vm_w2Cell(w2,spTOS);
LABEL2(two_swap)
NEXT_P1_5;
LABEL3(two_swap)
DO_GOTO;
}

LABEL(two_rote) /* 2rot ( w1 w2 w3 w4 w5 w6 -- w3 w4 w5 w6 w1 w2 ) S1 -- S1  */
/*  */
NAME("2rot")
{
DEF_CA
MAYBE_UNUSED Cell w1;
MAYBE_UNUSED Cell w2;
MAYBE_UNUSED Cell w3;
MAYBE_UNUSED Cell w4;
MAYBE_UNUSED Cell w5;
MAYBE_UNUSED Cell w6;
NEXT_P0;
vm_Cell2w(sp[5],w1);
vm_Cell2w(sp[4],w2);
vm_Cell2w(sp[3],w3);
vm_Cell2w(sp[2],w4);
vm_Cell2w(sp[1],w5);
vm_Cell2w(spTOS,w6);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w1=", vm_out); printarg_w(w1);
fputs(" w2=", vm_out); printarg_w(w2);
fputs(" w3=", vm_out); printarg_w(w3);
fputs(" w4=", vm_out); printarg_w(w4);
fputs(" w5=", vm_out); printarg_w(w5);
fputs(" w6=", vm_out); printarg_w(w6);
}
#endif
{
#line 1326 "prim"
#line 7129 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w3,sp[5]);
vm_w2Cell(w4,sp[4]);
vm_w2Cell(w5,sp[3]);
vm_w2Cell(w6,sp[2]);
vm_w2Cell(w1,sp[1]);
vm_w2Cell(w2,spTOS);
LABEL2(two_rote)
NEXT_P1_5;
LABEL3(two_rote)
DO_GOTO;
}

LABEL(two_nip) /* 2nip ( w1 w2 w3 w4 -- w3 w4 ) S1 -- S1  */
/*  */
NAME("2nip")
{
DEF_CA
MAYBE_UNUSED Cell w1;
MAYBE_UNUSED Cell w2;
MAYBE_UNUSED Cell w3;
MAYBE_UNUSED Cell w4;
NEXT_P0;
vm_Cell2w(sp[3],w1);
vm_Cell2w(sp[2],w2);
vm_Cell2w(sp[1],w3);
vm_Cell2w(spTOS,w4);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w1=", vm_out); printarg_w(w1);
fputs(" w2=", vm_out); printarg_w(w2);
fputs(" w3=", vm_out); printarg_w(w3);
fputs(" w4=", vm_out); printarg_w(w4);
}
#endif
sp += 2;
{
#line 1330 "prim"
#line 7175 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w3,sp[1]);
vm_w2Cell(w4,spTOS);
LABEL2(two_nip)
NEXT_P1_5;
LABEL3(two_nip)
DO_GOTO;
}

LABEL(two_tuck) /* 2tuck ( w1 w2 w3 w4 -- w3 w4 w1 w2 w3 w4 ) S1 -- S1  */
/*  */
NAME("2tuck")
{
DEF_CA
MAYBE_UNUSED Cell w1;
MAYBE_UNUSED Cell w2;
MAYBE_UNUSED Cell w3;
MAYBE_UNUSED Cell w4;
NEXT_P0;
vm_Cell2w(sp[3],w1);
vm_Cell2w(sp[2],w2);
vm_Cell2w(sp[1],w3);
vm_Cell2w(spTOS,w4);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w1=", vm_out); printarg_w(w1);
fputs(" w2=", vm_out); printarg_w(w2);
fputs(" w3=", vm_out); printarg_w(w3);
fputs(" w4=", vm_out); printarg_w(w4);
}
#endif
sp += -2;
{
#line 1334 "prim"
#line 7217 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w3,sp[5]);
vm_w2Cell(w4,sp[4]);
vm_w2Cell(w1,sp[3]);
vm_w2Cell(w2,sp[2]);
vm_w2Cell(w3,sp[1]);
vm_w2Cell(w4,spTOS);
LABEL2(two_tuck)
NEXT_P1_5;
LABEL3(two_tuck)
DO_GOTO;
}

GROUPADD(24)
GROUP( memory, 157)
LABEL(fetch) /* @ ( a_addr -- w ) S1 -- S1  */
/* @i{w} is the cell stored at @i{a_addr}. */
NAME("@")
{
DEF_CA
MAYBE_UNUSED Cell * a_addr;
Cell w;
NEXT_P0;
vm_Cell2a_(spTOS,a_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_addr=", vm_out); printarg_a_(a_addr);
}
#endif
{
#line 1343 "prim"
w = *a_addr;
#line 7257 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" w=", vm_out); printarg_w(w);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w,spTOS);
LABEL2(fetch)
NEXT_P1_5;
LABEL3(fetch)
DO_GOTO;
}

LABEL(lit_fetch) /* lit@ ( #a_addr -- w ) S1 -- S1  */
/*  */
NAME("lit@")
{
DEF_CA
MAYBE_UNUSED Cell * a_addr;
Cell w;
NEXT_P0;
sp[0] = spTOS;
vm_Cell2a_(IMM_ARG(IPTOS,305397794 ),a_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_addr=", vm_out); printarg_a_(a_addr);
}
#endif
INC_IP(1);
sp += -1;
{
#line 1348 "prim"
w = *a_addr;
#line 7294 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" w=", vm_out); printarg_w(w);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w,spTOS);
LABEL2(lit_fetch)
NEXT_P1_5;
LABEL3(lit_fetch)
DO_GOTO;
}

LABEL(store) /* ! ( w a_addr -- ) S1 -- S1  */
/* Store @i{w} into the cell at @i{a-addr}. */
NAME("!")
{
DEF_CA
MAYBE_UNUSED Cell w;
MAYBE_UNUSED Cell * a_addr;
NEXT_P0;
vm_Cell2w(sp[1],w);
vm_Cell2a_(spTOS,a_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w=", vm_out); printarg_w(w);
fputs(" a_addr=", vm_out); printarg_a_(a_addr);
}
#endif
sp += 2;
{
#line 1352 "prim"
*a_addr = w;
#line 7331 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
spTOS = sp[0];
LABEL2(store)
NEXT_P1_5;
LABEL3(store)
DO_GOTO;
}

LABEL(plus_store) /* +! ( n a_addr -- ) S1 -- S1  */
/* Add @i{n} to the cell at @i{a-addr}. */
NAME("+!")
{
DEF_CA
MAYBE_UNUSED Cell n;
MAYBE_UNUSED Cell * a_addr;
NEXT_P0;
vm_Cell2n(sp[1],n);
vm_Cell2a_(spTOS,a_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n=", vm_out); printarg_n(n);
fputs(" a_addr=", vm_out); printarg_a_(a_addr);
}
#endif
sp += 2;
{
#line 1356 "prim"
*a_addr += n;
#line 7367 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
spTOS = sp[0];
LABEL2(plus_store)
NEXT_P1_5;
LABEL3(plus_store)
DO_GOTO;
}

LABEL(c_fetch) /* c@ ( c_addr -- c ) S1 -- S1  */
/* @i{c} is the char stored at @i{c_addr}. */
NAME("c@")
{
DEF_CA
MAYBE_UNUSED Char * c_addr;
Char c;
NEXT_P0;
vm_Cell2c_(spTOS,c_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_addr=", vm_out); printarg_c_(c_addr);
}
#endif
{
#line 1362 "prim"
c = *c_addr;
#line 7400 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" c=", vm_out); printarg_c(c);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_c2Cell(c,spTOS);
LABEL2(c_fetch)
NEXT_P1_5;
LABEL3(c_fetch)
DO_GOTO;
}

LABEL(c_store) /* c! ( c c_addr -- ) S1 -- S1  */
/* Store @i{c} into the char at @i{c-addr}. */
NAME("c!")
{
DEF_CA
MAYBE_UNUSED Char c;
MAYBE_UNUSED Char * c_addr;
NEXT_P0;
vm_Cell2c(sp[1],c);
vm_Cell2c_(spTOS,c_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c=", vm_out); printarg_c(c);
fputs(" c_addr=", vm_out); printarg_c_(c_addr);
}
#endif
sp += 2;
{
#line 1388 "prim"
*c_addr = c;
#line 7437 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
spTOS = sp[0];
LABEL2(c_store)
NEXT_P1_5;
LABEL3(c_store)
DO_GOTO;
}

LABEL(two_store) /* 2! ( w1 w2 a_addr -- ) S1 -- S1  */
/* Store @i{w2} into the cell at @i{c-addr} and @i{w1} into the next cell. */
NAME("2!")
{
DEF_CA
MAYBE_UNUSED Cell w1;
MAYBE_UNUSED Cell w2;
MAYBE_UNUSED Cell * a_addr;
NEXT_P0;
vm_Cell2w(sp[2],w1);
vm_Cell2w(sp[1],w2);
vm_Cell2a_(spTOS,a_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w1=", vm_out); printarg_w(w1);
fputs(" w2=", vm_out); printarg_w(w2);
fputs(" a_addr=", vm_out); printarg_a_(a_addr);
}
#endif
sp += 3;
{
#line 1418 "prim"
a_addr[0] = w2;
a_addr[1] = w1;
#line 7477 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
spTOS = sp[0];
LABEL2(two_store)
NEXT_P1_5;
LABEL3(two_store)
DO_GOTO;
}

LABEL(two_fetch) /* 2@ ( a_addr -- w1 w2 ) S1 -- S1  */
/* @i{w2} is the content of the cell stored at @i{a-addr}, @i{w1} is
the content of the next cell. */
NAME("2@")
{
DEF_CA
MAYBE_UNUSED Cell * a_addr;
Cell w1;
Cell w2;
NEXT_P0;
vm_Cell2a_(spTOS,a_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_addr=", vm_out); printarg_a_(a_addr);
}
#endif
sp += -1;
{
#line 1426 "prim"
w2 = a_addr[0];
w1 = a_addr[1];
#line 7514 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" w1=", vm_out); printarg_w(w1);
fputs(" w2=", vm_out); printarg_w(w2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w1,sp[1]);
vm_w2Cell(w2,spTOS);
LABEL2(two_fetch)
NEXT_P1_5;
LABEL3(two_fetch)
DO_GOTO;
}

LABEL(cell_plus) /* cell+ ( a_addr1 -- a_addr2 ) S1 -- S1  */
/* @code{1 cells +} */
NAME("cell+")
{
DEF_CA
MAYBE_UNUSED Cell * a_addr1;
Cell * a_addr2;
NEXT_P0;
vm_Cell2a_(spTOS,a_addr1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_addr1=", vm_out); printarg_a_(a_addr1);
}
#endif
{
#line 1433 "prim"
a_addr2 = a_addr1+1;
#line 7550 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" a_addr2=", vm_out); printarg_a_(a_addr2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_a_2Cell(a_addr2,spTOS);
LABEL2(cell_plus)
NEXT_P1_5;
LABEL3(cell_plus)
DO_GOTO;
}

LABEL(cells) /* cells ( n1 -- n2 ) S1 -- S1  */
/*  @i{n2} is the number of address units of @i{n1} cells. */
NAME("cells")
{
DEF_CA
MAYBE_UNUSED Cell n1;
Cell n2;
NEXT_P0;
vm_Cell2n(spTOS,n1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n1=", vm_out); printarg_n(n1);
}
#endif
{
#line 1439 "prim"
n2 = n1 * sizeof(Cell);
#line 7584 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n2=", vm_out); printarg_n(n2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n2,spTOS);
LABEL2(cells)
NEXT_P1_5;
LABEL3(cells)
DO_GOTO;
}

LABEL(char_plus) /* char+ ( c_addr1 -- c_addr2 ) S1 -- S1  */
/* @code{1 chars +}. */
NAME("char+")
{
DEF_CA
MAYBE_UNUSED Char * c_addr1;
Char * c_addr2;
NEXT_P0;
vm_Cell2c_(spTOS,c_addr1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_addr1=", vm_out); printarg_c_(c_addr1);
}
#endif
{
#line 1450 "prim"
c_addr2 = c_addr1 + 1;
#line 7618 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" c_addr2=", vm_out); printarg_c_(c_addr2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_c_2Cell(c_addr2,spTOS);
LABEL2(char_plus)
NEXT_P1_5;
LABEL3(char_plus)
DO_GOTO;
}

LABEL(paren_chars) /* (chars) ( n1 -- n2 ) S1 -- S1  */
/*  */
NAME("(chars)")
{
DEF_CA
MAYBE_UNUSED Cell n1;
Cell n2;
NEXT_P0;
vm_Cell2n(spTOS,n1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n1=", vm_out); printarg_n(n1);
}
#endif
{
#line 1455 "prim"
n2 = n1 * sizeof(Char);
#line 7652 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n2=", vm_out); printarg_n(n2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n2,spTOS);
LABEL2(paren_chars)
NEXT_P1_5;
LABEL3(paren_chars)
DO_GOTO;
}

LABEL(count) /* count ( c_addr1 -- c_addr2 u ) S1 -- S1  */
/* @i{c-addr2} is the first character and @i{u} the length of the
counted string at @i{c-addr1}. */
NAME("count")
{
DEF_CA
MAYBE_UNUSED Char * c_addr1;
Char * c_addr2;
UCell u;
NEXT_P0;
vm_Cell2c_(spTOS,c_addr1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_addr1=", vm_out); printarg_c_(c_addr1);
}
#endif
sp += -1;
{
#line 1462 "prim"
u = *c_addr1;
c_addr2 = c_addr1+1;
#line 7690 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" c_addr2=", vm_out); printarg_c_(c_addr2);
fputs(" u=", vm_out); printarg_u(u);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_c_2Cell(c_addr2,sp[1]);
vm_u2Cell(u,spTOS);
LABEL2(count)
NEXT_P1_5;
LABEL3(count)
DO_GOTO;
}

GROUPADD(13)
GROUP( compiler, 170)
GROUPADD(0)
#ifdef HAS_F83HEADERSTRING
LABEL(paren_f83find) /* (f83find) ( c_addr u f83name1 -- f83name2 ) S1 -- S1  */
/*  */
NAME("(f83find)")
{
DEF_CA
MAYBE_UNUSED Char * c_addr;
MAYBE_UNUSED UCell u;
MAYBE_UNUSED struct F83Name * f83name1;
struct F83Name * f83name2;
NEXT_P0;
vm_Cell2c_(sp[2],c_addr);
vm_Cell2u(sp[1],u);
vm_Cell2f83name(spTOS,f83name1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_addr=", vm_out); printarg_c_(c_addr);
fputs(" u=", vm_out); printarg_u(u);
fputs(" f83name1=", vm_out); printarg_f83name(f83name1);
}
#endif
sp += 2;
{
#line 1472 "prim"
for (; f83name1 != NULL; f83name1 = (struct F83Name *)(f83name1->next))
  if ((UCell)F83NAME_COUNT(f83name1)==u &&
      memcasecmp(c_addr, f83name1->name, u)== 0 /* or inline? */)
    break;
f83name2=f83name1;
#ifdef DEBUG
fprintf(stderr, "F83find ");
fwrite(c_addr, u, 1, stderr);
fprintf(stderr, " found %08x\n", f83name2); 
#endif
#line 7746 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f83name2=", vm_out); printarg_f83name(f83name2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f83name2Cell(f83name2,spTOS);
LABEL2(paren_f83find)
NEXT_P1_5;
LABEL3(paren_f83find)
DO_GOTO;
}

GROUPADD(1)
#else /* 171 */
LABEL(paren_listlfind) /* (listlfind) ( c_addr u longname1 -- longname2 ) S1 -- S1  */
/*  */
NAME("(listlfind)")
{
DEF_CA
MAYBE_UNUSED Char * c_addr;
MAYBE_UNUSED UCell u;
MAYBE_UNUSED struct Longname * longname1;
struct Longname * longname2;
NEXT_P0;
vm_Cell2c_(sp[2],c_addr);
vm_Cell2u(sp[1],u);
vm_Cell2longname(spTOS,longname1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_addr=", vm_out); printarg_c_(c_addr);
fputs(" u=", vm_out); printarg_u(u);
fputs(" longname1=", vm_out); printarg_longname(longname1);
}
#endif
sp += 2;
{
#line 1502 "prim"
longname2=listlfind(c_addr, u, longname1);
#line 7789 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" longname2=", vm_out); printarg_longname(longname2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_longname2Cell(longname2,spTOS);
LABEL2(paren_listlfind)
NEXT_P1_5;
LABEL3(paren_listlfind)
DO_GOTO;
}

GROUPADD(1)
#ifdef HAS_HASH
LABEL(paren_hashlfind) /* (hashlfind) ( c_addr u a_addr -- longname2 ) S1 -- S1  */
/*  */
NAME("(hashlfind)")
{
DEF_CA
MAYBE_UNUSED Char * c_addr;
MAYBE_UNUSED UCell u;
MAYBE_UNUSED Cell * a_addr;
struct Longname * longname2;
NEXT_P0;
vm_Cell2c_(sp[2],c_addr);
vm_Cell2u(sp[1],u);
vm_Cell2a_(spTOS,a_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_addr=", vm_out); printarg_c_(c_addr);
fputs(" u=", vm_out); printarg_u(u);
fputs(" a_addr=", vm_out); printarg_a_(a_addr);
}
#endif
sp += 2;
{
#line 1523 "prim"
longname2 = hashlfind(c_addr, u, a_addr);
#line 7832 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" longname2=", vm_out); printarg_longname(longname2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_longname2Cell(longname2,spTOS);
LABEL2(paren_hashlfind)
NEXT_P1_5;
LABEL3(paren_hashlfind)
DO_GOTO;
}

LABEL(paren_tablelfind) /* (tablelfind) ( c_addr u a_addr -- longname2 ) S1 -- S1  */
/* A case-sensitive variant of @code{(hashfind)} */
NAME("(tablelfind)")
{
DEF_CA
MAYBE_UNUSED Char * c_addr;
MAYBE_UNUSED UCell u;
MAYBE_UNUSED Cell * a_addr;
struct Longname * longname2;
NEXT_P0;
vm_Cell2c_(sp[2],c_addr);
vm_Cell2u(sp[1],u);
vm_Cell2a_(spTOS,a_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_addr=", vm_out); printarg_c_(c_addr);
fputs(" u=", vm_out); printarg_u(u);
fputs(" a_addr=", vm_out); printarg_a_(a_addr);
}
#endif
sp += 2;
{
#line 1534 "prim"
longname2 = tablelfind(c_addr, u, a_addr);
#line 7873 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" longname2=", vm_out); printarg_longname(longname2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_longname2Cell(longname2,spTOS);
LABEL2(paren_tablelfind)
NEXT_P1_5;
LABEL3(paren_tablelfind)
DO_GOTO;
}

LABEL(paren_hashkey1) /* (hashkey1) ( c_addr u ubits -- ukey ) S1 -- S1  */
/* ukey is the hash key for the string c_addr u fitting in ubits bits */
NAME("(hashkey1)")
{
DEF_CA
MAYBE_UNUSED Char * c_addr;
MAYBE_UNUSED UCell u;
MAYBE_UNUSED UCell ubits;
UCell ukey;
NEXT_P0;
vm_Cell2c_(sp[2],c_addr);
vm_Cell2u(sp[1],u);
vm_Cell2u(spTOS,ubits);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_addr=", vm_out); printarg_c_(c_addr);
fputs(" u=", vm_out); printarg_u(u);
fputs(" ubits=", vm_out); printarg_u(ubits);
}
#endif
sp += 2;
{
#line 1551 "prim"
ukey = hashkey1(c_addr, u, ubits);
#line 7914 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" ukey=", vm_out); printarg_u(ukey);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_u2Cell(ukey,spTOS);
LABEL2(paren_hashkey1)
NEXT_P1_5;
LABEL3(paren_hashkey1)
DO_GOTO;
}

GROUPADD(3)
#endif
GROUPADD(0)
#endif
LABEL(paren_parse_white) /* (parse-white) ( c_addr1 u1 -- c_addr2 u2 ) S1 -- S1  */
/*  */
NAME("(parse-white)")
{
DEF_CA
MAYBE_UNUSED Char * c_addr1;
MAYBE_UNUSED UCell u1;
Char * c_addr2;
UCell u2;
NEXT_P0;
vm_Cell2c_(sp[1],c_addr1);
vm_Cell2u(spTOS,u1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_addr1=", vm_out); printarg_c_(c_addr1);
fputs(" u1=", vm_out); printarg_u(u1);
}
#endif
{
#line 1570 "prim"
struct Cellpair r=parse_white(c_addr1, u1);
c_addr2 = (Char *)(r.n1);
u2 = r.n2;
#line 7958 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" c_addr2=", vm_out); printarg_c_(c_addr2);
fputs(" u2=", vm_out); printarg_u(u2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_c_2Cell(c_addr2,sp[1]);
vm_u2Cell(u2,spTOS);
LABEL2(paren_parse_white)
NEXT_P1_5;
LABEL3(paren_parse_white)
DO_GOTO;
}

LABEL(aligned) /* aligned ( c_addr -- a_addr ) S1 -- S1  */
/*  @i{a-addr} is the first aligned address greater than or equal to @i{c-addr}. */
NAME("aligned")
{
DEF_CA
MAYBE_UNUSED Char * c_addr;
Cell * a_addr;
NEXT_P0;
vm_Cell2c_(spTOS,c_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_addr=", vm_out); printarg_c_(c_addr);
}
#endif
{
#line 1581 "prim"
a_addr = (Cell *)((((Cell)c_addr)+(sizeof(Cell)-1))&(-sizeof(Cell)));
#line 7994 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" a_addr=", vm_out); printarg_a_(a_addr);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_a_2Cell(a_addr,spTOS);
LABEL2(aligned)
NEXT_P1_5;
LABEL3(aligned)
DO_GOTO;
}

LABEL(f_aligned) /* faligned ( c_addr -- f_addr ) S1 -- S1  */
/*  @i{f-addr} is the first float-aligned address greater than or equal to @i{c-addr}. */
NAME("faligned")
{
DEF_CA
MAYBE_UNUSED Char * c_addr;
Float * f_addr;
NEXT_P0;
vm_Cell2c_(spTOS,c_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_addr=", vm_out); printarg_c_(c_addr);
}
#endif
{
#line 1587 "prim"
f_addr = (Float *)((((Cell)c_addr)+(sizeof(Float)-1))&(-sizeof(Float)));
#line 8028 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f_addr=", vm_out); printarg_f_(f_addr);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f_2Cell(f_addr,spTOS);
LABEL2(f_aligned)
NEXT_P1_5;
LABEL3(f_aligned)
DO_GOTO;
}

LABEL(threading_method) /* threading-method ( -- n ) S1 -- S1  */
/* 0 if the engine is direct threaded. Note that this may change during
the lifetime of an image. */
NAME("threading-method")
{
DEF_CA
Cell n;
NEXT_P0;
sp[0] = spTOS;
#ifdef VM_DEBUG
if (vm_debug) {
}
#endif
sp += -1;
{
#line 1596 "prim"
#if defined(DOUBLY_INDIRECT)
n=2;
#else
# if defined(DIRECT_THREADED)
n=0;
# else
n=1;
# endif
#endif
#line 8070 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n=", vm_out); printarg_n(n);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n,spTOS);
LABEL2(threading_method)
NEXT_P1_5;
LABEL3(threading_method)
DO_GOTO;
}

GROUPADD(4)
GROUP( hostos, 179)
LABEL(paren_key_file) /* key-file ( wfileid -- c ) S1 -- S1  */
/* Read one character @i{c} from @i{wfileid}.  This word disables
buffering for @i{wfileid}.  If you want to read characters from a
terminal in non-canonical (raw) mode, you have to put the terminal in
non-canonical mode yourself (using the C interface); the exception is
@code{stdin}: Gforth automatically puts it into non-canonical mode. */
NAME("key-file")
{
DEF_CA
MAYBE_UNUSED Cell wfileid;
Char c;
NEXT_P0;
vm_Cell2w(spTOS,wfileid);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" wfileid=", vm_out); printarg_w(wfileid);
}
#endif
{
#line 1618 "prim"
#ifdef HAS_FILE
fflush(stdout);
c = key((FILE*)wfileid);
#else
c = key(stdin);
#endif
#line 8115 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" c=", vm_out); printarg_c(c);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_c2Cell(c,spTOS);
LABEL2(paren_key_file)
NEXT_P1_5;
LABEL3(paren_key_file)
DO_GOTO;
}

LABEL(key_q_file) /* key?-file ( wfileid -- f ) S1 -- S1  */
/* @i{f} is true if at least one character can be read from @i{wfileid}
without blocking.  If you also want to use @code{read-file} or
@code{read-line} on the file, you have to call @code{key?-file} or
@code{key-file} first (these two words disable buffering). */
NAME("key?-file")
{
DEF_CA
MAYBE_UNUSED Cell wfileid;
Bool f;
NEXT_P0;
vm_Cell2w(spTOS,wfileid);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" wfileid=", vm_out); printarg_w(wfileid);
}
#endif
{
#line 1630 "prim"
#ifdef HAS_FILE
fflush(stdout);
f = key_query((FILE*)wfileid);
#else
f = key_query(stdin);
#endif
#line 8157 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
LABEL2(key_q_file)
NEXT_P1_5;
LABEL3(key_q_file)
DO_GOTO;
}

LABEL(stdin) /* stdin ( -- wfileid ) S1 -- S1  */
/* The standard input file of the Gforth process. */
NAME("stdin")
{
DEF_CA
Cell wfileid;
NEXT_P0;
sp[0] = spTOS;
#ifdef VM_DEBUG
if (vm_debug) {
}
#endif
sp += -1;
{
#line 1639 "prim"
wfileid = (Cell)stdin;
#line 8190 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" wfileid=", vm_out); printarg_w(wfileid);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(wfileid,spTOS);
LABEL2(stdin)
NEXT_P1_5;
LABEL3(stdin)
DO_GOTO;
}

LABEL(stdout) /* stdout ( -- wfileid ) S1 -- S1  */
/* The standard output file of the Gforth process. */
NAME("stdout")
{
DEF_CA
Cell wfileid;
NEXT_P0;
sp[0] = spTOS;
#ifdef VM_DEBUG
if (vm_debug) {
}
#endif
sp += -1;
{
#line 1643 "prim"
wfileid = (Cell)stdout;
#line 8223 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" wfileid=", vm_out); printarg_w(wfileid);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(wfileid,spTOS);
LABEL2(stdout)
NEXT_P1_5;
LABEL3(stdout)
DO_GOTO;
}

LABEL(stderr) /* stderr ( -- wfileid ) S1 -- S1  */
/* The standard error output file of the Gforth process. */
NAME("stderr")
{
DEF_CA
Cell wfileid;
NEXT_P0;
sp[0] = spTOS;
#ifdef VM_DEBUG
if (vm_debug) {
}
#endif
sp += -1;
{
#line 1647 "prim"
wfileid = (Cell)stderr;
#line 8256 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" wfileid=", vm_out); printarg_w(wfileid);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(wfileid,spTOS);
LABEL2(stderr)
NEXT_P1_5;
LABEL3(stderr)
DO_GOTO;
}

GROUPADD(5)
#ifdef HAS_OS
LABEL(form) /* form ( -- urows ucols ) S1 -- S1  */
/* The number of lines and columns in the terminal. These numbers may
change with the window size.  Note that it depends on the OS whether
this reflects the actual size and changes with the window size
(currently only on Unix-like OSs).  On other OSs you just get a
default, and can tell Gforth the terminal size by setting the
environment variables @code{COLUMNS} and @code{LINES} before starting
Gforth. */
NAME("form")
{
DEF_CA
UCell urows;
UCell ucols;
NEXT_P0;
sp[0] = spTOS;
#ifdef VM_DEBUG
if (vm_debug) {
}
#endif
sp += -2;
{
#line 1659 "prim"
/* we could block SIGWINCH here to get a consistent size, but I don't
 think this is necessary or always beneficial */
urows=rows;
ucols=cols;
#line 8301 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" urows=", vm_out); printarg_u(urows);
fputs(" ucols=", vm_out); printarg_u(ucols);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_u2Cell(urows,sp[1]);
vm_u2Cell(ucols,spTOS);
LABEL2(form)
NEXT_P1_5;
LABEL3(form)
DO_GOTO;
}

LABEL(wcwidth) /* wcwidth ( u -- n ) S1 -- S1  */
/* The number of fixed-width characters per unicode character u */
NAME("wcwidth")
{
DEF_CA
MAYBE_UNUSED UCell u;
Cell n;
NEXT_P0;
vm_Cell2u(spTOS,u);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" u=", vm_out); printarg_u(u);
}
#endif
{
#line 1666 "prim"
#ifdef HAVE_WCWIDTH
n = wcwidth(u);
#else
n = 1;
#endif
#line 8341 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n=", vm_out); printarg_n(n);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n,spTOS);
LABEL2(wcwidth)
NEXT_P1_5;
LABEL3(wcwidth)
DO_GOTO;
}

LABEL(flush_icache) /* flush-icache ( c_addr u -- ) S1 -- S1  */
/* Make sure that the instruction cache of the processor (if there is
one) does not contain stale data at @i{c-addr} and @i{u} bytes
afterwards. @code{END-CODE} performs a @code{flush-icache}
automatically. Caveat: @code{flush-icache} might not work on your
installation; this is usually the case if direct threading is not
supported on your machine (take a look at your @file{machine.h}) and
your machine has a separate instruction cache. In such cases,
@code{flush-icache} does nothing instead of flushing the instruction
cache. */
NAME("flush-icache")
{
DEF_CA
MAYBE_UNUSED Char * c_addr;
MAYBE_UNUSED UCell u;
NEXT_P0;
vm_Cell2c_(sp[1],c_addr);
vm_Cell2u(spTOS,u);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_addr=", vm_out); printarg_c_(c_addr);
fputs(" u=", vm_out); printarg_u(u);
}
#endif
sp += 2;
{
#line 1682 "prim"
FLUSH_ICACHE((caddr_t)c_addr,u);
#line 8386 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
spTOS = sp[0];
LABEL2(flush_icache)
NEXT_P1_5;
LABEL3(flush_icache)
DO_GOTO;
}

LABEL(paren_bye) /* (bye) ( n -- ) S1 -- S1  */
/*  */
NAME("(bye)")
{
DEF_CA
MAYBE_UNUSED Cell n;
NEXT_P0;
vm_Cell2n(spTOS,n);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n=", vm_out); printarg_n(n);
}
#endif
sp += 1;
{
#line 1685 "prim"
SUPER_END;
return (Label *)n;
#line 8420 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
spTOS = sp[0];
LABEL2(paren_bye)
NEXT_P1_5;
LABEL3(paren_bye)
DO_GOTO;
}

LABEL(paren_system) /* (system) ( c_addr u -- wretval wior ) S1 -- S1  */
/*  */
NAME("(system)")
{
DEF_CA
MAYBE_UNUSED Char * c_addr;
MAYBE_UNUSED UCell u;
Cell wretval;
Cell wior;
NEXT_P0;
vm_Cell2c_(sp[1],c_addr);
vm_Cell2u(spTOS,u);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_addr=", vm_out); printarg_c_(c_addr);
fputs(" u=", vm_out); printarg_u(u);
}
#endif
{
#line 1689 "prim"
wretval = gforth_system(c_addr, u);  
wior = IOR(wretval==-1 || (wretval==127 && errno != 0));
#line 8458 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" wretval=", vm_out); printarg_w(wretval);
fputs(" wior=", vm_out); printarg_w(wior);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(wretval,sp[1]);
vm_w2Cell(wior,spTOS);
LABEL2(paren_system)
NEXT_P1_5;
LABEL3(paren_system)
DO_GOTO;
}

LABEL(getenv) /* getenv ( c_addr1 u1 -- c_addr2 u2 ) S1 -- S1  */
/* The string @i{c-addr1 u1} specifies an environment variable. The string @i{c-addr2 u2}
is the host operating system's expansion of that environment variable. If the
environment variable does not exist, @i{c-addr2 u2} specifies a string 0 characters
in length. */
NAME("getenv")
{
DEF_CA
MAYBE_UNUSED Char * c_addr1;
MAYBE_UNUSED UCell u1;
Char * c_addr2;
UCell u2;
NEXT_P0;
vm_Cell2c_(sp[1],c_addr1);
vm_Cell2u(spTOS,u1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_addr1=", vm_out); printarg_c_(c_addr1);
fputs(" u1=", vm_out); printarg_u(u1);
}
#endif
{
#line 1697 "prim"
/* close ' to keep fontify happy */
c_addr2 = (Char *)getenv(cstr(c_addr1,u1,1));
u2 = (c_addr2 == NULL ? 0 : strlen((char *)c_addr2));
#line 8503 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" c_addr2=", vm_out); printarg_c_(c_addr2);
fputs(" u2=", vm_out); printarg_u(u2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_c_2Cell(c_addr2,sp[1]);
vm_u2Cell(u2,spTOS);
LABEL2(getenv)
NEXT_P1_5;
LABEL3(getenv)
DO_GOTO;
}

LABEL(open_pipe) /* open-pipe ( c_addr u wfam -- wfileid wior ) S1 -- S1  */
/*  */
NAME("open-pipe")
{
DEF_CA
MAYBE_UNUSED Char * c_addr;
MAYBE_UNUSED UCell u;
MAYBE_UNUSED Cell wfam;
Cell wfileid;
Cell wior;
NEXT_P0;
vm_Cell2c_(sp[2],c_addr);
vm_Cell2u(sp[1],u);
vm_Cell2w(spTOS,wfam);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_addr=", vm_out); printarg_c_(c_addr);
fputs(" u=", vm_out); printarg_u(u);
fputs(" wfam=", vm_out); printarg_w(wfam);
}
#endif
sp += 1;
{
#line 1702 "prim"
wfileid=(Cell)popen(cstr(c_addr,u,1),pfileattr[wfam]); /* ~ expansion of 1st arg? */
wior = IOR(wfileid==0); /* !! the man page says that errno is not set reliably */
#line 8548 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" wfileid=", vm_out); printarg_w(wfileid);
fputs(" wior=", vm_out); printarg_w(wior);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(wfileid,sp[1]);
vm_w2Cell(wior,spTOS);
LABEL2(open_pipe)
NEXT_P1_5;
LABEL3(open_pipe)
DO_GOTO;
}

LABEL(close_pipe) /* close-pipe ( wfileid -- wretval wior ) S1 -- S1  */
/*  */
NAME("close-pipe")
{
DEF_CA
MAYBE_UNUSED Cell wfileid;
Cell wretval;
Cell wior;
NEXT_P0;
vm_Cell2w(spTOS,wfileid);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" wfileid=", vm_out); printarg_w(wfileid);
}
#endif
sp += -1;
{
#line 1706 "prim"
wretval = pclose((FILE *)wfileid);
wior = IOR(wretval==-1);
#line 8587 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" wretval=", vm_out); printarg_w(wretval);
fputs(" wior=", vm_out); printarg_w(wior);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(wretval,sp[1]);
vm_w2Cell(wior,spTOS);
LABEL2(close_pipe)
NEXT_P1_5;
LABEL3(close_pipe)
DO_GOTO;
}

LABEL(time_and_date) /* time&date ( -- nsec nmin nhour nday nmonth nyear ) S1 -- S1  */
/* Report the current time of day. Seconds, minutes and hours are numbered from 0.
Months are numbered from 1. */
NAME("time&date")
{
DEF_CA
Cell nsec;
Cell nmin;
Cell nhour;
Cell nday;
Cell nmonth;
Cell nyear;
NEXT_P0;
sp[0] = spTOS;
#ifdef VM_DEBUG
if (vm_debug) {
}
#endif
sp += -6;
{
#line 1712 "prim"
#if 1
time_t now;
struct tm *ltime;
time(&now);
ltime=localtime(&now);
#else
struct timeval time1;
struct timezone zone1;
struct tm *ltime;
gettimeofday(&time1,&zone1);
/* !! Single Unix specification: 
   If tzp is not a null pointer, the behaviour is unspecified. */
ltime=localtime((time_t *)&time1.tv_sec);
#endif
nyear =ltime->tm_year+1900;
nmonth=ltime->tm_mon+1;
nday  =ltime->tm_mday;
nhour =ltime->tm_hour;
nmin  =ltime->tm_min;
nsec  =ltime->tm_sec;
#line 8647 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" nsec=", vm_out); printarg_n(nsec);
fputs(" nmin=", vm_out); printarg_n(nmin);
fputs(" nhour=", vm_out); printarg_n(nhour);
fputs(" nday=", vm_out); printarg_n(nday);
fputs(" nmonth=", vm_out); printarg_n(nmonth);
fputs(" nyear=", vm_out); printarg_n(nyear);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(nsec,sp[5]);
vm_n2Cell(nmin,sp[4]);
vm_n2Cell(nhour,sp[3]);
vm_n2Cell(nday,sp[2]);
vm_n2Cell(nmonth,sp[1]);
vm_n2Cell(nyear,spTOS);
LABEL2(time_and_date)
NEXT_P1_5;
LABEL3(time_and_date)
DO_GOTO;
}

LABEL(ms) /* ms ( u -- ) S1 -- S1  */
/* Wait at least @i{n} milli-second. */
NAME("ms")
{
DEF_CA
MAYBE_UNUSED UCell u;
NEXT_P0;
vm_Cell2u(spTOS,u);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" u=", vm_out); printarg_u(u);
}
#endif
sp += 1;
{
#line 1735 "prim"
gforth_ms(u);
#line 8691 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
spTOS = sp[0];
LABEL2(ms)
NEXT_P1_5;
LABEL3(ms)
DO_GOTO;
}

LABEL(allocate) /* allocate ( u -- a_addr wior ) S1 -- S1  */
/* Allocate @i{u} address units of contiguous data space. The initial
contents of the data space is undefined. If the allocation is successful,
@i{a-addr} is the start address of the allocated region and @i{wior}
is 0. If the allocation fails, @i{a-addr} is undefined and @i{wior}
is a non-zero I/O result code. */
NAME("allocate")
{
DEF_CA
MAYBE_UNUSED UCell u;
Cell * a_addr;
Cell wior;
NEXT_P0;
vm_Cell2u(spTOS,u);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" u=", vm_out); printarg_u(u);
}
#endif
sp += -1;
{
#line 1743 "prim"
a_addr = (Cell *)malloc(u?u:1);
wior = IOR(a_addr==NULL);
#line 8731 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" a_addr=", vm_out); printarg_a_(a_addr);
fputs(" wior=", vm_out); printarg_w(wior);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_a_2Cell(a_addr,sp[1]);
vm_w2Cell(wior,spTOS);
LABEL2(allocate)
NEXT_P1_5;
LABEL3(allocate)
DO_GOTO;
}

LABEL(free) /* free ( a_addr -- wior ) S1 -- S1  */
/* Return the region of data space starting at @i{a-addr} to the system.
The region must originally have been obtained using @code{allocate} or
@code{resize}. If the operational is successful, @i{wior} is 0.
If the operation fails, @i{wior} is a non-zero I/O result code. */
NAME("free")
{
DEF_CA
MAYBE_UNUSED Cell * a_addr;
Cell wior;
NEXT_P0;
vm_Cell2a_(spTOS,a_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_addr=", vm_out); printarg_a_(a_addr);
}
#endif
{
#line 1751 "prim"
free(a_addr);
wior = 0;
#line 8771 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" wior=", vm_out); printarg_w(wior);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(wior,spTOS);
LABEL2(free)
NEXT_P1_5;
LABEL3(free)
DO_GOTO;
}

LABEL(resize) /* resize ( a_addr1 u -- a_addr2 wior ) S1 -- S1  */
/* Change the size of the allocated area at @i{a-addr1} to @i{u}
address units, possibly moving the contents to a different
area. @i{a-addr2} is the address of the resulting area.
If the operation is successful, @i{wior} is 0.
If the operation fails, @i{wior} is a non-zero
I/O result code. If @i{a-addr1} is 0, Gforth's (but not the Standard)
@code{resize} @code{allocate}s @i{u} address units. */
NAME("resize")
{
DEF_CA
MAYBE_UNUSED Cell * a_addr1;
MAYBE_UNUSED UCell u;
Cell * a_addr2;
Cell wior;
NEXT_P0;
vm_Cell2a_(sp[1],a_addr1);
vm_Cell2u(spTOS,u);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_addr1=", vm_out); printarg_a_(a_addr1);
fputs(" u=", vm_out); printarg_u(u);
}
#endif
{
#line 1762 "prim"
/* the following check is not necessary on most OSs, but it is needed
   on SunOS 4.1.2. */
/* close ' to keep fontify happy */
if (a_addr1==NULL)
  a_addr2 = (Cell *)malloc(u);
else
  a_addr2 = (Cell *)realloc(a_addr1, u);
wior = IOR(a_addr2==NULL);	/* !! Define a return code */
#line 8822 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" a_addr2=", vm_out); printarg_a_(a_addr2);
fputs(" wior=", vm_out); printarg_w(wior);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_a_2Cell(a_addr2,sp[1]);
vm_w2Cell(wior,spTOS);
LABEL2(resize)
NEXT_P1_5;
LABEL3(resize)
DO_GOTO;
}

LABEL(strerror) /* strerror ( n -- c_addr u ) S1 -- S1  */
/*  */
NAME("strerror")
{
DEF_CA
MAYBE_UNUSED Cell n;
Char * c_addr;
UCell u;
NEXT_P0;
vm_Cell2n(spTOS,n);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n=", vm_out); printarg_n(n);
}
#endif
sp += -1;
{
#line 1772 "prim"
c_addr = (Char *)strerror(n);
u = strlen((char *)c_addr);
#line 8861 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" c_addr=", vm_out); printarg_c_(c_addr);
fputs(" u=", vm_out); printarg_u(u);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_c_2Cell(c_addr,sp[1]);
vm_u2Cell(u,spTOS);
LABEL2(strerror)
NEXT_P1_5;
LABEL3(strerror)
DO_GOTO;
}

LABEL(strsignal) /* strsignal ( n -- c_addr u ) S1 -- S1  */
/*  */
NAME("strsignal")
{
DEF_CA
MAYBE_UNUSED Cell n;
Char * c_addr;
UCell u;
NEXT_P0;
vm_Cell2n(spTOS,n);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n=", vm_out); printarg_n(n);
}
#endif
sp += -1;
{
#line 1776 "prim"
c_addr = (Char *)strsignal(n);
u = strlen((char *)c_addr);
#line 8900 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" c_addr=", vm_out); printarg_c_(c_addr);
fputs(" u=", vm_out); printarg_u(u);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_c_2Cell(c_addr,sp[1]);
vm_u2Cell(u,spTOS);
LABEL2(strsignal)
NEXT_P1_5;
LABEL3(strsignal)
DO_GOTO;
}

LABEL(call_c) /* call-c ( ... w -- ... ) S1 -- S1  */
/* Call the C function pointed to by @i{w}. The C function has to
access the stack itself. The stack pointers are exported in the global
variables @code{gforth_SP} and @code{gforth_FP}. */
NAME("call-c")
{
DEF_CA
MAYBE_UNUSED Cell w;
NEXT_P0;
fp[0] = fpTOS;
vm_Cell2w(spTOS,w);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w=", vm_out); printarg_w(w);
}
#endif
sp += 1;
{
#line 1783 "prim"
/* This is a first attempt at support for calls to C. This may change in
   the future */
IF_fpTOS(fp[0]=fpTOS);
gforth_FP=fp;
gforth_SP=sp;
gforth_RP=rp;
gforth_LP=lp;
#ifdef HAS_LINKBACK
((void (*)())w)();
#else
((void (*)(void *))w)(gforth_pointers);
#endif
sp=gforth_SP;
fp=gforth_FP;
rp=gforth_RP;
lp=gforth_LP;
IF_fpTOS(fpTOS=fp[0]);
#line 8955 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
spTOS = sp[0];
fpTOS = fp[0];
LABEL2(call_c)
NEXT_P1_5;
LABEL3(call_c)
DO_GOTO;
}

GROUPADD(16)
#endif
GROUPADD(0)
#ifdef HAS_FILE
LABEL(close_file) /* close-file ( wfileid -- wior ) S1 -- S1  */
/*  */
NAME("close-file")
{
DEF_CA
MAYBE_UNUSED Cell wfileid;
Cell wior;
NEXT_P0;
vm_Cell2w(spTOS,wfileid);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" wfileid=", vm_out); printarg_w(wfileid);
}
#endif
{
#line 1805 "prim"
wior = IOR(fclose((FILE *)wfileid)==EOF);
#line 8993 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" wior=", vm_out); printarg_w(wior);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(wior,spTOS);
LABEL2(close_file)
NEXT_P1_5;
LABEL3(close_file)
DO_GOTO;
}

LABEL(open_file) /* open-file ( c_addr u wfam -- wfileid wior ) S1 -- S1  */
/*  */
NAME("open-file")
{
DEF_CA
MAYBE_UNUSED Char * c_addr;
MAYBE_UNUSED UCell u;
MAYBE_UNUSED Cell wfam;
Cell wfileid;
Cell wior;
NEXT_P0;
vm_Cell2c_(sp[2],c_addr);
vm_Cell2u(sp[1],u);
vm_Cell2w(spTOS,wfam);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_addr=", vm_out); printarg_c_(c_addr);
fputs(" u=", vm_out); printarg_u(u);
fputs(" wfam=", vm_out); printarg_w(wfam);
}
#endif
sp += 1;
{
#line 1808 "prim"
wfileid = opencreate_file(tilde_cstr(c_addr,u,1), wfam, 0, &wior);
#line 9035 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" wfileid=", vm_out); printarg_w(wfileid);
fputs(" wior=", vm_out); printarg_w(wior);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(wfileid,sp[1]);
vm_w2Cell(wior,spTOS);
LABEL2(open_file)
NEXT_P1_5;
LABEL3(open_file)
DO_GOTO;
}

LABEL(create_file) /* create-file ( c_addr u wfam -- wfileid wior ) S1 -- S1  */
/*  */
NAME("create-file")
{
DEF_CA
MAYBE_UNUSED Char * c_addr;
MAYBE_UNUSED UCell u;
MAYBE_UNUSED Cell wfam;
Cell wfileid;
Cell wior;
NEXT_P0;
vm_Cell2c_(sp[2],c_addr);
vm_Cell2u(sp[1],u);
vm_Cell2w(spTOS,wfam);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_addr=", vm_out); printarg_c_(c_addr);
fputs(" u=", vm_out); printarg_u(u);
fputs(" wfam=", vm_out); printarg_w(wfam);
}
#endif
sp += 1;
{
#line 1811 "prim"
wfileid = opencreate_file(tilde_cstr(c_addr,u,1), wfam, O_CREAT|O_TRUNC, &wior);
#line 9079 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" wfileid=", vm_out); printarg_w(wfileid);
fputs(" wior=", vm_out); printarg_w(wior);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(wfileid,sp[1]);
vm_w2Cell(wior,spTOS);
LABEL2(create_file)
NEXT_P1_5;
LABEL3(create_file)
DO_GOTO;
}

LABEL(delete_file) /* delete-file ( c_addr u -- wior ) S1 -- S1  */
/*  */
NAME("delete-file")
{
DEF_CA
MAYBE_UNUSED Char * c_addr;
MAYBE_UNUSED UCell u;
Cell wior;
NEXT_P0;
vm_Cell2c_(sp[1],c_addr);
vm_Cell2u(spTOS,u);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_addr=", vm_out); printarg_c_(c_addr);
fputs(" u=", vm_out); printarg_u(u);
}
#endif
sp += 1;
{
#line 1814 "prim"
wior = IOR(unlink(tilde_cstr(c_addr, u, 1))==-1);
#line 9119 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" wior=", vm_out); printarg_w(wior);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(wior,spTOS);
LABEL2(delete_file)
NEXT_P1_5;
LABEL3(delete_file)
DO_GOTO;
}

LABEL(rename_file) /* rename-file ( c_addr1 u1 c_addr2 u2 -- wior ) S1 -- S1  */
/* Rename file @i{c_addr1 u1} to new name @i{c_addr2 u2} */
NAME("rename-file")
{
DEF_CA
MAYBE_UNUSED Char * c_addr1;
MAYBE_UNUSED UCell u1;
MAYBE_UNUSED Char * c_addr2;
MAYBE_UNUSED UCell u2;
Cell wior;
NEXT_P0;
vm_Cell2c_(sp[3],c_addr1);
vm_Cell2u(sp[2],u1);
vm_Cell2c_(sp[1],c_addr2);
vm_Cell2u(spTOS,u2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_addr1=", vm_out); printarg_c_(c_addr1);
fputs(" u1=", vm_out); printarg_u(u1);
fputs(" c_addr2=", vm_out); printarg_c_(c_addr2);
fputs(" u2=", vm_out); printarg_u(u2);
}
#endif
sp += 3;
{
#line 1818 "prim"
wior = rename_file(c_addr1, u1, c_addr2, u2);
#line 9163 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" wior=", vm_out); printarg_w(wior);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(wior,spTOS);
LABEL2(rename_file)
NEXT_P1_5;
LABEL3(rename_file)
DO_GOTO;
}

LABEL(file_position) /* file-position ( wfileid -- ud wior ) S1 -- S1  */
/*  */
NAME("file-position")
{
DEF_CA
MAYBE_UNUSED Cell wfileid;
UDCell ud;
Cell wior;
NEXT_P0;
vm_Cell2w(spTOS,wfileid);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" wfileid=", vm_out); printarg_w(wfileid);
}
#endif
sp += -2;
{
#line 1821 "prim"
/* !! use tell and lseek? */
ud = OFF2UD(ftello((FILE *)wfileid));
wior = IOR(UD2OFF(ud)==-1);
#line 9201 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" ud=", vm_out); printarg_ud(ud);
fputs(" wior=", vm_out); printarg_w(wior);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_ud2twoCell(ud, sp[2], sp[1])
vm_w2Cell(wior,spTOS);
LABEL2(file_position)
NEXT_P1_5;
LABEL3(file_position)
DO_GOTO;
}

LABEL(reposition_file) /* reposition-file ( ud wfileid -- wior ) S1 -- S1  */
/*  */
NAME("reposition-file")
{
DEF_CA
MAYBE_UNUSED UDCell ud;
MAYBE_UNUSED Cell wfileid;
Cell wior;
NEXT_P0;
vm_twoCell2ud(sp[2], sp[1], ud)
vm_Cell2w(spTOS,wfileid);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" ud=", vm_out); printarg_ud(ud);
fputs(" wfileid=", vm_out); printarg_w(wfileid);
}
#endif
sp += 2;
{
#line 1826 "prim"
wior = IOR(fseeko((FILE *)wfileid, UD2OFF(ud), SEEK_SET)==-1);
#line 9241 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" wior=", vm_out); printarg_w(wior);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(wior,spTOS);
LABEL2(reposition_file)
NEXT_P1_5;
LABEL3(reposition_file)
DO_GOTO;
}

LABEL(file_size) /* file-size ( wfileid -- ud wior ) S1 -- S1  */
/*  */
NAME("file-size")
{
DEF_CA
MAYBE_UNUSED Cell wfileid;
UDCell ud;
Cell wior;
NEXT_P0;
vm_Cell2w(spTOS,wfileid);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" wfileid=", vm_out); printarg_w(wfileid);
}
#endif
sp += -2;
{
#line 1829 "prim"
struct stat buf;
wior = IOR(fstat(fileno((FILE *)wfileid), &buf)==-1);
ud = OFF2UD(buf.st_size);
#line 9279 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" ud=", vm_out); printarg_ud(ud);
fputs(" wior=", vm_out); printarg_w(wior);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_ud2twoCell(ud, sp[2], sp[1])
vm_w2Cell(wior,spTOS);
LABEL2(file_size)
NEXT_P1_5;
LABEL3(file_size)
DO_GOTO;
}

LABEL(resize_file) /* resize-file ( ud wfileid -- wior ) S1 -- S1  */
/*  */
NAME("resize-file")
{
DEF_CA
MAYBE_UNUSED UDCell ud;
MAYBE_UNUSED Cell wfileid;
Cell wior;
NEXT_P0;
vm_twoCell2ud(sp[2], sp[1], ud)
vm_Cell2w(spTOS,wfileid);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" ud=", vm_out); printarg_ud(ud);
fputs(" wfileid=", vm_out); printarg_w(wfileid);
}
#endif
sp += 2;
{
#line 1834 "prim"
wior = IOR(ftruncate(fileno((FILE *)wfileid), UD2OFF(ud))==-1);
#line 9319 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" wior=", vm_out); printarg_w(wior);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(wior,spTOS);
LABEL2(resize_file)
NEXT_P1_5;
LABEL3(resize_file)
DO_GOTO;
}

LABEL(read_file) /* read-file ( c_addr u1 wfileid -- u2 wior ) S1 -- S1  */
/*  */
NAME("read-file")
{
DEF_CA
MAYBE_UNUSED Char * c_addr;
MAYBE_UNUSED UCell u1;
MAYBE_UNUSED Cell wfileid;
UCell u2;
Cell wior;
NEXT_P0;
vm_Cell2c_(sp[2],c_addr);
vm_Cell2u(sp[1],u1);
vm_Cell2w(spTOS,wfileid);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_addr=", vm_out); printarg_c_(c_addr);
fputs(" u1=", vm_out); printarg_u(u1);
fputs(" wfileid=", vm_out); printarg_w(wfileid);
}
#endif
sp += 1;
{
#line 1837 "prim"
/* !! fread does not guarantee enough */
u2 = fread(c_addr, sizeof(Char), u1, (FILE *)wfileid);
wior = FILEIO(u2<u1 && ferror((FILE *)wfileid));
/* !! is the value of ferror errno-compatible? */
if (wior)
  clearerr((FILE *)wfileid);
#line 9366 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" u2=", vm_out); printarg_u(u2);
fputs(" wior=", vm_out); printarg_w(wior);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_u2Cell(u2,sp[1]);
vm_w2Cell(wior,spTOS);
LABEL2(read_file)
NEXT_P1_5;
LABEL3(read_file)
DO_GOTO;
}

LABEL(paren_read_line) /* (read-line) ( c_addr u1 wfileid -- u2 flag u3 wior ) S1 -- S1  */
/*  */
NAME("(read-line)")
{
DEF_CA
MAYBE_UNUSED Char * c_addr;
MAYBE_UNUSED UCell u1;
MAYBE_UNUSED Cell wfileid;
UCell u2;
Bool flag;
UCell u3;
Cell wior;
NEXT_P0;
vm_Cell2c_(sp[2],c_addr);
vm_Cell2u(sp[1],u1);
vm_Cell2w(spTOS,wfileid);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_addr=", vm_out); printarg_c_(c_addr);
fputs(" u1=", vm_out); printarg_u(u1);
fputs(" wfileid=", vm_out); printarg_w(wfileid);
}
#endif
sp += -1;
{
#line 1845 "prim"
struct Cellquad r = read_line(c_addr, u1, wfileid);
u2   = r.n1;
flag = r.n2;
u3   = r.n3;
wior = r.n4;
#line 9416 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" u2=", vm_out); printarg_u(u2);
fputs(" flag=", vm_out); printarg_f(flag);
fputs(" u3=", vm_out); printarg_u(u3);
fputs(" wior=", vm_out); printarg_w(wior);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_u2Cell(u2,sp[3]);
vm_f2Cell(flag,sp[2]);
vm_u2Cell(u3,sp[1]);
vm_w2Cell(wior,spTOS);
LABEL2(paren_read_line)
NEXT_P1_5;
LABEL3(paren_read_line)
DO_GOTO;
}

GROUPADD(11)
#endif
LABEL(write_file) /* write-file ( c_addr u1 wfileid -- wior ) S1 -- S1  */
/*  */
NAME("write-file")
{
DEF_CA
MAYBE_UNUSED Char * c_addr;
MAYBE_UNUSED UCell u1;
MAYBE_UNUSED Cell wfileid;
Cell wior;
NEXT_P0;
vm_Cell2c_(sp[2],c_addr);
vm_Cell2u(sp[1],u1);
vm_Cell2w(spTOS,wfileid);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_addr=", vm_out); printarg_c_(c_addr);
fputs(" u1=", vm_out); printarg_u(u1);
fputs(" wfileid=", vm_out); printarg_w(wfileid);
}
#endif
sp += 2;
{
#line 1854 "prim"
/* !! fwrite does not guarantee enough */
#ifdef HAS_FILE
{
  UCell u2 = fwrite(c_addr, sizeof(Char), u1, (FILE *)wfileid);
  wior = FILEIO(u2<u1 && ferror((FILE *)wfileid));
  if (wior)
    clearerr((FILE *)wfileid);
}
#else
TYPE(c_addr, u1);
#endif
#line 9475 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" wior=", vm_out); printarg_w(wior);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(wior,spTOS);
LABEL2(write_file)
NEXT_P1_5;
LABEL3(write_file)
DO_GOTO;
}

LABEL(emit_file) /* emit-file ( c wfileid -- wior ) S1 -- S1  */
/*  */
NAME("emit-file")
{
DEF_CA
MAYBE_UNUSED Char c;
MAYBE_UNUSED Cell wfileid;
Cell wior;
NEXT_P0;
vm_Cell2c(sp[1],c);
vm_Cell2w(spTOS,wfileid);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c=", vm_out); printarg_c(c);
fputs(" wfileid=", vm_out); printarg_w(wfileid);
}
#endif
sp += 1;
{
#line 1867 "prim"
#ifdef HAS_FILE
wior = FILEIO(putc(c, (FILE *)wfileid)==EOF);
if (wior)
  clearerr((FILE *)wfileid);
#else
PUTC(c);
#endif
#line 9519 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" wior=", vm_out); printarg_w(wior);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(wior,spTOS);
LABEL2(emit_file)
NEXT_P1_5;
LABEL3(emit_file)
DO_GOTO;
}

GROUPADD(2)
#ifdef HAS_FILE
LABEL(flush_file) /* flush-file ( wfileid -- wior ) S1 -- S1  */
/*  */
NAME("flush-file")
{
DEF_CA
MAYBE_UNUSED Cell wfileid;
Cell wior;
NEXT_P0;
vm_Cell2w(spTOS,wfileid);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" wfileid=", vm_out); printarg_w(wfileid);
}
#endif
{
#line 1878 "prim"
wior = IOR(fflush((FILE *) wfileid)==EOF);
#line 9555 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" wior=", vm_out); printarg_w(wior);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(wior,spTOS);
LABEL2(flush_file)
NEXT_P1_5;
LABEL3(flush_file)
DO_GOTO;
}

LABEL(file_status) /* file-status ( c_addr u -- wfam wior ) S1 -- S1  */
/*  */
NAME("file-status")
{
DEF_CA
MAYBE_UNUSED Char * c_addr;
MAYBE_UNUSED UCell u;
Cell wfam;
Cell wior;
NEXT_P0;
vm_Cell2c_(sp[1],c_addr);
vm_Cell2u(spTOS,u);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_addr=", vm_out); printarg_c_(c_addr);
fputs(" u=", vm_out); printarg_u(u);
}
#endif
{
#line 1881 "prim"
struct Cellpair r = file_status(c_addr, u);
wfam = r.n1;
wior = r.n2;
#line 9595 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" wfam=", vm_out); printarg_w(wfam);
fputs(" wior=", vm_out); printarg_w(wior);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(wfam,sp[1]);
vm_w2Cell(wior,spTOS);
LABEL2(file_status)
NEXT_P1_5;
LABEL3(file_status)
DO_GOTO;
}

LABEL(file_eof_query) /* file-eof? ( wfileid -- flag ) S1 -- S1  */
/*  */
NAME("file-eof?")
{
DEF_CA
MAYBE_UNUSED Cell wfileid;
Bool flag;
NEXT_P0;
vm_Cell2w(spTOS,wfileid);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" wfileid=", vm_out); printarg_w(wfileid);
}
#endif
{
#line 1886 "prim"
flag = FLAG(feof((FILE *) wfileid));
#line 9631 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" flag=", vm_out); printarg_f(flag);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(flag,spTOS);
LABEL2(file_eof_query)
NEXT_P1_5;
LABEL3(file_eof_query)
DO_GOTO;
}

LABEL(open_dir) /* open-dir ( c_addr u -- wdirid wior ) S1 -- S1  */
/* Open the directory specified by @i{c-addr, u}
and return @i{dir-id} for futher access to it. */
NAME("open-dir")
{
DEF_CA
MAYBE_UNUSED Char * c_addr;
MAYBE_UNUSED UCell u;
Cell wdirid;
Cell wior;
NEXT_P0;
vm_Cell2c_(sp[1],c_addr);
vm_Cell2u(spTOS,u);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_addr=", vm_out); printarg_c_(c_addr);
fputs(" u=", vm_out); printarg_u(u);
}
#endif
{
#line 1891 "prim"
wdirid = (Cell)opendir(tilde_cstr(c_addr, u, 1));
wior =  IOR(wdirid == 0);
#line 9671 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" wdirid=", vm_out); printarg_w(wdirid);
fputs(" wior=", vm_out); printarg_w(wior);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(wdirid,sp[1]);
vm_w2Cell(wior,spTOS);
LABEL2(open_dir)
NEXT_P1_5;
LABEL3(open_dir)
DO_GOTO;
}

LABEL(read_dir) /* read-dir ( c_addr u1 wdirid -- u2 flag wior ) S1 -- S1  */
/* Attempt to read the next entry from the directory specified
by @i{dir-id} to the buffer of length @i{u1} at address @i{c-addr}. 
If the attempt fails because there is no more entries,
@i{ior}=0, @i{flag}=0, @i{u2}=0, and the buffer is unmodified.
If the attempt to read the next entry fails because of any other reason, 
return @i{ior}<>0.
If the attempt succeeds, store file name to the buffer at @i{c-addr}
and return @i{ior}=0, @i{flag}=true and @i{u2} equal to the size of the file name.
If the length of the file name is greater than @i{u1}, 
store first @i{u1} characters from file name into the buffer and
indicate "name too long" with @i{ior}, @i{flag}=true, and @i{u2}=@i{u1}. */
NAME("read-dir")
{
DEF_CA
MAYBE_UNUSED Char * c_addr;
MAYBE_UNUSED UCell u1;
MAYBE_UNUSED Cell wdirid;
UCell u2;
Bool flag;
Cell wior;
NEXT_P0;
vm_Cell2c_(sp[2],c_addr);
vm_Cell2u(sp[1],u1);
vm_Cell2w(spTOS,wdirid);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_addr=", vm_out); printarg_c_(c_addr);
fputs(" u1=", vm_out); printarg_u(u1);
fputs(" wdirid=", vm_out); printarg_w(wdirid);
}
#endif
{
#line 1906 "prim"
struct dirent * dent;
dent = readdir((DIR *)wdirid);
wior = 0;
flag = -1;
if(dent == NULL) {
  u2 = 0;
  flag = 0;
} else {
  u2 = strlen((char *)dent->d_name);
  if(u2 > u1) {
    u2 = u1;
    wior = -512-ENAMETOOLONG;
  }
  memmove(c_addr, dent->d_name, u2);
}
#line 9739 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" u2=", vm_out); printarg_u(u2);
fputs(" flag=", vm_out); printarg_f(flag);
fputs(" wior=", vm_out); printarg_w(wior);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_u2Cell(u2,sp[2]);
vm_f2Cell(flag,sp[1]);
vm_w2Cell(wior,spTOS);
LABEL2(read_dir)
NEXT_P1_5;
LABEL3(read_dir)
DO_GOTO;
}

LABEL(close_dir) /* close-dir ( wdirid -- wior ) S1 -- S1  */
/* Close the directory specified by @i{dir-id}. */
NAME("close-dir")
{
DEF_CA
MAYBE_UNUSED Cell wdirid;
Cell wior;
NEXT_P0;
vm_Cell2w(spTOS,wdirid);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" wdirid=", vm_out); printarg_w(wdirid);
}
#endif
{
#line 1924 "prim"
wior = IOR(closedir((DIR *)wdirid));
#line 9777 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" wior=", vm_out); printarg_w(wior);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(wior,spTOS);
LABEL2(close_dir)
NEXT_P1_5;
LABEL3(close_dir)
DO_GOTO;
}

LABEL(match_file) /* filename-match ( c_addr1 u1 c_addr2 u2 -- flag ) S1 -- S1  */
/*  */
NAME("filename-match")
{
DEF_CA
MAYBE_UNUSED Char * c_addr1;
MAYBE_UNUSED UCell u1;
MAYBE_UNUSED Char * c_addr2;
MAYBE_UNUSED UCell u2;
Bool flag;
NEXT_P0;
vm_Cell2c_(sp[3],c_addr1);
vm_Cell2u(sp[2],u1);
vm_Cell2c_(sp[1],c_addr2);
vm_Cell2u(spTOS,u2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_addr1=", vm_out); printarg_c_(c_addr1);
fputs(" u1=", vm_out); printarg_u(u1);
fputs(" c_addr2=", vm_out); printarg_c_(c_addr2);
fputs(" u2=", vm_out); printarg_u(u2);
}
#endif
sp += 3;
{
#line 1927 "prim"
char * string = cstr(c_addr1, u1, 1);
char * pattern = cstr(c_addr2, u2, 0);
flag = FLAG(!fnmatch(pattern, string, 0));
#line 9823 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" flag=", vm_out); printarg_f(flag);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(flag,spTOS);
LABEL2(match_file)
NEXT_P1_5;
LABEL3(match_file)
DO_GOTO;
}

LABEL(set_dir) /* set-dir ( c_addr u -- wior ) S1 -- S1  */
/* Change the current directory to @i{c-addr, u}.
Return an error if this is not possible */
NAME("set-dir")
{
DEF_CA
MAYBE_UNUSED Char * c_addr;
MAYBE_UNUSED UCell u;
Cell wior;
NEXT_P0;
vm_Cell2c_(sp[1],c_addr);
vm_Cell2u(spTOS,u);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_addr=", vm_out); printarg_c_(c_addr);
fputs(" u=", vm_out); printarg_u(u);
}
#endif
sp += 1;
{
#line 1934 "prim"
wior = IOR(chdir(tilde_cstr(c_addr, u, 1)));
#line 9862 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" wior=", vm_out); printarg_w(wior);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(wior,spTOS);
LABEL2(set_dir)
NEXT_P1_5;
LABEL3(set_dir)
DO_GOTO;
}

LABEL(get_dir) /* get-dir ( c_addr1 u1 -- c_addr2 u2 ) S1 -- S1  */
/* Store the current directory in the buffer specified by @i{c-addr1, u1}.
If the buffer size is not sufficient, return 0 0 */
NAME("get-dir")
{
DEF_CA
MAYBE_UNUSED Char * c_addr1;
MAYBE_UNUSED UCell u1;
Char * c_addr2;
UCell u2;
NEXT_P0;
vm_Cell2c_(sp[1],c_addr1);
vm_Cell2u(spTOS,u1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_addr1=", vm_out); printarg_c_(c_addr1);
fputs(" u1=", vm_out); printarg_u(u1);
}
#endif
{
#line 1939 "prim"
c_addr2 = (Char *)getcwd((char *)c_addr1, u1);
if(c_addr2 != NULL) {
  u2 = strlen((char *)c_addr2);
} else {
  u2 = 0;
}
#line 9906 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" c_addr2=", vm_out); printarg_c_(c_addr2);
fputs(" u2=", vm_out); printarg_u(u2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_c_2Cell(c_addr2,sp[1]);
vm_u2Cell(u2,spTOS);
LABEL2(get_dir)
NEXT_P1_5;
LABEL3(get_dir)
DO_GOTO;
}

LABEL(equals_mkdir) /* =mkdir ( c_addr u wmode -- wior ) S1 -- S1  */
/* Create directory @i{c-addr u} with mode @i{wmode}. */
NAME("=mkdir")
{
DEF_CA
MAYBE_UNUSED Char * c_addr;
MAYBE_UNUSED UCell u;
MAYBE_UNUSED Cell wmode;
Cell wior;
NEXT_P0;
vm_Cell2c_(sp[2],c_addr);
vm_Cell2u(sp[1],u);
vm_Cell2w(spTOS,wmode);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_addr=", vm_out); printarg_c_(c_addr);
fputs(" u=", vm_out); printarg_u(u);
fputs(" wmode=", vm_out); printarg_w(wmode);
}
#endif
sp += 2;
{
#line 1948 "prim"
wior = IOR(mkdir(tilde_cstr(c_addr,u,1),wmode));
#line 9949 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" wior=", vm_out); printarg_w(wior);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(wior,spTOS);
LABEL2(equals_mkdir)
NEXT_P1_5;
LABEL3(equals_mkdir)
DO_GOTO;
}

GROUPADD(10)
#endif
LABEL(newline) /* newline ( -- c_addr u ) S1 -- S1  */
/* String containing the newline sequence of the host OS */
NAME("newline")
{
DEF_CA
Char * c_addr;
UCell u;
NEXT_P0;
sp[0] = spTOS;
#ifdef VM_DEBUG
if (vm_debug) {
}
#endif
sp += -2;
{
#line 1954 "prim"
static const char newline[] = {
#if DIRSEP=='/'
/* Unix */
'\n'
#else
/* DOS, Win, OS/2 */
'\r','\n'
#endif
};
c_addr=(Char *)newline;
u=sizeof(newline);
#line 9995 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" c_addr=", vm_out); printarg_c_(c_addr);
fputs(" u=", vm_out); printarg_u(u);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_c_2Cell(c_addr,sp[1]);
vm_u2Cell(u,spTOS);
LABEL2(newline)
NEXT_P1_5;
LABEL3(newline)
DO_GOTO;
}

GROUPADD(1)
#ifdef HAS_OS
LABEL(utime) /* utime ( -- dtime ) S1 -- S1  */
/* Report the current time in microseconds since some epoch. */
NAME("utime")
{
DEF_CA
DCell dtime;
NEXT_P0;
sp[0] = spTOS;
#ifdef VM_DEBUG
if (vm_debug) {
}
#endif
sp += -2;
{
#line 1973 "prim"
struct timeval time1;
gettimeofday(&time1,NULL);
dtime = timeval2us(&time1);
#line 10034 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" dtime=", vm_out); printarg_d(dtime);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_d2twoCell(dtime, sp[1], spTOS)
LABEL2(utime)
NEXT_P1_5;
LABEL3(utime)
DO_GOTO;
}

LABEL(cputime) /* cputime ( -- duser dsystem ) S1 -- S1  */
/* duser and dsystem are the respective user- and system-level CPU
times used since the start of the Forth system (excluding child
processes), in microseconds (the granularity may be much larger,
however).  On platforms without the getrusage call, it reports elapsed
time (since some epoch) for duser and 0 for dsystem. */
NAME("cputime")
{
DEF_CA
DCell duser;
DCell dsystem;
NEXT_P0;
sp[0] = spTOS;
#ifdef VM_DEBUG
if (vm_debug) {
}
#endif
sp += -4;
{
#line 1983 "prim"
#ifdef HAVE_GETRUSAGE
struct rusage usage;
getrusage(RUSAGE_SELF, &usage);
duser = timeval2us(&usage.ru_utime);
dsystem = timeval2us(&usage.ru_stime);
#else
struct timeval time1;
gettimeofday(&time1,NULL);
duser = timeval2us(&time1);
dsystem = DZERO;
#endif
#line 10082 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" duser=", vm_out); printarg_d(duser);
fputs(" dsystem=", vm_out); printarg_d(dsystem);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_d2twoCell(duser, sp[3], sp[2])
vm_d2twoCell(dsystem, sp[1], spTOS)
LABEL2(cputime)
NEXT_P1_5;
LABEL3(cputime)
DO_GOTO;
}

GROUPADD(2)
#endif
GROUPADD(0)
#ifdef HAS_FLOATING
GROUPADD(0)
GROUP( floating, 226)
LABEL(f_equals) /* f= ( r1 r2 -- f ) S1 -- S1  */
/*  */
NAME("f=")
{
DEF_CA
MAYBE_UNUSED Float r1;
MAYBE_UNUSED Float r2;
Bool f;
NEXT_P0;
sp[0] = spTOS;
vm_Float2r(fp[1],r1);
vm_Float2r(fpTOS,r2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
fputs(" r2=", vm_out); printarg_r(r2);
}
#endif
sp += -1;
fp += 2;
{
#line 2001 "prim"
f = FLAG(r1==r2);
#line 2000
#line 10131 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
fpTOS = fp[0];
LABEL2(f_equals)
NEXT_P1_5;
LABEL3(f_equals)
DO_GOTO;
}

LABEL(f_not_equals) /* f<> ( r1 r2 -- f ) S1 -- S1  */
/*  */
NAME("f<>")
{
DEF_CA
MAYBE_UNUSED Float r1;
MAYBE_UNUSED Float r2;
Bool f;
NEXT_P0;
sp[0] = spTOS;
vm_Float2r(fp[1],r1);
vm_Float2r(fpTOS,r2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
fputs(" r2=", vm_out); printarg_r(r2);
}
#endif
sp += -1;
fp += 2;
{
#line 2001 "prim"
f = FLAG(r1!=r2);
#line 2000
#line 10173 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
fpTOS = fp[0];
LABEL2(f_not_equals)
NEXT_P1_5;
LABEL3(f_not_equals)
DO_GOTO;
}

LABEL(f_less_than) /* f< ( r1 r2 -- f ) S1 -- S1  */
/*  */
NAME("f<")
{
DEF_CA
MAYBE_UNUSED Float r1;
MAYBE_UNUSED Float r2;
Bool f;
NEXT_P0;
sp[0] = spTOS;
vm_Float2r(fp[1],r1);
vm_Float2r(fpTOS,r2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
fputs(" r2=", vm_out); printarg_r(r2);
}
#endif
sp += -1;
fp += 2;
{
#line 2001 "prim"
f = FLAG(r1<r2);
#line 2000
#line 10215 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
fpTOS = fp[0];
LABEL2(f_less_than)
NEXT_P1_5;
LABEL3(f_less_than)
DO_GOTO;
}

LABEL(f_greater_than) /* f> ( r1 r2 -- f ) S1 -- S1  */
/*  */
NAME("f>")
{
DEF_CA
MAYBE_UNUSED Float r1;
MAYBE_UNUSED Float r2;
Bool f;
NEXT_P0;
sp[0] = spTOS;
vm_Float2r(fp[1],r1);
vm_Float2r(fpTOS,r2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
fputs(" r2=", vm_out); printarg_r(r2);
}
#endif
sp += -1;
fp += 2;
{
#line 2001 "prim"
f = FLAG(r1>r2);
#line 2000
#line 10257 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
fpTOS = fp[0];
LABEL2(f_greater_than)
NEXT_P1_5;
LABEL3(f_greater_than)
DO_GOTO;
}

LABEL(f_less_or_equal) /* f<= ( r1 r2 -- f ) S1 -- S1  */
/*  */
NAME("f<=")
{
DEF_CA
MAYBE_UNUSED Float r1;
MAYBE_UNUSED Float r2;
Bool f;
NEXT_P0;
sp[0] = spTOS;
vm_Float2r(fp[1],r1);
vm_Float2r(fpTOS,r2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
fputs(" r2=", vm_out); printarg_r(r2);
}
#endif
sp += -1;
fp += 2;
{
#line 2001 "prim"
f = FLAG(r1<=r2);
#line 2000
#line 10299 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
fpTOS = fp[0];
LABEL2(f_less_or_equal)
NEXT_P1_5;
LABEL3(f_less_or_equal)
DO_GOTO;
}

LABEL(f_greater_or_equal) /* f>= ( r1 r2 -- f ) S1 -- S1  */
/*  */
NAME("f>=")
{
DEF_CA
MAYBE_UNUSED Float r1;
MAYBE_UNUSED Float r2;
Bool f;
NEXT_P0;
sp[0] = spTOS;
vm_Float2r(fp[1],r1);
vm_Float2r(fpTOS,r2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
fputs(" r2=", vm_out); printarg_r(r2);
}
#endif
sp += -1;
fp += 2;
{
#line 2001 "prim"
f = FLAG(r1>=r2);
#line 2000
#line 10341 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
fpTOS = fp[0];
LABEL2(f_greater_or_equal)
NEXT_P1_5;
LABEL3(f_greater_or_equal)
DO_GOTO;
}

LABEL(f_zero_equals) /* f0= ( r -- f ) S1 -- S1  */
/*  */
NAME("f0=")
{
DEF_CA
MAYBE_UNUSED Float r;
Bool f;
NEXT_P0;
sp[0] = spTOS;
vm_Float2r(fpTOS,r);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r=", vm_out); printarg_r(r);
}
#endif
sp += -1;
fp += 1;
{
#line 2002 "prim"
f = FLAG(r==0.);
#line 2001
#line 10380 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
fpTOS = fp[0];
LABEL2(f_zero_equals)
NEXT_P1_5;
LABEL3(f_zero_equals)
DO_GOTO;
}

LABEL(f_zero_not_equals) /* f0<> ( r -- f ) S1 -- S1  */
/*  */
NAME("f0<>")
{
DEF_CA
MAYBE_UNUSED Float r;
Bool f;
NEXT_P0;
sp[0] = spTOS;
vm_Float2r(fpTOS,r);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r=", vm_out); printarg_r(r);
}
#endif
sp += -1;
fp += 1;
{
#line 2002 "prim"
f = FLAG(r!=0.);
#line 2001
#line 10419 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
fpTOS = fp[0];
LABEL2(f_zero_not_equals)
NEXT_P1_5;
LABEL3(f_zero_not_equals)
DO_GOTO;
}

LABEL(f_zero_less_than) /* f0< ( r -- f ) S1 -- S1  */
/*  */
NAME("f0<")
{
DEF_CA
MAYBE_UNUSED Float r;
Bool f;
NEXT_P0;
sp[0] = spTOS;
vm_Float2r(fpTOS,r);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r=", vm_out); printarg_r(r);
}
#endif
sp += -1;
fp += 1;
{
#line 2002 "prim"
f = FLAG(r<0.);
#line 2001
#line 10458 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
fpTOS = fp[0];
LABEL2(f_zero_less_than)
NEXT_P1_5;
LABEL3(f_zero_less_than)
DO_GOTO;
}

LABEL(f_zero_greater_than) /* f0> ( r -- f ) S1 -- S1  */
/*  */
NAME("f0>")
{
DEF_CA
MAYBE_UNUSED Float r;
Bool f;
NEXT_P0;
sp[0] = spTOS;
vm_Float2r(fpTOS,r);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r=", vm_out); printarg_r(r);
}
#endif
sp += -1;
fp += 1;
{
#line 2002 "prim"
f = FLAG(r>0.);
#line 2001
#line 10497 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
fpTOS = fp[0];
LABEL2(f_zero_greater_than)
NEXT_P1_5;
LABEL3(f_zero_greater_than)
DO_GOTO;
}

LABEL(f_zero_less_or_equal) /* f0<= ( r -- f ) S1 -- S1  */
/*  */
NAME("f0<=")
{
DEF_CA
MAYBE_UNUSED Float r;
Bool f;
NEXT_P0;
sp[0] = spTOS;
vm_Float2r(fpTOS,r);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r=", vm_out); printarg_r(r);
}
#endif
sp += -1;
fp += 1;
{
#line 2002 "prim"
f = FLAG(r<=0.);
#line 2001
#line 10536 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
fpTOS = fp[0];
LABEL2(f_zero_less_or_equal)
NEXT_P1_5;
LABEL3(f_zero_less_or_equal)
DO_GOTO;
}

LABEL(f_zero_greater_or_equal) /* f0>= ( r -- f ) S1 -- S1  */
/*  */
NAME("f0>=")
{
DEF_CA
MAYBE_UNUSED Float r;
Bool f;
NEXT_P0;
sp[0] = spTOS;
vm_Float2r(fpTOS,r);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r=", vm_out); printarg_r(r);
}
#endif
sp += -1;
fp += 1;
{
#line 2002 "prim"
f = FLAG(r>=0.);
#line 2001
#line 10575 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
fpTOS = fp[0];
LABEL2(f_zero_greater_or_equal)
NEXT_P1_5;
LABEL3(f_zero_greater_or_equal)
DO_GOTO;
}

LABEL(s_to_f) /* s>f ( n -- r ) S1 -- S1  */
/*  */
NAME("s>f")
{
DEF_CA
MAYBE_UNUSED Cell n;
Float r;
NEXT_P0;
fp[0] = fpTOS;
vm_Cell2n(spTOS,n);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n=", vm_out); printarg_n(n);
}
#endif
sp += 1;
fp += -1;
{
#line 2005 "prim"
r = n;
#line 10613 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r=", vm_out); printarg_r(r);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r,fpTOS);
spTOS = sp[0];
LABEL2(s_to_f)
NEXT_P1_5;
LABEL3(s_to_f)
DO_GOTO;
}

LABEL(d_to_f) /* d>f ( d -- r ) S1 -- S1  */
/*  */
NAME("d>f")
{
DEF_CA
MAYBE_UNUSED DCell d;
Float r;
NEXT_P0;
fp[0] = fpTOS;
vm_twoCell2d(sp[1], spTOS, d)
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" d=", vm_out); printarg_d(d);
}
#endif
sp += 2;
fp += -1;
{
#line 2008 "prim"
#ifdef BUGGY_LL_D2F
extern double ldexp(double x, int exp);
if (DHI(d)<0) {
#ifdef BUGGY_LL_ADD
  DCell d2=dnegate(d);
#else
  DCell d2=-d;
#endif
  r = -(ldexp((Float)DHI(d2),CELL_BITS) + (Float)DLO(d2));
} else
  r = ldexp((Float)DHI(d),CELL_BITS) + (Float)DLO(d);
#else
r = d;
#endif
#line 10664 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r=", vm_out); printarg_r(r);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r,fpTOS);
spTOS = sp[0];
LABEL2(d_to_f)
NEXT_P1_5;
LABEL3(d_to_f)
DO_GOTO;
}

LABEL(f_to_d) /* f>d ( r -- d ) S1 -- S1  */
/*  */
NAME("f>d")
{
DEF_CA
MAYBE_UNUSED Float r;
DCell d;
NEXT_P0;
sp[0] = spTOS;
vm_Float2r(fpTOS,r);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r=", vm_out); printarg_r(r);
}
#endif
sp += -2;
fp += 1;
{
#line 2024 "prim"
extern DCell double2ll(Float r);
d = double2ll(r);
#line 10703 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" d=", vm_out); printarg_d(d);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_d2twoCell(d, sp[1], spTOS)
fpTOS = fp[0];
LABEL2(f_to_d)
NEXT_P1_5;
LABEL3(f_to_d)
DO_GOTO;
}

LABEL(f_to_s) /* f>s ( r -- n ) S1 -- S1  */
/*  */
NAME("f>s")
{
DEF_CA
MAYBE_UNUSED Float r;
Cell n;
NEXT_P0;
sp[0] = spTOS;
vm_Float2r(fpTOS,r);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r=", vm_out); printarg_r(r);
}
#endif
sp += -1;
fp += 1;
{
#line 2028 "prim"
n = (Cell)r;
#line 10741 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n=", vm_out); printarg_n(n);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n,spTOS);
fpTOS = fp[0];
LABEL2(f_to_s)
NEXT_P1_5;
LABEL3(f_to_s)
DO_GOTO;
}

LABEL(f_store) /* f! ( r f_addr -- ) S1 -- S1  */
/* Store @i{r} into the float at address @i{f-addr}. */
NAME("f!")
{
DEF_CA
MAYBE_UNUSED Float r;
MAYBE_UNUSED Float * f_addr;
NEXT_P0;
vm_Float2r(fpTOS,r);
vm_Cell2f_(spTOS,f_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r=", vm_out); printarg_r(r);
fputs(" f_addr=", vm_out); printarg_f_(f_addr);
}
#endif
sp += 1;
fp += 1;
{
#line 2032 "prim"
*f_addr = r;
#line 10780 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
spTOS = sp[0];
fpTOS = fp[0];
LABEL2(f_store)
NEXT_P1_5;
LABEL3(f_store)
DO_GOTO;
}

LABEL(f_fetch) /* f@ ( f_addr -- r ) S1 -- S1  */
/* @i{r} is the float at address @i{f-addr}. */
NAME("f@")
{
DEF_CA
MAYBE_UNUSED Float * f_addr;
Float r;
NEXT_P0;
fp[0] = fpTOS;
vm_Cell2f_(spTOS,f_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" f_addr=", vm_out); printarg_f_(f_addr);
}
#endif
sp += 1;
fp += -1;
{
#line 2036 "prim"
r = *f_addr;
#line 10817 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r=", vm_out); printarg_r(r);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r,fpTOS);
spTOS = sp[0];
LABEL2(f_fetch)
NEXT_P1_5;
LABEL3(f_fetch)
DO_GOTO;
}

LABEL(d_f_fetch) /* df@ ( df_addr -- r ) S1 -- S1  */
/* Fetch the double-precision IEEE floating-point value @i{r} from the address @i{df-addr}. */
NAME("df@")
{
DEF_CA
MAYBE_UNUSED DFloat * df_addr;
Float r;
NEXT_P0;
fp[0] = fpTOS;
vm_Cell2df_(spTOS,df_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" df_addr=", vm_out); printarg_df_(df_addr);
}
#endif
sp += 1;
fp += -1;
{
#line 2040 "prim"
#ifdef IEEE_FP
r = *df_addr;
#else
!! df@
#endif
#line 10859 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r=", vm_out); printarg_r(r);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r,fpTOS);
spTOS = sp[0];
LABEL2(d_f_fetch)
NEXT_P1_5;
LABEL3(d_f_fetch)
DO_GOTO;
}

LABEL(d_f_store) /* df! ( r df_addr -- ) S1 -- S1  */
/* Store @i{r} as double-precision IEEE floating-point value to the
address @i{df-addr}. */
NAME("df!")
{
DEF_CA
MAYBE_UNUSED Float r;
MAYBE_UNUSED DFloat * df_addr;
NEXT_P0;
vm_Float2r(fpTOS,r);
vm_Cell2df_(spTOS,df_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r=", vm_out); printarg_r(r);
fputs(" df_addr=", vm_out); printarg_df_(df_addr);
}
#endif
sp += 1;
fp += 1;
{
#line 2049 "prim"
#ifdef IEEE_FP
*df_addr = r;
#else
!! df!
#endif
#line 10903 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
spTOS = sp[0];
fpTOS = fp[0];
LABEL2(d_f_store)
NEXT_P1_5;
LABEL3(d_f_store)
DO_GOTO;
}

LABEL(s_f_fetch) /* sf@ ( sf_addr -- r ) S1 -- S1  */
/* Fetch the single-precision IEEE floating-point value @i{r} from the address @i{sf-addr}. */
NAME("sf@")
{
DEF_CA
MAYBE_UNUSED SFloat * sf_addr;
Float r;
NEXT_P0;
fp[0] = fpTOS;
vm_Cell2sf_(spTOS,sf_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" sf_addr=", vm_out); printarg_sf_(sf_addr);
}
#endif
sp += 1;
fp += -1;
{
#line 2057 "prim"
#ifdef IEEE_FP
r = *sf_addr;
#else
!! sf@
#endif
#line 10944 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r=", vm_out); printarg_r(r);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r,fpTOS);
spTOS = sp[0];
LABEL2(s_f_fetch)
NEXT_P1_5;
LABEL3(s_f_fetch)
DO_GOTO;
}

LABEL(s_f_store) /* sf! ( r sf_addr -- ) S1 -- S1  */
/* Store @i{r} as single-precision IEEE floating-point value to the
address @i{sf-addr}. */
NAME("sf!")
{
DEF_CA
MAYBE_UNUSED Float r;
MAYBE_UNUSED SFloat * sf_addr;
NEXT_P0;
vm_Float2r(fpTOS,r);
vm_Cell2sf_(spTOS,sf_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r=", vm_out); printarg_r(r);
fputs(" sf_addr=", vm_out); printarg_sf_(sf_addr);
}
#endif
sp += 1;
fp += 1;
{
#line 2066 "prim"
#ifdef IEEE_FP
*sf_addr = r;
#else
!! sf!
#endif
#line 10988 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
spTOS = sp[0];
fpTOS = fp[0];
LABEL2(s_f_store)
NEXT_P1_5;
LABEL3(s_f_store)
DO_GOTO;
}

LABEL(f_plus) /* f+ ( r1 r2 -- r3 ) S1 -- S1  */
/*  */
NAME("f+")
{
DEF_CA
MAYBE_UNUSED Float r1;
MAYBE_UNUSED Float r2;
Float r3;
NEXT_P0;
vm_Float2r(fp[1],r1);
vm_Float2r(fpTOS,r2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
fputs(" r2=", vm_out); printarg_r(r2);
}
#endif
fp += 1;
{
#line 2073 "prim"
r3 = r1+r2;
#line 11026 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r3=", vm_out); printarg_r(r3);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r3,fpTOS);
LABEL2(f_plus)
NEXT_P1_5;
LABEL3(f_plus)
DO_GOTO;
}

LABEL(f_minus) /* f- ( r1 r2 -- r3 ) S1 -- S1  */
/*  */
NAME("f-")
{
DEF_CA
MAYBE_UNUSED Float r1;
MAYBE_UNUSED Float r2;
Float r3;
NEXT_P0;
vm_Float2r(fp[1],r1);
vm_Float2r(fpTOS,r2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
fputs(" r2=", vm_out); printarg_r(r2);
}
#endif
fp += 1;
{
#line 2076 "prim"
r3 = r1-r2;
#line 11064 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r3=", vm_out); printarg_r(r3);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r3,fpTOS);
LABEL2(f_minus)
NEXT_P1_5;
LABEL3(f_minus)
DO_GOTO;
}

LABEL(f_star) /* f* ( r1 r2 -- r3 ) S1 -- S1  */
/*  */
NAME("f*")
{
DEF_CA
MAYBE_UNUSED Float r1;
MAYBE_UNUSED Float r2;
Float r3;
NEXT_P0;
vm_Float2r(fp[1],r1);
vm_Float2r(fpTOS,r2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
fputs(" r2=", vm_out); printarg_r(r2);
}
#endif
fp += 1;
{
#line 2079 "prim"
r3 = r1*r2;
#line 11102 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r3=", vm_out); printarg_r(r3);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r3,fpTOS);
LABEL2(f_star)
NEXT_P1_5;
LABEL3(f_star)
DO_GOTO;
}

LABEL(f_slash) /* f/ ( r1 r2 -- r3 ) S1 -- S1  */
/*  */
NAME("f/")
{
DEF_CA
MAYBE_UNUSED Float r1;
MAYBE_UNUSED Float r2;
Float r3;
NEXT_P0;
vm_Float2r(fp[1],r1);
vm_Float2r(fpTOS,r2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
fputs(" r2=", vm_out); printarg_r(r2);
}
#endif
fp += 1;
{
#line 2082 "prim"
r3 = r1/r2;
#line 11140 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r3=", vm_out); printarg_r(r3);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r3,fpTOS);
LABEL2(f_slash)
NEXT_P1_5;
LABEL3(f_slash)
DO_GOTO;
}

LABEL(f_star_star) /* f** ( r1 r2 -- r3 ) S1 -- S1  */
/* @i{r3} is @i{r1} raised to the @i{r2}th power. */
NAME("f**")
{
DEF_CA
MAYBE_UNUSED Float r1;
MAYBE_UNUSED Float r2;
Float r3;
NEXT_P0;
vm_Float2r(fp[1],r1);
vm_Float2r(fpTOS,r2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
fputs(" r2=", vm_out); printarg_r(r2);
}
#endif
fp += 1;
{
#line 2086 "prim"
CLOBBER_TOS_WORKAROUND_START;
r3 = pow(r1,r2);
CLOBBER_TOS_WORKAROUND_END;
#line 11180 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r3=", vm_out); printarg_r(r3);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r3,fpTOS);
LABEL2(f_star_star)
NEXT_P1_5;
LABEL3(f_star_star)
DO_GOTO;
}

LABEL(fm_star) /* fm* ( r1 n -- r2 ) S1 -- S1  */
/*  */
NAME("fm*")
{
DEF_CA
MAYBE_UNUSED Float r1;
MAYBE_UNUSED Cell n;
Float r2;
NEXT_P0;
vm_Float2r(fpTOS,r1);
vm_Cell2n(spTOS,n);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
fputs(" n=", vm_out); printarg_n(n);
}
#endif
sp += 1;
{
#line 2091 "prim"
r2 = r1*n;
#line 11218 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r2=", vm_out); printarg_r(r2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r2,fpTOS);
spTOS = sp[0];
LABEL2(fm_star)
NEXT_P1_5;
LABEL3(fm_star)
DO_GOTO;
}

LABEL(fm_slash) /* fm/ ( r1 n -- r2 ) S1 -- S1  */
/*  */
NAME("fm/")
{
DEF_CA
MAYBE_UNUSED Float r1;
MAYBE_UNUSED Cell n;
Float r2;
NEXT_P0;
vm_Float2r(fpTOS,r1);
vm_Cell2n(spTOS,n);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
fputs(" n=", vm_out); printarg_n(n);
}
#endif
sp += 1;
{
#line 2094 "prim"
r2 = r1/n;
#line 11257 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r2=", vm_out); printarg_r(r2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r2,fpTOS);
spTOS = sp[0];
LABEL2(fm_slash)
NEXT_P1_5;
LABEL3(fm_slash)
DO_GOTO;
}

LABEL(fm_star_slash) /* fmx/ ( r1 n1 n2 -- r2 ) S1 -- S1  */
/*  */
NAME("fm*/")
{
DEF_CA
MAYBE_UNUSED Float r1;
MAYBE_UNUSED Cell n1;
MAYBE_UNUSED Cell n2;
Float r2;
NEXT_P0;
vm_Float2r(fpTOS,r1);
vm_Cell2n(sp[1],n1);
vm_Cell2n(spTOS,n2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
fputs(" n1=", vm_out); printarg_n(n1);
fputs(" n2=", vm_out); printarg_n(n2);
}
#endif
sp += 2;
{
#line 2097 "prim"
r2 = (r1*n1)/n2;
#line 11299 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r2=", vm_out); printarg_r(r2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r2,fpTOS);
spTOS = sp[0];
LABEL2(fm_star_slash)
NEXT_P1_5;
LABEL3(fm_star_slash)
DO_GOTO;
}

LABEL(fm_square) /* f**2 ( r1 -- r2 ) S1 -- S1  */
/*  */
NAME("f**2")
{
DEF_CA
MAYBE_UNUSED Float r1;
Float r2;
NEXT_P0;
vm_Float2r(fpTOS,r1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
}
#endif
{
#line 2100 "prim"
r2 = r1*r1;
#line 11334 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r2=", vm_out); printarg_r(r2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r2,fpTOS);
LABEL2(fm_square)
NEXT_P1_5;
LABEL3(fm_square)
DO_GOTO;
}

LABEL(f_negate) /* fnegate ( r1 -- r2 ) S1 -- S1  */
/*  */
NAME("fnegate")
{
DEF_CA
MAYBE_UNUSED Float r1;
Float r2;
NEXT_P0;
vm_Float2r(fpTOS,r1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
}
#endif
{
#line 2103 "prim"
r2 = - r1;
#line 11368 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r2=", vm_out); printarg_r(r2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r2,fpTOS);
LABEL2(f_negate)
NEXT_P1_5;
LABEL3(f_negate)
DO_GOTO;
}

LABEL(f_drop) /* fdrop ( r -- ) S1 -- S1  */
/*  */
NAME("fdrop")
{
DEF_CA
MAYBE_UNUSED Float r;
NEXT_P0;
vm_Float2r(fpTOS,r);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r=", vm_out); printarg_r(r);
}
#endif
fp += 1;
{
#line 2106 "prim"
#line 11401 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
fpTOS = fp[0];
LABEL2(f_drop)
NEXT_P1_5;
LABEL3(f_drop)
DO_GOTO;
}

LABEL(f_dupe) /* fdup ( r -- r r ) S1 -- S1  */
/*  */
NAME("fdup")
{
DEF_CA
MAYBE_UNUSED Float r;
NEXT_P0;
vm_Float2r(fpTOS,r);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r=", vm_out); printarg_r(r);
}
#endif
fp += -1;
{
#line 2108 "prim"
#line 11433 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r,fp[1]);
vm_r2Float(r,fpTOS);
LABEL2(f_dupe)
NEXT_P1_5;
LABEL3(f_dupe)
DO_GOTO;
}

LABEL(f_swap) /* fswap ( r1 r2 -- r2 r1 ) S1 -- S1  */
/*  */
NAME("fswap")
{
DEF_CA
MAYBE_UNUSED Float r1;
MAYBE_UNUSED Float r2;
NEXT_P0;
vm_Float2r(fp[1],r1);
vm_Float2r(fpTOS,r2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
fputs(" r2=", vm_out); printarg_r(r2);
}
#endif
{
#line 2110 "prim"
#line 11468 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r2,fp[1]);
vm_r2Float(r1,fpTOS);
LABEL2(f_swap)
NEXT_P1_5;
LABEL3(f_swap)
DO_GOTO;
}

LABEL(f_over) /* fover ( r1 r2 -- r1 r2 r1 ) S1 -- S1  */
/*  */
NAME("fover")
{
DEF_CA
MAYBE_UNUSED Float r1;
MAYBE_UNUSED Float r2;
NEXT_P0;
vm_Float2r(fp[1],r1);
vm_Float2r(fpTOS,r2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
fputs(" r2=", vm_out); printarg_r(r2);
}
#endif
fp += -1;
{
#line 2112 "prim"
#line 11504 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r2,fp[1]);
vm_r2Float(r1,fpTOS);
LABEL2(f_over)
NEXT_P1_5;
LABEL3(f_over)
DO_GOTO;
}

LABEL(f_rote) /* frot ( r1 r2 r3 -- r2 r3 r1 ) S1 -- S1  */
/*  */
NAME("frot")
{
DEF_CA
MAYBE_UNUSED Float r1;
MAYBE_UNUSED Float r2;
MAYBE_UNUSED Float r3;
NEXT_P0;
vm_Float2r(fp[2],r1);
vm_Float2r(fp[1],r2);
vm_Float2r(fpTOS,r3);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
fputs(" r2=", vm_out); printarg_r(r2);
fputs(" r3=", vm_out); printarg_r(r3);
}
#endif
{
#line 2114 "prim"
#line 11542 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r2,fp[2]);
vm_r2Float(r3,fp[1]);
vm_r2Float(r1,fpTOS);
LABEL2(f_rote)
NEXT_P1_5;
LABEL3(f_rote)
DO_GOTO;
}

LABEL(f_nip) /* fnip ( r1 r2 -- r2 ) S1 -- S1  */
/*  */
NAME("fnip")
{
DEF_CA
MAYBE_UNUSED Float r1;
MAYBE_UNUSED Float r2;
NEXT_P0;
vm_Float2r(fp[1],r1);
vm_Float2r(fpTOS,r2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
fputs(" r2=", vm_out); printarg_r(r2);
}
#endif
fp += 1;
{
#line 2116 "prim"
#line 11579 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r2,fpTOS);
LABEL2(f_nip)
NEXT_P1_5;
LABEL3(f_nip)
DO_GOTO;
}

LABEL(f_tuck) /* ftuck ( r1 r2 -- r2 r1 r2 ) S1 -- S1  */
/*  */
NAME("ftuck")
{
DEF_CA
MAYBE_UNUSED Float r1;
MAYBE_UNUSED Float r2;
NEXT_P0;
vm_Float2r(fp[1],r1);
vm_Float2r(fpTOS,r2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
fputs(" r2=", vm_out); printarg_r(r2);
}
#endif
fp += -1;
{
#line 2118 "prim"
#line 11614 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r2,fp[2]);
vm_r2Float(r1,fp[1]);
vm_r2Float(r2,fpTOS);
LABEL2(f_tuck)
NEXT_P1_5;
LABEL3(f_tuck)
DO_GOTO;
}

LABEL(float_plus) /* float+ ( f_addr1 -- f_addr2 ) S1 -- S1  */
/* @code{1 floats +}. */
NAME("float+")
{
DEF_CA
MAYBE_UNUSED Float * f_addr1;
Float * f_addr2;
NEXT_P0;
vm_Cell2f_(spTOS,f_addr1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" f_addr1=", vm_out); printarg_f_(f_addr1);
}
#endif
{
#line 2121 "prim"
f_addr2 = f_addr1+1;
#line 11649 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f_addr2=", vm_out); printarg_f_(f_addr2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f_2Cell(f_addr2,spTOS);
LABEL2(float_plus)
NEXT_P1_5;
LABEL3(float_plus)
DO_GOTO;
}

LABEL(floats) /* floats ( n1 -- n2 ) S1 -- S1  */
/* @i{n2} is the number of address units of @i{n1} floats. */
NAME("floats")
{
DEF_CA
MAYBE_UNUSED Cell n1;
Cell n2;
NEXT_P0;
vm_Cell2n(spTOS,n1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n1=", vm_out); printarg_n(n1);
}
#endif
{
#line 2125 "prim"
n2 = n1*sizeof(Float);
#line 11683 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n2=", vm_out); printarg_n(n2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n2,spTOS);
LABEL2(floats)
NEXT_P1_5;
LABEL3(floats)
DO_GOTO;
}

LABEL(floor) /* floor ( r1 -- r2 ) S1 -- S1  */
/* Round towards the next smaller integral value, i.e., round toward negative infinity. */
NAME("floor")
{
DEF_CA
MAYBE_UNUSED Float r1;
Float r2;
NEXT_P0;
vm_Float2r(fpTOS,r1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
}
#endif
{
#line 2129 "prim"
/* !! unclear wording */
CLOBBER_TOS_WORKAROUND_START;
r2 = floor(r1);
CLOBBER_TOS_WORKAROUND_END;
#line 11720 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r2=", vm_out); printarg_r(r2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r2,fpTOS);
LABEL2(floor)
NEXT_P1_5;
LABEL3(floor)
DO_GOTO;
}

LABEL(f_round) /* fround ( r1 -- r2 ) S1 -- S1  */
/* Round to the nearest integral value. */
NAME("fround")
{
DEF_CA
MAYBE_UNUSED Float r1;
Float r2;
NEXT_P0;
vm_Float2r(fpTOS,r1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
}
#endif
{
#line 2136 "prim"
CLOBBER_TOS_WORKAROUND_START;
r2 = rint(r1);
CLOBBER_TOS_WORKAROUND_END;
#line 11756 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r2=", vm_out); printarg_r(r2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r2,fpTOS);
LABEL2(f_round)
NEXT_P1_5;
LABEL3(f_round)
DO_GOTO;
}

LABEL(f_max) /* fmax ( r1 r2 -- r3 ) S1 -- S1  */
/*  */
NAME("fmax")
{
DEF_CA
MAYBE_UNUSED Float r1;
MAYBE_UNUSED Float r2;
Float r3;
NEXT_P0;
vm_Float2r(fp[1],r1);
vm_Float2r(fpTOS,r2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
fputs(" r2=", vm_out); printarg_r(r2);
}
#endif
fp += 1;
{
#line 2141 "prim"
if (r1<r2)
  r3 = r2;
else
  r3 = r1;
#line 11797 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r3=", vm_out); printarg_r(r3);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r3,fpTOS);
LABEL2(f_max)
NEXT_P1_5;
LABEL3(f_max)
DO_GOTO;
}

LABEL(f_min) /* fmin ( r1 r2 -- r3 ) S1 -- S1  */
/*  */
NAME("fmin")
{
DEF_CA
MAYBE_UNUSED Float r1;
MAYBE_UNUSED Float r2;
Float r3;
NEXT_P0;
vm_Float2r(fp[1],r1);
vm_Float2r(fpTOS,r2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
fputs(" r2=", vm_out); printarg_r(r2);
}
#endif
fp += 1;
{
#line 2147 "prim"
if (r1<r2)
  r3 = r1;
else
  r3 = r2;
#line 11838 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r3=", vm_out); printarg_r(r3);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r3,fpTOS);
LABEL2(f_min)
NEXT_P1_5;
LABEL3(f_min)
DO_GOTO;
}

LABEL(represent) /* represent ( r c_addr u -- n f1 f2 ) S1 -- S1  */
/*  */
NAME("represent")
{
DEF_CA
MAYBE_UNUSED Float r;
MAYBE_UNUSED Char * c_addr;
MAYBE_UNUSED UCell u;
Cell n;
Bool f1;
Bool f2;
NEXT_P0;
vm_Float2r(fpTOS,r);
vm_Cell2c_(sp[1],c_addr);
vm_Cell2u(spTOS,u);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r=", vm_out); printarg_r(r);
fputs(" c_addr=", vm_out); printarg_c_(c_addr);
fputs(" u=", vm_out); printarg_u(u);
}
#endif
sp += -1;
fp += 1;
{
#line 2153 "prim"
char *sig;
size_t siglen;
int flag;
int decpt;
sig=ecvt(r, u, &decpt, &flag);
n=(r==0. ? 1 : decpt);
f1=FLAG(flag!=0);
f2=FLAG(isdigit((unsigned)(sig[0]))!=0);
siglen=strlen((char *)sig);
if (siglen>u) /* happens in glibc-2.1.3 if 999.. is rounded up */
  siglen=u;
if (!f2) /* workaround Cygwin trailing 0s for Inf and Nan */
  for (; sig[siglen-1]=='0'; siglen--);
    ;
memcpy(c_addr,sig,siglen);
memset(c_addr+siglen,f2?'0':' ',u-siglen);
#line 11897 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n=", vm_out); printarg_n(n);
fputs(" f1=", vm_out); printarg_f(f1);
fputs(" f2=", vm_out); printarg_f(f2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n,sp[2]);
vm_f2Cell(f1,sp[1]);
vm_f2Cell(f2,spTOS);
fpTOS = fp[0];
LABEL2(represent)
NEXT_P1_5;
LABEL3(represent)
DO_GOTO;
}

LABEL(to_float) /* >float ( c_addr u -- f:... flag ) S1 -- S1  */
/* Actual stack effect: ( c_addr u -- r t | f ).  Attempt to convert the
character string @i{c-addr u} to internal floating-point
representation. If the string represents a valid floating-point number
@i{r} is placed on the floating-point stack and @i{flag} is
true. Otherwise, @i{flag} is false. A string of blanks is a special
case and represents the floating-point number 0. */
NAME(">float")
{
DEF_CA
MAYBE_UNUSED Char * c_addr;
MAYBE_UNUSED UCell u;
Bool flag;
NEXT_P0;
fp[0] = fpTOS;
vm_Cell2c_(sp[1],c_addr);
vm_Cell2u(spTOS,u);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_addr=", vm_out); printarg_c_(c_addr);
fputs(" u=", vm_out); printarg_u(u);
}
#endif
sp += 1;
{
#line 2177 "prim"
Float r;
flag = to_float(c_addr, u, &r);
if (flag) {
  fp--;
  fp[0]=r;
}
#line 11951 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" flag=", vm_out); printarg_f(flag);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(flag,spTOS);
fpTOS = fp[0];
LABEL2(to_float)
NEXT_P1_5;
LABEL3(to_float)
DO_GOTO;
}

LABEL(f_abs) /* fabs ( r1 -- r2 ) S1 -- S1  */
/*  */
NAME("fabs")
{
DEF_CA
MAYBE_UNUSED Float r1;
Float r2;
NEXT_P0;
vm_Float2r(fpTOS,r1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
}
#endif
{
#line 2185 "prim"
r2 = fabs(r1);
#line 11986 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r2=", vm_out); printarg_r(r2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r2,fpTOS);
LABEL2(f_abs)
NEXT_P1_5;
LABEL3(f_abs)
DO_GOTO;
}

LABEL(f_a_cos) /* facos ( r1 -- r2 ) S1 -- S1  */
/*  */
NAME("facos")
{
DEF_CA
MAYBE_UNUSED Float r1;
Float r2;
NEXT_P0;
vm_Float2r(fpTOS,r1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
}
#endif
{
#line 2188 "prim"
CLOBBER_TOS_WORKAROUND_START;
r2 = acos(r1);
CLOBBER_TOS_WORKAROUND_END;
#line 12022 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r2=", vm_out); printarg_r(r2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r2,fpTOS);
LABEL2(f_a_cos)
NEXT_P1_5;
LABEL3(f_a_cos)
DO_GOTO;
}

LABEL(f_a_sine) /* fasin ( r1 -- r2 ) S1 -- S1  */
/*  */
NAME("fasin")
{
DEF_CA
MAYBE_UNUSED Float r1;
Float r2;
NEXT_P0;
vm_Float2r(fpTOS,r1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
}
#endif
{
#line 2193 "prim"
CLOBBER_TOS_WORKAROUND_START;
r2 = asin(r1);
CLOBBER_TOS_WORKAROUND_END;
#line 12058 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r2=", vm_out); printarg_r(r2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r2,fpTOS);
LABEL2(f_a_sine)
NEXT_P1_5;
LABEL3(f_a_sine)
DO_GOTO;
}

LABEL(f_a_tan) /* fatan ( r1 -- r2 ) S1 -- S1  */
/*  */
NAME("fatan")
{
DEF_CA
MAYBE_UNUSED Float r1;
Float r2;
NEXT_P0;
vm_Float2r(fpTOS,r1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
}
#endif
{
#line 2198 "prim"
CLOBBER_TOS_WORKAROUND_START;
r2 = atan(r1);
CLOBBER_TOS_WORKAROUND_END;
#line 12094 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r2=", vm_out); printarg_r(r2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r2,fpTOS);
LABEL2(f_a_tan)
NEXT_P1_5;
LABEL3(f_a_tan)
DO_GOTO;
}

LABEL(f_a_tan_two) /* fatan2 ( r1 r2 -- r3 ) S1 -- S1  */
/* @i{r1/r2}=tan(@i{r3}). ANS Forth does not require, but probably
intends this to be the inverse of @code{fsincos}. In gforth it is. */
NAME("fatan2")
{
DEF_CA
MAYBE_UNUSED Float r1;
MAYBE_UNUSED Float r2;
Float r3;
NEXT_P0;
vm_Float2r(fp[1],r1);
vm_Float2r(fpTOS,r2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
fputs(" r2=", vm_out); printarg_r(r2);
}
#endif
fp += 1;
{
#line 2205 "prim"
CLOBBER_TOS_WORKAROUND_START;
r3 = atan2(r1,r2);
CLOBBER_TOS_WORKAROUND_END;
#line 12135 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r3=", vm_out); printarg_r(r3);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r3,fpTOS);
LABEL2(f_a_tan_two)
NEXT_P1_5;
LABEL3(f_a_tan_two)
DO_GOTO;
}

LABEL(f_cos) /* fcos ( r1 -- r2 ) S1 -- S1  */
/*  */
NAME("fcos")
{
DEF_CA
MAYBE_UNUSED Float r1;
Float r2;
NEXT_P0;
vm_Float2r(fpTOS,r1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
}
#endif
{
#line 2210 "prim"
CLOBBER_TOS_WORKAROUND_START;
r2 = cos(r1);
CLOBBER_TOS_WORKAROUND_END;
#line 12171 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r2=", vm_out); printarg_r(r2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r2,fpTOS);
LABEL2(f_cos)
NEXT_P1_5;
LABEL3(f_cos)
DO_GOTO;
}

LABEL(f_e_x_p) /* fexp ( r1 -- r2 ) S1 -- S1  */
/*  */
NAME("fexp")
{
DEF_CA
MAYBE_UNUSED Float r1;
Float r2;
NEXT_P0;
vm_Float2r(fpTOS,r1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
}
#endif
{
#line 2215 "prim"
CLOBBER_TOS_WORKAROUND_START;
r2 = exp(r1);
CLOBBER_TOS_WORKAROUND_END;
#line 12207 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r2=", vm_out); printarg_r(r2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r2,fpTOS);
LABEL2(f_e_x_p)
NEXT_P1_5;
LABEL3(f_e_x_p)
DO_GOTO;
}

LABEL(f_e_x_p_m_one) /* fexpm1 ( r1 -- r2 ) S1 -- S1  */
/* @i{r2}=@i{e}**@i{r1}@minus{}1 */
NAME("fexpm1")
{
DEF_CA
MAYBE_UNUSED Float r1;
Float r2;
NEXT_P0;
vm_Float2r(fpTOS,r1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
}
#endif
{
#line 2221 "prim"
CLOBBER_TOS_WORKAROUND_START;
#ifdef HAVE_EXPM1
extern double
#ifdef NeXT
              const
#endif
                    expm1(double);
r2 = expm1(r1);
#else
r2 = exp(r1)-1.;
#endif
CLOBBER_TOS_WORKAROUND_END;
#line 12252 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r2=", vm_out); printarg_r(r2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r2,fpTOS);
LABEL2(f_e_x_p_m_one)
NEXT_P1_5;
LABEL3(f_e_x_p_m_one)
DO_GOTO;
}

LABEL(f_l_n) /* fln ( r1 -- r2 ) S1 -- S1  */
/*  */
NAME("fln")
{
DEF_CA
MAYBE_UNUSED Float r1;
Float r2;
NEXT_P0;
vm_Float2r(fpTOS,r1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
}
#endif
{
#line 2235 "prim"
CLOBBER_TOS_WORKAROUND_START;
r2 = log(r1);
CLOBBER_TOS_WORKAROUND_END;
#line 12288 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r2=", vm_out); printarg_r(r2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r2,fpTOS);
LABEL2(f_l_n)
NEXT_P1_5;
LABEL3(f_l_n)
DO_GOTO;
}

LABEL(f_l_n_p_one) /* flnp1 ( r1 -- r2 ) S1 -- S1  */
/* @i{r2}=ln(@i{r1}+1) */
NAME("flnp1")
{
DEF_CA
MAYBE_UNUSED Float r1;
Float r2;
NEXT_P0;
vm_Float2r(fpTOS,r1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
}
#endif
{
#line 2241 "prim"
CLOBBER_TOS_WORKAROUND_START;
#ifdef HAVE_LOG1P
extern double
#ifdef NeXT
              const
#endif
                    log1p(double);
r2 = log1p(r1);
#else
r2 = log(r1+1.);
#endif
CLOBBER_TOS_WORKAROUND_END;
#line 12333 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r2=", vm_out); printarg_r(r2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r2,fpTOS);
LABEL2(f_l_n_p_one)
NEXT_P1_5;
LABEL3(f_l_n_p_one)
DO_GOTO;
}

LABEL(f_log) /* flog ( r1 -- r2 ) S1 -- S1  */
/* The decimal logarithm. */
NAME("flog")
{
DEF_CA
MAYBE_UNUSED Float r1;
Float r2;
NEXT_P0;
vm_Float2r(fpTOS,r1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
}
#endif
{
#line 2256 "prim"
CLOBBER_TOS_WORKAROUND_START;
r2 = log10(r1);
CLOBBER_TOS_WORKAROUND_END;
#line 12369 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r2=", vm_out); printarg_r(r2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r2,fpTOS);
LABEL2(f_log)
NEXT_P1_5;
LABEL3(f_log)
DO_GOTO;
}

LABEL(f_a_log) /* falog ( r1 -- r2 ) S1 -- S1  */
/* @i{r2}=10**@i{r1} */
NAME("falog")
{
DEF_CA
MAYBE_UNUSED Float r1;
Float r2;
NEXT_P0;
vm_Float2r(fpTOS,r1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
}
#endif
{
#line 2262 "prim"
extern double pow10(double);
CLOBBER_TOS_WORKAROUND_START;
r2 = pow10(r1);
CLOBBER_TOS_WORKAROUND_END;
#line 12406 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r2=", vm_out); printarg_r(r2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r2,fpTOS);
LABEL2(f_a_log)
NEXT_P1_5;
LABEL3(f_a_log)
DO_GOTO;
}

LABEL(f_sine) /* fsin ( r1 -- r2 ) S1 -- S1  */
/*  */
NAME("fsin")
{
DEF_CA
MAYBE_UNUSED Float r1;
Float r2;
NEXT_P0;
vm_Float2r(fpTOS,r1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
}
#endif
{
#line 2268 "prim"
CLOBBER_TOS_WORKAROUND_START;
r2 = sin(r1);
#line 12441 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r2=", vm_out); printarg_r(r2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r2,fpTOS);
LABEL2(f_sine)
NEXT_P1_5;
LABEL3(f_sine)
DO_GOTO;
}

LABEL(f_sine_cos) /* fsincos ( r1 -- r2 r3 ) S1 -- S1  */
/* @i{r2}=sin(@i{r1}), @i{r3}=cos(@i{r1}) */
NAME("fsincos")
{
DEF_CA
MAYBE_UNUSED Float r1;
Float r2;
Float r3;
NEXT_P0;
vm_Float2r(fpTOS,r1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
}
#endif
fp += -1;
{
#line 2273 "prim"
CLOBBER_TOS_WORKAROUND_START;
r2 = sin(r1);
r3 = cos(r1);
CLOBBER_TOS_WORKAROUND_END;
#line 12480 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r2=", vm_out); printarg_r(r2);
fputs(" r3=", vm_out); printarg_r(r3);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r2,fp[1]);
vm_r2Float(r3,fpTOS);
LABEL2(f_sine_cos)
NEXT_P1_5;
LABEL3(f_sine_cos)
DO_GOTO;
}

LABEL(f_square_root) /* fsqrt ( r1 -- r2 ) S1 -- S1  */
/*  */
NAME("fsqrt")
{
DEF_CA
MAYBE_UNUSED Float r1;
Float r2;
NEXT_P0;
vm_Float2r(fpTOS,r1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
}
#endif
{
#line 2279 "prim"
CLOBBER_TOS_WORKAROUND_START;
r2 = sqrt(r1);
CLOBBER_TOS_WORKAROUND_END;
#line 12518 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r2=", vm_out); printarg_r(r2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r2,fpTOS);
LABEL2(f_square_root)
NEXT_P1_5;
LABEL3(f_square_root)
DO_GOTO;
}

LABEL(f_tan) /* ftan ( r1 -- r2 ) S1 -- S1  */
/*  */
NAME("ftan")
{
DEF_CA
MAYBE_UNUSED Float r1;
Float r2;
NEXT_P0;
vm_Float2r(fpTOS,r1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
}
#endif
{
#line 2284 "prim"
CLOBBER_TOS_WORKAROUND_START;
r2 = tan(r1);
CLOBBER_TOS_WORKAROUND_END;
#line 12554 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r2=", vm_out); printarg_r(r2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r2,fpTOS);
LABEL2(f_tan)
NEXT_P1_5;
LABEL3(f_tan)
DO_GOTO;
}

LABEL(f_cinch) /* fsinh ( r1 -- r2 ) S1 -- S1  */
/*  */
NAME("fsinh")
{
DEF_CA
MAYBE_UNUSED Float r1;
Float r2;
NEXT_P0;
vm_Float2r(fpTOS,r1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
}
#endif
{
#line 2291 "prim"
CLOBBER_TOS_WORKAROUND_START;
r2 = sinh(r1);
CLOBBER_TOS_WORKAROUND_END;
#line 12590 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r2=", vm_out); printarg_r(r2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r2,fpTOS);
LABEL2(f_cinch)
NEXT_P1_5;
LABEL3(f_cinch)
DO_GOTO;
}

LABEL(f_cosh) /* fcosh ( r1 -- r2 ) S1 -- S1  */
/*  */
NAME("fcosh")
{
DEF_CA
MAYBE_UNUSED Float r1;
Float r2;
NEXT_P0;
vm_Float2r(fpTOS,r1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
}
#endif
{
#line 2298 "prim"
CLOBBER_TOS_WORKAROUND_START;
r2 = cosh(r1);
CLOBBER_TOS_WORKAROUND_END;
#line 12626 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r2=", vm_out); printarg_r(r2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r2,fpTOS);
LABEL2(f_cosh)
NEXT_P1_5;
LABEL3(f_cosh)
DO_GOTO;
}

LABEL(f_tan_h) /* ftanh ( r1 -- r2 ) S1 -- S1  */
/*  */
NAME("ftanh")
{
DEF_CA
MAYBE_UNUSED Float r1;
Float r2;
NEXT_P0;
vm_Float2r(fpTOS,r1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
}
#endif
{
#line 2305 "prim"
CLOBBER_TOS_WORKAROUND_START;
r2 = tanh(r1);
CLOBBER_TOS_WORKAROUND_END;
#line 12662 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r2=", vm_out); printarg_r(r2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r2,fpTOS);
LABEL2(f_tan_h)
NEXT_P1_5;
LABEL3(f_tan_h)
DO_GOTO;
}

LABEL(f_a_cinch) /* fasinh ( r1 -- r2 ) S1 -- S1  */
/*  */
NAME("fasinh")
{
DEF_CA
MAYBE_UNUSED Float r1;
Float r2;
NEXT_P0;
vm_Float2r(fpTOS,r1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
}
#endif
{
#line 2312 "prim"
CLOBBER_TOS_WORKAROUND_START;
r2 = asinh(r1);
CLOBBER_TOS_WORKAROUND_END;
#line 12698 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r2=", vm_out); printarg_r(r2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r2,fpTOS);
LABEL2(f_a_cinch)
NEXT_P1_5;
LABEL3(f_a_cinch)
DO_GOTO;
}

LABEL(f_a_cosh) /* facosh ( r1 -- r2 ) S1 -- S1  */
/*  */
NAME("facosh")
{
DEF_CA
MAYBE_UNUSED Float r1;
Float r2;
NEXT_P0;
vm_Float2r(fpTOS,r1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
}
#endif
{
#line 2319 "prim"
CLOBBER_TOS_WORKAROUND_START;
r2 = acosh(r1);
CLOBBER_TOS_WORKAROUND_END;
#line 12734 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r2=", vm_out); printarg_r(r2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r2,fpTOS);
LABEL2(f_a_cosh)
NEXT_P1_5;
LABEL3(f_a_cosh)
DO_GOTO;
}

LABEL(f_a_tan_h) /* fatanh ( r1 -- r2 ) S1 -- S1  */
/*  */
NAME("fatanh")
{
DEF_CA
MAYBE_UNUSED Float r1;
Float r2;
NEXT_P0;
vm_Float2r(fpTOS,r1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r1=", vm_out); printarg_r(r1);
}
#endif
{
#line 2326 "prim"
CLOBBER_TOS_WORKAROUND_START;
r2 = atanh(r1);
CLOBBER_TOS_WORKAROUND_END;
#line 12770 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r2=", vm_out); printarg_r(r2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r2,fpTOS);
LABEL2(f_a_tan_h)
NEXT_P1_5;
LABEL3(f_a_tan_h)
DO_GOTO;
}

LABEL(s_floats) /* sfloats ( n1 -- n2 ) S1 -- S1  */
/* @i{n2} is the number of address units of @i{n1}
single-precision IEEE floating-point numbers. */
NAME("sfloats")
{
DEF_CA
MAYBE_UNUSED Cell n1;
Cell n2;
NEXT_P0;
vm_Cell2n(spTOS,n1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n1=", vm_out); printarg_n(n1);
}
#endif
{
#line 2336 "prim"
n2 = n1*sizeof(SFloat);
#line 12805 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n2=", vm_out); printarg_n(n2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n2,spTOS);
LABEL2(s_floats)
NEXT_P1_5;
LABEL3(s_floats)
DO_GOTO;
}

LABEL(d_floats) /* dfloats ( n1 -- n2 ) S1 -- S1  */
/* @i{n2} is the number of address units of @i{n1}
double-precision IEEE floating-point numbers. */
NAME("dfloats")
{
DEF_CA
MAYBE_UNUSED Cell n1;
Cell n2;
NEXT_P0;
vm_Cell2n(spTOS,n1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n1=", vm_out); printarg_n(n1);
}
#endif
{
#line 2341 "prim"
n2 = n1*sizeof(DFloat);
#line 12840 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n2=", vm_out); printarg_n(n2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n2,spTOS);
LABEL2(d_floats)
NEXT_P1_5;
LABEL3(d_floats)
DO_GOTO;
}

LABEL(s_f_aligned) /* sfaligned ( c_addr -- sf_addr ) S1 -- S1  */
/* @i{sf-addr} is the first single-float-aligned address greater
than or equal to @i{c-addr}. */
NAME("sfaligned")
{
DEF_CA
MAYBE_UNUSED Char * c_addr;
SFloat * sf_addr;
NEXT_P0;
vm_Cell2c_(spTOS,c_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_addr=", vm_out); printarg_c_(c_addr);
}
#endif
{
#line 2346 "prim"
sf_addr = (SFloat *)((((Cell)c_addr)+(sizeof(SFloat)-1))&(-sizeof(SFloat)));
#line 12875 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" sf_addr=", vm_out); printarg_sf_(sf_addr);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_sf_2Cell(sf_addr,spTOS);
LABEL2(s_f_aligned)
NEXT_P1_5;
LABEL3(s_f_aligned)
DO_GOTO;
}

LABEL(d_f_aligned) /* dfaligned ( c_addr -- df_addr ) S1 -- S1  */
/* @i{df-addr} is the first double-float-aligned address greater
than or equal to @i{c-addr}. */
NAME("dfaligned")
{
DEF_CA
MAYBE_UNUSED Char * c_addr;
DFloat * df_addr;
NEXT_P0;
vm_Cell2c_(spTOS,c_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_addr=", vm_out); printarg_c_(c_addr);
}
#endif
{
#line 2353 "prim"
df_addr = (DFloat *)((((Cell)c_addr)+(sizeof(DFloat)-1))&(-sizeof(DFloat)));
#line 12910 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" df_addr=", vm_out); printarg_df_(df_addr);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_df_2Cell(df_addr,spTOS);
LABEL2(d_f_aligned)
NEXT_P1_5;
LABEL3(d_f_aligned)
DO_GOTO;
}

LABEL(v_star) /* v* ( f_addr1 nstride1 f_addr2 nstride2 ucount -- r ) S1 -- S1  */
/* dot-product: r=v1*v2.  The first element of v1 is at f_addr1, the
next at f_addr1+nstride1 and so on (similar for v2). Both vectors have
ucount elements. */
NAME("v*")
{
DEF_CA
MAYBE_UNUSED Float * f_addr1;
MAYBE_UNUSED Cell nstride1;
MAYBE_UNUSED Float * f_addr2;
MAYBE_UNUSED Cell nstride2;
MAYBE_UNUSED UCell ucount;
Float r;
NEXT_P0;
fp[0] = fpTOS;
vm_Cell2f_(sp[4],f_addr1);
vm_Cell2n(sp[3],nstride1);
vm_Cell2f_(sp[2],f_addr2);
vm_Cell2n(sp[1],nstride2);
vm_Cell2u(spTOS,ucount);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" f_addr1=", vm_out); printarg_f_(f_addr1);
fputs(" nstride1=", vm_out); printarg_n(nstride1);
fputs(" f_addr2=", vm_out); printarg_f_(f_addr2);
fputs(" nstride2=", vm_out); printarg_n(nstride2);
fputs(" ucount=", vm_out); printarg_u(ucount);
}
#endif
sp += 5;
fp += -1;
{
#line 2361 "prim"
r = v_star(f_addr1, nstride1, f_addr2, nstride2, ucount);
#line 12961 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r=", vm_out); printarg_r(r);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r,fpTOS);
spTOS = sp[0];
LABEL2(v_star)
NEXT_P1_5;
LABEL3(v_star)
DO_GOTO;
}

LABEL(faxpy) /* faxpy ( ra f_x nstridex f_y nstridey ucount -- ) S1 -- S1  */
/* vy=ra*vx+vy */
NAME("faxpy")
{
DEF_CA
MAYBE_UNUSED Float ra;
MAYBE_UNUSED Float * f_x;
MAYBE_UNUSED Cell nstridex;
MAYBE_UNUSED Float * f_y;
MAYBE_UNUSED Cell nstridey;
MAYBE_UNUSED UCell ucount;
NEXT_P0;
vm_Float2r(fpTOS,ra);
vm_Cell2f_(sp[4],f_x);
vm_Cell2n(sp[3],nstridex);
vm_Cell2f_(sp[2],f_y);
vm_Cell2n(sp[1],nstridey);
vm_Cell2u(spTOS,ucount);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" ra=", vm_out); printarg_r(ra);
fputs(" f_x=", vm_out); printarg_f_(f_x);
fputs(" nstridex=", vm_out); printarg_n(nstridex);
fputs(" f_y=", vm_out); printarg_f_(f_y);
fputs(" nstridey=", vm_out); printarg_n(nstridey);
fputs(" ucount=", vm_out); printarg_u(ucount);
}
#endif
sp += 5;
fp += 1;
{
#line 2369 "prim"
faxpy(ra, f_x, nstridex, f_y, nstridey, ucount);
#line 13012 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
spTOS = sp[0];
fpTOS = fp[0];
LABEL2(faxpy)
NEXT_P1_5;
LABEL3(faxpy)
DO_GOTO;
}

GROUPADD(75)
#endif
GROUPADD(0)
#ifdef HAS_GLOCALS
GROUPADD(0)
GROUP( locals, 301)
LABEL(fetch_local_number) /* @local# ( #noffset -- w ) S1 -- S1  */
/*  */
NAME("@local#")
{
DEF_CA
MAYBE_UNUSED Cell noffset;
Cell w;
NEXT_P0;
sp[0] = spTOS;
vm_Cell2n(IMM_ARG(IPTOS,305397795 ),noffset);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" noffset=", vm_out); printarg_n(noffset);
}
#endif
INC_IP(1);
sp += -1;
{
#line 2389 "prim"
w = *(Cell *)(lp+noffset);
#line 13055 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" w=", vm_out); printarg_w(w);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w,spTOS);
LABEL2(fetch_local_number)
NEXT_P1_5;
LABEL3(fetch_local_number)
DO_GOTO;
}

LABEL(fetch_local_zero) /* @local0 ( -- w ) S1 -- S1  */
/*  */
NAME("@local0")
{
DEF_CA
Cell w;
NEXT_P0;
sp[0] = spTOS;
#ifdef VM_DEBUG
if (vm_debug) {
}
#endif
sp += -1;
{
#line 2392 "prim"
w = ((Cell *)lp)[0];
#line 13088 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" w=", vm_out); printarg_w(w);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w,spTOS);
LABEL2(fetch_local_zero)
NEXT_P1_5;
LABEL3(fetch_local_zero)
DO_GOTO;
}

LABEL(fetch_local_four) /* @local1 ( -- w ) S1 -- S1  */
/*  */
NAME("@local1")
{
DEF_CA
Cell w;
NEXT_P0;
sp[0] = spTOS;
#ifdef VM_DEBUG
if (vm_debug) {
}
#endif
sp += -1;
{
#line 2395 "prim"
w = ((Cell *)lp)[1];
#line 13121 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" w=", vm_out); printarg_w(w);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w,spTOS);
LABEL2(fetch_local_four)
NEXT_P1_5;
LABEL3(fetch_local_four)
DO_GOTO;
}

LABEL(fetch_local_eight) /* @local2 ( -- w ) S1 -- S1  */
/*  */
NAME("@local2")
{
DEF_CA
Cell w;
NEXT_P0;
sp[0] = spTOS;
#ifdef VM_DEBUG
if (vm_debug) {
}
#endif
sp += -1;
{
#line 2398 "prim"
w = ((Cell *)lp)[2];
#line 13154 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" w=", vm_out); printarg_w(w);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w,spTOS);
LABEL2(fetch_local_eight)
NEXT_P1_5;
LABEL3(fetch_local_eight)
DO_GOTO;
}

LABEL(fetch_local_twelve) /* @local3 ( -- w ) S1 -- S1  */
/*  */
NAME("@local3")
{
DEF_CA
Cell w;
NEXT_P0;
sp[0] = spTOS;
#ifdef VM_DEBUG
if (vm_debug) {
}
#endif
sp += -1;
{
#line 2401 "prim"
w = ((Cell *)lp)[3];
#line 13187 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" w=", vm_out); printarg_w(w);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w,spTOS);
LABEL2(fetch_local_twelve)
NEXT_P1_5;
LABEL3(fetch_local_twelve)
DO_GOTO;
}

GROUPADD(5)
#ifdef HAS_FLOATING
LABEL(f_fetch_local_number) /* f@local# ( #noffset -- r ) S1 -- S1  */
/*  */
NAME("f@local#")
{
DEF_CA
MAYBE_UNUSED Cell noffset;
Float r;
NEXT_P0;
fp[0] = fpTOS;
vm_Cell2n(IMM_ARG(IPTOS,305397796 ),noffset);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" noffset=", vm_out); printarg_n(noffset);
}
#endif
INC_IP(1);
fp += -1;
{
#line 2406 "prim"
r = *(Float *)(lp+noffset);
#line 13226 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r=", vm_out); printarg_r(r);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r,fpTOS);
LABEL2(f_fetch_local_number)
NEXT_P1_5;
LABEL3(f_fetch_local_number)
DO_GOTO;
}

LABEL(f_fetch_local_zero) /* f@local0 ( -- r ) S1 -- S1  */
/*  */
NAME("f@local0")
{
DEF_CA
Float r;
NEXT_P0;
fp[0] = fpTOS;
#ifdef VM_DEBUG
if (vm_debug) {
}
#endif
fp += -1;
{
#line 2409 "prim"
r = ((Float *)lp)[0];
#line 13259 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r=", vm_out); printarg_r(r);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r,fpTOS);
LABEL2(f_fetch_local_zero)
NEXT_P1_5;
LABEL3(f_fetch_local_zero)
DO_GOTO;
}

LABEL(f_fetch_local_eight) /* f@local1 ( -- r ) S1 -- S1  */
/*  */
NAME("f@local1")
{
DEF_CA
Float r;
NEXT_P0;
fp[0] = fpTOS;
#ifdef VM_DEBUG
if (vm_debug) {
}
#endif
fp += -1;
{
#line 2412 "prim"
r = ((Float *)lp)[1];
#line 13292 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r=", vm_out); printarg_r(r);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_r2Float(r,fpTOS);
LABEL2(f_fetch_local_eight)
NEXT_P1_5;
LABEL3(f_fetch_local_eight)
DO_GOTO;
}

GROUPADD(3)
#endif
LABEL(laddr_number) /* laddr# ( #noffset -- c_addr ) S1 -- S1  */
/*  */
NAME("laddr#")
{
DEF_CA
MAYBE_UNUSED Cell noffset;
Char * c_addr;
NEXT_P0;
sp[0] = spTOS;
vm_Cell2n(IMM_ARG(IPTOS,305397797 ),noffset);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" noffset=", vm_out); printarg_n(noffset);
}
#endif
INC_IP(1);
sp += -1;
{
#line 2417 "prim"
/* this can also be used to implement lp@ */
c_addr = (Char *)(lp+noffset);
#line 13332 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" c_addr=", vm_out); printarg_c_(c_addr);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_c_2Cell(c_addr,spTOS);
LABEL2(laddr_number)
NEXT_P1_5;
LABEL3(laddr_number)
DO_GOTO;
}

LABEL(lp_plus_store_number) /* lp+!# ( #noffset -- ) S1 -- S1  */
/* used with negative immediate values it allocates memory on the
local stack, a positive immediate argument drops memory from the local
stack */
NAME("lp+!#")
{
DEF_CA
MAYBE_UNUSED Cell noffset;
NEXT_P0;
vm_Cell2n(IMM_ARG(IPTOS,305397798 ),noffset);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" noffset=", vm_out); printarg_n(noffset);
}
#endif
INC_IP(1);
{
#line 2424 "prim"
lp += noffset;
#line 13368 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
LABEL2(lp_plus_store_number)
NEXT_P1_5;
LABEL3(lp_plus_store_number)
DO_GOTO;
}

LABEL(minus_four_lp_plus_store) /* lp- ( -- ) S1 -- S1  */
/*  */
NAME("lp-")
{
DEF_CA
NEXT_P0;
#ifdef VM_DEBUG
if (vm_debug) {
}
#endif
{
#line 2427 "prim"
lp += -sizeof(Cell);
#line 13396 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
LABEL2(minus_four_lp_plus_store)
NEXT_P1_5;
LABEL3(minus_four_lp_plus_store)
DO_GOTO;
}

LABEL(eight_lp_plus_store) /* lp+ ( -- ) S1 -- S1  */
/*  */
NAME("lp+")
{
DEF_CA
NEXT_P0;
#ifdef VM_DEBUG
if (vm_debug) {
}
#endif
{
#line 2430 "prim"
lp += sizeof(Float);
#line 13424 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
LABEL2(eight_lp_plus_store)
NEXT_P1_5;
LABEL3(eight_lp_plus_store)
DO_GOTO;
}

LABEL(sixteen_lp_plus_store) /* lp+2 ( -- ) S1 -- S1  */
/*  */
NAME("lp+2")
{
DEF_CA
NEXT_P0;
#ifdef VM_DEBUG
if (vm_debug) {
}
#endif
{
#line 2433 "prim"
lp += 2*sizeof(Float);
#line 13452 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
LABEL2(sixteen_lp_plus_store)
NEXT_P1_5;
LABEL3(sixteen_lp_plus_store)
DO_GOTO;
}

LABEL(lp_store) /* lp! ( c_addr -- ) S1 -- S1  */
/*  */
NAME("lp!")
{
DEF_CA
MAYBE_UNUSED Char * c_addr;
NEXT_P0;
vm_Cell2c_(spTOS,c_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_addr=", vm_out); printarg_c_(c_addr);
}
#endif
sp += 1;
{
#line 2436 "prim"
lp = (Address)c_addr;
#line 13484 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
spTOS = sp[0];
LABEL2(lp_store)
NEXT_P1_5;
LABEL3(lp_store)
DO_GOTO;
}

LABEL(to_l) /* >l ( w -- ) S1 -- S1  */
/*  */
NAME(">l")
{
DEF_CA
MAYBE_UNUSED Cell w;
NEXT_P0;
vm_Cell2w(spTOS,w);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w=", vm_out); printarg_w(w);
}
#endif
sp += 1;
{
#line 2439 "prim"
lp -= sizeof(Cell);
*(Cell *)lp = w;
#line 13518 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
spTOS = sp[0];
LABEL2(to_l)
NEXT_P1_5;
LABEL3(to_l)
DO_GOTO;
}

GROUPADD(7)
#ifdef HAS_FLOATING
LABEL(f_to_l) /* f>l ( r -- ) S1 -- S1  */
/*  */
NAME("f>l")
{
DEF_CA
MAYBE_UNUSED Float r;
NEXT_P0;
vm_Float2r(fpTOS,r);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" r=", vm_out); printarg_r(r);
}
#endif
fp += 1;
{
#line 2445 "prim"
lp -= sizeof(Float);
*(Float *)lp = r;
#line 13554 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
fpTOS = fp[0];
LABEL2(f_to_l)
NEXT_P1_5;
LABEL3(f_to_l)
DO_GOTO;
}

LABEL(fpick) /* fpick ( f:... u -- f:... r ) S1 -- S1  */
/* Actually the stack effect is @code{ r0 ... ru u -- r0 ... ru r0 }. */
NAME("fpick")
{
DEF_CA
MAYBE_UNUSED UCell u;
Float r;
NEXT_P0;
fp[0] = fpTOS;
vm_Cell2u(spTOS,u);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" u=", vm_out); printarg_u(u);
}
#endif
sp += 1;
{
#line 2450 "prim"
r = fp[u];
#line 13589 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" r=", vm_out); printarg_r(r);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
fp += -1;
vm_r2Float(r,fpTOS);
spTOS = sp[0];
LABEL2(fpick)
NEXT_P1_5;
LABEL3(fpick)
DO_GOTO;
}

GROUPADD(2)
#endif
GROUPADD(0)
#endif
GROUPADD(0)
#ifdef HAS_OS
GROUPADD(0)
GROUP( syslib, 318)
LABEL(open_lib) /* open-lib ( c_addr1 u1 -- u2 ) S1 -- S1  */
/*  */
NAME("open-lib")
{
DEF_CA
MAYBE_UNUSED Char * c_addr1;
MAYBE_UNUSED UCell u1;
UCell u2;
NEXT_P0;
vm_Cell2c_(sp[1],c_addr1);
vm_Cell2u(spTOS,u1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_addr1=", vm_out); printarg_c_(c_addr1);
fputs(" u1=", vm_out); printarg_u(u1);
}
#endif
sp += 1;
{
#line 2462 "prim"
u2 = gforth_dlopen(c_addr1, u1);
#line 13637 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" u2=", vm_out); printarg_u(u2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_u2Cell(u2,spTOS);
LABEL2(open_lib)
NEXT_P1_5;
LABEL3(open_lib)
DO_GOTO;
}

LABEL(lib_sym) /* lib-sym ( c_addr1 u1 u2 -- u3 ) S1 -- S1  */
/*  */
NAME("lib-sym")
{
DEF_CA
MAYBE_UNUSED Char * c_addr1;
MAYBE_UNUSED UCell u1;
MAYBE_UNUSED UCell u2;
UCell u3;
NEXT_P0;
vm_Cell2c_(sp[2],c_addr1);
vm_Cell2u(sp[1],u1);
vm_Cell2u(spTOS,u2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_addr1=", vm_out); printarg_c_(c_addr1);
fputs(" u1=", vm_out); printarg_u(u1);
fputs(" u2=", vm_out); printarg_u(u2);
}
#endif
sp += 2;
{
#line 2465 "prim"
#ifdef HAVE_LIBLTDL
u3 = (UCell) lt_dlsym((lt_dlhandle)u2, cstr(c_addr1, u1, 1));
#elif defined(HAVE_LIBDL) || defined(HAVE_DLOPEN)
u3 = (UCell) dlsym((void*)u2,cstr(c_addr1, u1, 1));
#else
#  ifdef _WIN32
u3 = (Cell) GetProcAddress((HMODULE)u2, cstr(c_addr1, u1, 1));
#  else
#warning Define lib-sym!
u3 = 0;
#  endif
#endif
#line 13689 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" u3=", vm_out); printarg_u(u3);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_u2Cell(u3,spTOS);
LABEL2(lib_sym)
NEXT_P1_5;
LABEL3(lib_sym)
DO_GOTO;
}

LABEL(wcall) /* wcall ( ... u -- ... ) S1 -- S1  */
/*  */
NAME("wcall")
{
DEF_CA
MAYBE_UNUSED UCell u;
NEXT_P0;
fp[0] = fpTOS;
vm_Cell2u(spTOS,u);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" u=", vm_out); printarg_u(u);
}
#endif
sp += 1;
{
#line 2479 "prim"
gforth_FP=fp;
sp=(Cell*)(SYSCALL(Cell*(*)(Cell *, void *))u)(sp, &gforth_FP);
fp=gforth_FP;
#line 13726 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
spTOS = sp[0];
fpTOS = fp[0];
LABEL2(wcall)
NEXT_P1_5;
LABEL3(wcall)
DO_GOTO;
}

LABEL(u_w_fetch) /* uw@ ( c_addr -- u ) S1 -- S1  */
/* @i{u} is the zero-extended 16-bit value stored at @i{c_addr}. */
NAME("uw@")
{
DEF_CA
MAYBE_UNUSED Char * c_addr;
UCell u;
NEXT_P0;
vm_Cell2c_(spTOS,c_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_addr=", vm_out); printarg_c_(c_addr);
}
#endif
{
#line 2485 "prim"
u = *(UWyde*)(c_addr);
#line 13760 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" u=", vm_out); printarg_u(u);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_u2Cell(u,spTOS);
LABEL2(u_w_fetch)
NEXT_P1_5;
LABEL3(u_w_fetch)
DO_GOTO;
}

LABEL(s_w_fetch) /* sw@ ( c_addr -- n ) S1 -- S1  */
/* @i{n} is the sign-extended 16-bit value stored at @i{c_addr}. */
NAME("sw@")
{
DEF_CA
MAYBE_UNUSED Char * c_addr;
Cell n;
NEXT_P0;
vm_Cell2c_(spTOS,c_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_addr=", vm_out); printarg_c_(c_addr);
}
#endif
{
#line 2489 "prim"
n = *(Wyde*)(c_addr);
#line 13794 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n=", vm_out); printarg_n(n);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n,spTOS);
LABEL2(s_w_fetch)
NEXT_P1_5;
LABEL3(s_w_fetch)
DO_GOTO;
}

LABEL(w_store) /* w! ( w c_addr -- ) S1 -- S1  */
/* Store the bottom 16 bits of @i{w} at @i{c_addr}. */
NAME("w!")
{
DEF_CA
MAYBE_UNUSED Cell w;
MAYBE_UNUSED Char * c_addr;
NEXT_P0;
vm_Cell2w(sp[1],w);
vm_Cell2c_(spTOS,c_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w=", vm_out); printarg_w(w);
fputs(" c_addr=", vm_out); printarg_c_(c_addr);
}
#endif
sp += 2;
{
#line 2493 "prim"
*(Wyde*)(c_addr) = w;
#line 13831 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
spTOS = sp[0];
LABEL2(w_store)
NEXT_P1_5;
LABEL3(w_store)
DO_GOTO;
}

LABEL(u_l_fetch) /* ul@ ( c_addr -- u ) S1 -- S1  */
/* @i{u} is the zero-extended 32-bit value stored at @i{c_addr}. */
NAME("ul@")
{
DEF_CA
MAYBE_UNUSED Char * c_addr;
UCell u;
NEXT_P0;
vm_Cell2c_(spTOS,c_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_addr=", vm_out); printarg_c_(c_addr);
}
#endif
{
#line 2497 "prim"
u = *(UTetrabyte*)(c_addr);
#line 13864 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" u=", vm_out); printarg_u(u);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_u2Cell(u,spTOS);
LABEL2(u_l_fetch)
NEXT_P1_5;
LABEL3(u_l_fetch)
DO_GOTO;
}

LABEL(s_l_fetch) /* sl@ ( c_addr -- n ) S1 -- S1  */
/* @i{n} is the sign-extended 32-bit value stored at @i{c_addr}. */
NAME("sl@")
{
DEF_CA
MAYBE_UNUSED Char * c_addr;
Cell n;
NEXT_P0;
vm_Cell2c_(spTOS,c_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_addr=", vm_out); printarg_c_(c_addr);
}
#endif
{
#line 2501 "prim"
n = *(Tetrabyte*)(c_addr);
#line 13898 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n=", vm_out); printarg_n(n);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n,spTOS);
LABEL2(s_l_fetch)
NEXT_P1_5;
LABEL3(s_l_fetch)
DO_GOTO;
}

LABEL(l_store) /* l! ( w c_addr -- ) S1 -- S1  */
/* Store the bottom 32 bits of @i{w} at @i{c_addr}. */
NAME("l!")
{
DEF_CA
MAYBE_UNUSED Cell w;
MAYBE_UNUSED Char * c_addr;
NEXT_P0;
vm_Cell2w(sp[1],w);
vm_Cell2c_(spTOS,c_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w=", vm_out); printarg_w(w);
fputs(" c_addr=", vm_out); printarg_c_(c_addr);
}
#endif
sp += 2;
{
#line 2505 "prim"
*(Tetrabyte*)(c_addr) = w;
#line 13935 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
spTOS = sp[0];
LABEL2(l_store)
NEXT_P1_5;
LABEL3(l_store)
DO_GOTO;
}

LABEL(lib_error) /* lib-error ( -- c_addr u ) S1 -- S1  */
/* Error message for last failed @code{open-lib} or @code{lib-sym}. */
NAME("lib-error")
{
DEF_CA
Char * c_addr;
UCell u;
NEXT_P0;
sp[0] = spTOS;
#ifdef VM_DEBUG
if (vm_debug) {
}
#endif
sp += -2;
{
#line 2509 "prim"
#ifdef HAVE_LIBLTDL
c_addr = (Char *)lt_dlerror();
u = (c_addr == NULL) ? 0 : strlen((char *)c_addr);
#else
c_addr = "libltdl is not configured";
u = strlen(c_addr);
#endif
#line 13974 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" c_addr=", vm_out); printarg_c_(c_addr);
fputs(" u=", vm_out); printarg_u(u);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_c_2Cell(c_addr,sp[1]);
vm_u2Cell(u,spTOS);
LABEL2(lib_error)
NEXT_P1_5;
LABEL3(lib_error)
DO_GOTO;
}

GROUPADD(10)
#endif
GROUPADD(0)
GROUP( peephole, 328)
GROUPADD(0)
#ifdef HAS_PEEPHOLE
LABEL(compile_prim1) /* compile-prim1 ( a_prim -- ) S1 -- S1  */
/* compile prim (incl. immargs) at @var{a_prim} */
NAME("compile-prim1")
{
DEF_CA
MAYBE_UNUSED Cell * a_prim;
NEXT_P0;
vm_Cell2a_(spTOS,a_prim);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_prim=", vm_out); printarg_a_(a_prim);
}
#endif
sp += 1;
{
#line 2524 "prim"
compile_prim1(a_prim);
#line 14016 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
spTOS = sp[0];
LABEL2(compile_prim1)
NEXT_P1_5;
LABEL3(compile_prim1)
DO_GOTO;
}

LABEL(finish_code) /* finish-code ( ... -- ... ) S1 -- S1  */
/* Perform delayed steps in code generation (branch resolution, I-cache
flushing). */
NAME("finish-code")
{
DEF_CA
NEXT_P0;
sp[0] = spTOS;
fp[0] = fpTOS;
#ifdef VM_DEBUG
if (vm_debug) {
}
#endif
{
#line 2529 "prim"
/* The ... above are a workaround for a bug in gcc-2.95, which fails
   to save spTOS (gforth-fast --enable-force-reg) */
finish_code();
#line 14050 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
spTOS = sp[0];
fpTOS = fp[0];
LABEL2(finish_code)
NEXT_P1_5;
LABEL3(finish_code)
DO_GOTO;
}

LABEL(forget_dyncode) /* forget-dyncode ( c_code -- f ) S1 -- S1  */
/*  */
NAME("forget-dyncode")
{
DEF_CA
MAYBE_UNUSED Char * c_code;
Bool f;
NEXT_P0;
vm_Cell2c_(spTOS,c_code);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" c_code=", vm_out); printarg_c_(c_code);
}
#endif
{
#line 2534 "prim"
f = forget_dyncode(c_code);
#line 14084 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" f=", vm_out); printarg_f(f);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_f2Cell(f,spTOS);
LABEL2(forget_dyncode)
NEXT_P1_5;
LABEL3(forget_dyncode)
DO_GOTO;
}

LABEL(decompile_prim) /* decompile-prim ( a_code -- a_prim ) S1 -- S1  */
/* a_prim is the code address of the primitive that has been
compile_prim1ed to a_code */
NAME("decompile-prim")
{
DEF_CA
MAYBE_UNUSED Cell * a_code;
Cell * a_prim;
NEXT_P0;
vm_Cell2a_(spTOS,a_code);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_code=", vm_out); printarg_a_(a_code);
}
#endif
{
#line 2539 "prim"
a_prim = (Cell *)decompile_code((Label)a_code);
#line 14119 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" a_prim=", vm_out); printarg_a_(a_prim);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_a_2Cell(a_prim,spTOS);
LABEL2(decompile_prim)
NEXT_P1_5;
LABEL3(decompile_prim)
DO_GOTO;
}

LABEL(set_next_code) /* set-next-code ( #w -- ) S1 -- S1  */
/*  */
NAME("set-next-code")
{
DEF_CA
MAYBE_UNUSED Cell w;
NEXT_P0;
vm_Cell2w(IMM_ARG(IPTOS,305397799 ),w);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w=", vm_out); printarg_w(w);
}
#endif
INC_IP(1);
{
#line 2545 "prim"
#ifdef NO_IP
next_code = (Label)w;
#endif
#line 14155 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
LABEL2(set_next_code)
NEXT_P1_5;
LABEL3(set_next_code)
DO_GOTO;
}

LABEL(call2) /* call2 ( #a_callee #a_ret_addr -- R:a_ret_addr ) S1 -- S1  */
/*  */
NAME("call2")
{
DEF_CA
MAYBE_UNUSED Cell * a_callee;
MAYBE_UNUSED Cell * a_ret_addr;
NEXT_P0;
vm_Cell2a_(IMM_ARG(IPTOS,305397800 ),a_callee);
vm_Cell2a_(IMM_ARG(IP[1],305397801 ),a_ret_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_callee=", vm_out); printarg_a_(a_callee);
fputs(" a_ret_addr=", vm_out); printarg_a_(a_ret_addr);
}
#endif
INC_IP(2);
rp += -1;
{
#line 2550 "prim"
/* call with explicit return address */
#ifdef NO_IP

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_a_2Cell(a_ret_addr,rp[0]);

JUMP(a_callee);
#else
assert(0);
#endif
#line 14205 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_a_2Cell(a_ret_addr,rp[0]);
LABEL2(call2)
NEXT_P1_5;
LABEL3(call2)
DO_GOTO;
}

LABEL(tag_offsets) /* tag-offsets ( -- a_addr ) S1 -- S1  */
/*  */
NAME("tag-offsets")
{
DEF_CA
Cell * a_addr;
NEXT_P0;
sp[0] = spTOS;
#ifdef VM_DEBUG
if (vm_debug) {
}
#endif
sp += -1;
{
#line 2559 "prim"
extern Cell groups[32];
a_addr = groups;
#line 14238 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" a_addr=", vm_out); printarg_a_(a_addr);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_a_2Cell(a_addr,spTOS);
LABEL2(tag_offsets)
NEXT_P1_5;
LABEL3(tag_offsets)
DO_GOTO;
}

GROUPADD(7)
#endif
GROUPADD(0)
GROUP( static_super, 335)
LABEL(super1)
{
DEF_CA
Cell MAYBE_UNUSED _IP0;
Cell MAYBE_UNUSED _IP1;
Cell MAYBE_UNUSED _sp0;
Cell MAYBE_UNUSED _sp1;
NEXT_P0;
/* lit ( #w -- w ) */
NAME("lit")
{
MAYBE_UNUSED Cell w;
vm_Cell2w(IMM_ARG(IPTOS,305397802 ),w);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w=", vm_out); printarg_w(w);
}
#endif
{
#line 2559 "prim"
#line 14279 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
vm_w2Cell(w,_sp1);
}
/* + ( n1 n2 -- n ) */
NAME("+")
{
MAYBE_UNUSED Cell n1;
MAYBE_UNUSED Cell n2;
Cell n;
vm_Cell2n(spTOS,n1);
vm_Cell2n(_sp1,n2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n1=", vm_out); printarg_n(n1);
fputs(" n2=", vm_out); printarg_n(n2);
}
#endif
INC_IP(2);
{
#line 2559 "prim"
n = n1+n2;
#line 14307 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n=", vm_out); printarg_n(n);
fputc('\n', vm_out);
}
#endif
vm_n2Cell(n,spTOS);
}
NEXT_P1;
LABEL2(super1)
NEXT_P1_5;
LABEL3(super1)
DO_GOTO;
}

LABEL(super3)
{
DEF_CA
Cell MAYBE_UNUSED _IP0;
Cell MAYBE_UNUSED _IP1;
Cell MAYBE_UNUSED _sp0;
NEXT_P0;
sp[0] = spTOS;
/* lit ( #w -- w ) */
NAME("lit")
{
MAYBE_UNUSED Cell w;
vm_Cell2w(IMM_ARG(IPTOS,305397803 ),w);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w=", vm_out); printarg_w(w);
}
#endif
sp += -1;
{
#line 2559 "prim"
#line 14346 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
vm_w2Cell(w,_sp0);
}
/* @ ( a_addr -- w ) */
NAME("@")
{
MAYBE_UNUSED Cell * a_addr;
Cell w;
vm_Cell2a_(_sp0,a_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_addr=", vm_out); printarg_a_(a_addr);
}
#endif
INC_IP(2);
{
#line 2559 "prim"
w = *a_addr;
#line 14371 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" w=", vm_out); printarg_w(w);
fputc('\n', vm_out);
}
#endif
vm_w2Cell(w,spTOS);
}
NEXT_P1;
LABEL2(super3)
NEXT_P1_5;
LABEL3(super3)
DO_GOTO;
}

LABEL(super6)
{
DEF_CA
Cell MAYBE_UNUSED _IP0;
Cell MAYBE_UNUSED _IP1;
Cell MAYBE_UNUSED _sp0;
Cell MAYBE_UNUSED _sp1;
NEXT_P0;
/* lit ( #w -- w ) */
NAME("lit")
{
MAYBE_UNUSED Cell w;
vm_Cell2w(IMM_ARG(IPTOS,305397804 ),w);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w=", vm_out); printarg_w(w);
}
#endif
{
#line 2559 "prim"
#line 14409 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
vm_w2Cell(w,_sp1);
}
/* ! ( w a_addr -- ) */
NAME("!")
{
MAYBE_UNUSED Cell w;
MAYBE_UNUSED Cell * a_addr;
vm_Cell2w(spTOS,w);
vm_Cell2a_(_sp1,a_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w=", vm_out); printarg_w(w);
fputs(" a_addr=", vm_out); printarg_a_(a_addr);
}
#endif
INC_IP(2);
sp += 1;
{
#line 2559 "prim"
*a_addr = w;
#line 14437 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
}
NEXT_P1;
spTOS = sp[0];
LABEL2(super6)
NEXT_P1_5;
LABEL3(super6)
DO_GOTO;
}

LABEL(super11)
{
DEF_CA
Cell MAYBE_UNUSED _IP0;
Cell MAYBE_UNUSED _sp0;
Cell MAYBE_UNUSED _sp1;
Cell MAYBE_UNUSED _rp0;
NEXT_P0;
/* ! ( w a_addr -- ) */
NAME("!")
{
MAYBE_UNUSED Cell w;
MAYBE_UNUSED Cell * a_addr;
vm_Cell2w(sp[1],w);
vm_Cell2a_(spTOS,a_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w=", vm_out); printarg_w(w);
fputs(" a_addr=", vm_out); printarg_a_(a_addr);
}
#endif
sp += 2;
{
#line 2559 "prim"
*a_addr = w;
#line 14479 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
}
/* ;s ( R:w -- ) */
NAME(";s")
{
MAYBE_UNUSED Cell w;
vm_Cell2w(rp[0],w);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w=", vm_out); printarg_w(w);
}
#endif
INC_IP(1);
rp += 1;
{
#line 2559 "prim"
#ifdef NO_IP

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
SUPER_END;
NEXT_P1;
spTOS = sp[0];

goto *(void *)w;
#else
SET_IP((Xt *)w);
#endif
#line 14517 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
}
SUPER_END;
NEXT_P1;
spTOS = sp[0];
LABEL2(super11)
NEXT_P1_5;
LABEL3(super11)
DO_GOTO;
}

LABEL(super12)
{
DEF_CA
Cell MAYBE_UNUSED _IP0;
Cell MAYBE_UNUSED _IP1;
Cell MAYBE_UNUSED _IP2;
Cell MAYBE_UNUSED _sp0;
Cell MAYBE_UNUSED _sp1;
NEXT_P0;
/* lit ( #w -- w ) */
NAME("lit")
{
MAYBE_UNUSED Cell w;
vm_Cell2w(IMM_ARG(IPTOS,305397805 ),w);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w=", vm_out); printarg_w(w);
}
#endif
{
#line 2559 "prim"
#line 14556 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
vm_w2Cell(w,_sp1);
}
/* + ( n1 n2 -- n ) */
NAME("+")
{
MAYBE_UNUSED Cell n1;
MAYBE_UNUSED Cell n2;
Cell n;
vm_Cell2n(spTOS,n1);
vm_Cell2n(_sp1,n2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n1=", vm_out); printarg_n(n1);
fputs(" n2=", vm_out); printarg_n(n2);
}
#endif
{
#line 2559 "prim"
n = n1+n2;
#line 14583 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n=", vm_out); printarg_n(n);
fputc('\n', vm_out);
}
#endif
vm_n2Cell(n,_sp0);
}
/* @ ( a_addr -- w ) */
NAME("@")
{
MAYBE_UNUSED Cell * a_addr;
Cell w;
vm_Cell2a_(_sp0,a_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_addr=", vm_out); printarg_a_(a_addr);
}
#endif
INC_IP(3);
{
#line 2559 "prim"
w = *a_addr;
#line 14609 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" w=", vm_out); printarg_w(w);
fputc('\n', vm_out);
}
#endif
vm_w2Cell(w,spTOS);
}
NEXT_P1;
LABEL2(super12)
NEXT_P1_5;
LABEL3(super12)
DO_GOTO;
}

LABEL(super15)
{
DEF_CA
Cell MAYBE_UNUSED _IP0;
Cell MAYBE_UNUSED _IP1;
Cell MAYBE_UNUSED _sp0;
NEXT_P0;
sp[0] = spTOS;
/* useraddr ( #u -- a_addr ) */
NAME("useraddr")
{
MAYBE_UNUSED UCell u;
Cell * a_addr;
vm_Cell2u(IMM_ARG(IPTOS,305397806 ),u);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" u=", vm_out); printarg_u(u);
}
#endif
sp += -1;
{
#line 2559 "prim"
a_addr = (Cell *)(up+u);
#line 14650 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" a_addr=", vm_out); printarg_a_(a_addr);
fputc('\n', vm_out);
}
#endif
vm_a_2Cell(a_addr,_sp0);
}
/* @ ( a_addr -- w ) */
NAME("@")
{
MAYBE_UNUSED Cell * a_addr;
Cell w;
vm_Cell2a_(_sp0,a_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_addr=", vm_out); printarg_a_(a_addr);
}
#endif
INC_IP(2);
{
#line 2559 "prim"
w = *a_addr;
#line 14676 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" w=", vm_out); printarg_w(w);
fputc('\n', vm_out);
}
#endif
vm_w2Cell(w,spTOS);
}
NEXT_P1;
LABEL2(super15)
NEXT_P1_5;
LABEL3(super15)
DO_GOTO;
}

LABEL(super16)
{
DEF_CA
Cell MAYBE_UNUSED _IP0;
Cell MAYBE_UNUSED _sp0;
Cell MAYBE_UNUSED _sp1;
NEXT_P0;
/* + ( n1 n2 -- n ) */
NAME("+")
{
MAYBE_UNUSED Cell n1;
MAYBE_UNUSED Cell n2;
Cell n;
vm_Cell2n(sp[1],n1);
vm_Cell2n(spTOS,n2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n1=", vm_out); printarg_n(n1);
fputs(" n2=", vm_out); printarg_n(n2);
}
#endif
sp += 1;
{
#line 2559 "prim"
n = n1+n2;
#line 14719 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n=", vm_out); printarg_n(n);
fputc('\n', vm_out);
}
#endif
vm_n2Cell(n,_sp0);
}
/* @ ( a_addr -- w ) */
NAME("@")
{
MAYBE_UNUSED Cell * a_addr;
Cell w;
vm_Cell2a_(_sp0,a_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_addr=", vm_out); printarg_a_(a_addr);
}
#endif
INC_IP(1);
{
#line 2559 "prim"
w = *a_addr;
#line 14745 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" w=", vm_out); printarg_w(w);
fputc('\n', vm_out);
}
#endif
vm_w2Cell(w,spTOS);
}
NEXT_P1;
LABEL2(super16)
NEXT_P1_5;
LABEL3(super16)
DO_GOTO;
}

LABEL(super18)
{
DEF_CA
Cell MAYBE_UNUSED _IP0;
Cell MAYBE_UNUSED _IP1;
Cell MAYBE_UNUSED _IP2;
Cell MAYBE_UNUSED _sp0;
Cell MAYBE_UNUSED _sp1;
Cell MAYBE_UNUSED _rp0;
NEXT_P0;
/* lit ( #w -- w ) */
NAME("lit")
{
MAYBE_UNUSED Cell w;
vm_Cell2w(IMM_ARG(IPTOS,305397807 ),w);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w=", vm_out); printarg_w(w);
}
#endif
{
#line 2559 "prim"
#line 14785 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
vm_w2Cell(w,_sp1);
}
/* ! ( w a_addr -- ) */
NAME("!")
{
MAYBE_UNUSED Cell w;
MAYBE_UNUSED Cell * a_addr;
vm_Cell2w(spTOS,w);
vm_Cell2a_(_sp1,a_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w=", vm_out); printarg_w(w);
fputs(" a_addr=", vm_out); printarg_a_(a_addr);
}
#endif
sp += 1;
{
#line 2559 "prim"
*a_addr = w;
#line 14812 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
}
/* ;s ( R:w -- ) */
NAME(";s")
{
MAYBE_UNUSED Cell w;
vm_Cell2w(rp[0],w);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w=", vm_out); printarg_w(w);
}
#endif
INC_IP(3);
rp += 1;
{
#line 2559 "prim"
#ifdef NO_IP

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
SUPER_END;
NEXT_P1;
spTOS = sp[0];

goto *(void *)w;
#else
SET_IP((Xt *)w);
#endif
#line 14850 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
}
SUPER_END;
NEXT_P1;
spTOS = sp[0];
LABEL2(super18)
NEXT_P1_5;
LABEL3(super18)
DO_GOTO;
}

LABEL(super19)
{
DEF_CA
Cell MAYBE_UNUSED _IP0;
Cell MAYBE_UNUSED _IP1;
Cell MAYBE_UNUSED _IP2;
Cell MAYBE_UNUSED _sp0;
Cell MAYBE_UNUSED _sp1;
NEXT_P0;
/* lit ( #w -- w ) */
NAME("lit")
{
MAYBE_UNUSED Cell w;
vm_Cell2w(IMM_ARG(IPTOS,305397808 ),w);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w=", vm_out); printarg_w(w);
}
#endif
{
#line 2559 "prim"
#line 14889 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
vm_w2Cell(w,_sp1);
}
/* @ ( a_addr -- w ) */
NAME("@")
{
MAYBE_UNUSED Cell * a_addr;
Cell w;
vm_Cell2a_(_sp1,a_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_addr=", vm_out); printarg_a_(a_addr);
}
#endif
{
#line 2559 "prim"
w = *a_addr;
#line 14913 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" w=", vm_out); printarg_w(w);
fputc('\n', vm_out);
}
#endif
vm_w2Cell(w,_sp1);
}
/* and ( w1 w2 -- w ) */
NAME("and")
{
MAYBE_UNUSED Cell w1;
MAYBE_UNUSED Cell w2;
Cell w;
vm_Cell2w(spTOS,w1);
vm_Cell2w(_sp1,w2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w1=", vm_out); printarg_w(w1);
fputs(" w2=", vm_out); printarg_w(w2);
}
#endif
INC_IP(3);
{
#line 2559 "prim"
w = w1&w2;
#line 14942 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" w=", vm_out); printarg_w(w);
fputc('\n', vm_out);
}
#endif
vm_w2Cell(w,spTOS);
}
NEXT_P1;
LABEL2(super19)
NEXT_P1_5;
LABEL3(super19)
DO_GOTO;
}

LABEL(super23)
{
DEF_CA
Cell MAYBE_UNUSED _IP0;
Cell MAYBE_UNUSED _IP1;
Cell MAYBE_UNUSED _sp0;
Cell MAYBE_UNUSED _sp1;
NEXT_P0;
/* useraddr ( #u -- a_addr ) */
NAME("useraddr")
{
MAYBE_UNUSED UCell u;
Cell * a_addr;
vm_Cell2u(IMM_ARG(IPTOS,305397809 ),u);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" u=", vm_out); printarg_u(u);
}
#endif
{
#line 2559 "prim"
a_addr = (Cell *)(up+u);
#line 14982 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" a_addr=", vm_out); printarg_a_(a_addr);
fputc('\n', vm_out);
}
#endif
vm_a_2Cell(a_addr,_sp1);
}
/* ! ( w a_addr -- ) */
NAME("!")
{
MAYBE_UNUSED Cell w;
MAYBE_UNUSED Cell * a_addr;
vm_Cell2w(spTOS,w);
vm_Cell2a_(_sp1,a_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w=", vm_out); printarg_w(w);
fputs(" a_addr=", vm_out); printarg_a_(a_addr);
}
#endif
INC_IP(2);
sp += 1;
{
#line 2559 "prim"
*a_addr = w;
#line 15011 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
}
NEXT_P1;
spTOS = sp[0];
LABEL2(super23)
NEXT_P1_5;
LABEL3(super23)
DO_GOTO;
}

LABEL(super25)
{
DEF_CA
Cell MAYBE_UNUSED _IP0;
Cell MAYBE_UNUSED _sp0;
Cell MAYBE_UNUSED _rp0;
NEXT_P0;
/* @ ( a_addr -- w ) */
NAME("@")
{
MAYBE_UNUSED Cell * a_addr;
Cell w;
vm_Cell2a_(spTOS,a_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_addr=", vm_out); printarg_a_(a_addr);
}
#endif
{
#line 2559 "prim"
w = *a_addr;
#line 15049 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" w=", vm_out); printarg_w(w);
fputc('\n', vm_out);
}
#endif
vm_w2Cell(w,spTOS);
}
/* ;s ( R:w -- ) */
NAME(";s")
{
MAYBE_UNUSED Cell w;
vm_Cell2w(rp[0],w);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w=", vm_out); printarg_w(w);
}
#endif
INC_IP(1);
rp += 1;
{
#line 2559 "prim"
#ifdef NO_IP

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
SUPER_END;
NEXT_P1;

goto *(void *)w;
#else
SET_IP((Xt *)w);
#endif
#line 15088 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
}
SUPER_END;
NEXT_P1;
LABEL2(super25)
NEXT_P1_5;
LABEL3(super25)
DO_GOTO;
}

LABEL(super26)
{
DEF_CA
Cell MAYBE_UNUSED _IP0;
Cell MAYBE_UNUSED _IP1;
Cell MAYBE_UNUSED _IP2;
Cell MAYBE_UNUSED _sp0;
Cell MAYBE_UNUSED _sp1;
NEXT_P0;
/* lit ( #w -- w ) */
NAME("lit")
{
MAYBE_UNUSED Cell w;
vm_Cell2w(IMM_ARG(IPTOS,305397810 ),w);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w=", vm_out); printarg_w(w);
}
#endif
{
#line 2559 "prim"
#line 15126 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
vm_w2Cell(w,_sp1);
}
/* @ ( a_addr -- w ) */
NAME("@")
{
MAYBE_UNUSED Cell * a_addr;
Cell w;
vm_Cell2a_(_sp1,a_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_addr=", vm_out); printarg_a_(a_addr);
}
#endif
{
#line 2559 "prim"
w = *a_addr;
#line 15150 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" w=", vm_out); printarg_w(w);
fputc('\n', vm_out);
}
#endif
vm_w2Cell(w,_sp1);
}
/* + ( n1 n2 -- n ) */
NAME("+")
{
MAYBE_UNUSED Cell n1;
MAYBE_UNUSED Cell n2;
Cell n;
vm_Cell2n(spTOS,n1);
vm_Cell2n(_sp1,n2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n1=", vm_out); printarg_n(n1);
fputs(" n2=", vm_out); printarg_n(n2);
}
#endif
INC_IP(3);
{
#line 2559 "prim"
n = n1+n2;
#line 15179 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" n=", vm_out); printarg_n(n);
fputc('\n', vm_out);
}
#endif
vm_n2Cell(n,spTOS);
}
NEXT_P1;
LABEL2(super26)
NEXT_P1_5;
LABEL3(super26)
DO_GOTO;
}

LABEL(super27)
{
DEF_CA
Cell MAYBE_UNUSED _IP0;
Cell MAYBE_UNUSED _sp0;
Cell MAYBE_UNUSED _sp1;
NEXT_P0;
/* dup ( w -- w w ) */
NAME("dup")
{
MAYBE_UNUSED Cell w;
vm_Cell2w(spTOS,w);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w=", vm_out); printarg_w(w);
}
#endif
sp += -1;
{
#line 2559 "prim"
#line 15217 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
vm_w2Cell(w,sp[1]);
vm_w2Cell(w,_sp1);
}
/* @ ( a_addr -- w ) */
NAME("@")
{
MAYBE_UNUSED Cell * a_addr;
Cell w;
vm_Cell2a_(_sp1,a_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_addr=", vm_out); printarg_a_(a_addr);
}
#endif
INC_IP(1);
{
#line 2559 "prim"
w = *a_addr;
#line 15243 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" w=", vm_out); printarg_w(w);
fputc('\n', vm_out);
}
#endif
vm_w2Cell(w,spTOS);
}
NEXT_P1;
LABEL2(super27)
NEXT_P1_5;
LABEL3(super27)
DO_GOTO;
}

LABEL(_p0) /* lit ( #w -- w ) S0 -- S1  */
/*  */
NAME("lit")
{
DEF_CA
MAYBE_UNUSED Cell w;
NEXT_P0;
vm_Cell2w(IMM_ARG(IPTOS,305397811 ),w);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w=", vm_out); printarg_w(w);
}
#endif
INC_IP(1);
{
#line 2559 "prim"
#line 15277 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w,spTOS);
LABEL2(_p0)
NEXT_P1_5;
LABEL3(_p0)
DO_GOTO;
}

LABEL(_p1) /* lit@ ( #a_addr -- w ) S0 -- S1  */
/*  */
NAME("lit@")
{
DEF_CA
MAYBE_UNUSED Cell * a_addr;
Cell w;
NEXT_P0;
vm_Cell2a_(IMM_ARG(IPTOS,305397812 ),a_addr);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" a_addr=", vm_out); printarg_a_(a_addr);
}
#endif
INC_IP(1);
{
#line 2559 "prim"
w = *a_addr;
#line 15311 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" w=", vm_out); printarg_w(w);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w,spTOS);
LABEL2(_p1)
NEXT_P1_5;
LABEL3(_p1)
DO_GOTO;
}

LABEL(_p2) /* dup ( w -- w w ) S0 -- S1  */
/*  */
NAME("dup")
{
DEF_CA
MAYBE_UNUSED Cell w;
NEXT_P0;
vm_Cell2w(sp[1],w);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w=", vm_out); printarg_w(w);
}
#endif
{
#line 2559 "prim"
#line 15343 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w,spTOS);
LABEL2(_p2)
NEXT_P1_5;
LABEL3(_p2)
DO_GOTO;
}

LABEL(_p3) /* i ( R:n -- R:n n ) S0 -- S1  */
/*  */
NAME("i")
{
DEF_CA
MAYBE_UNUSED Cell n;
NEXT_P0;
vm_Cell2n(rp[0],n);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" n=", vm_out); printarg_n(n);
}
#endif
{
#line 2559 "prim"
#line 15374 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_n2Cell(n,spTOS);
LABEL2(_p3)
NEXT_P1_5;
LABEL3(_p3)
DO_GOTO;
}

LABEL(_p4) /* useraddr ( #u -- a_addr ) S0 -- S1  */
/*  */
NAME("useraddr")
{
DEF_CA
MAYBE_UNUSED UCell u;
Cell * a_addr;
NEXT_P0;
vm_Cell2u(IMM_ARG(IPTOS,305397813 ),u);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" u=", vm_out); printarg_u(u);
}
#endif
INC_IP(1);
{
#line 2559 "prim"
a_addr = (Cell *)(up+u);
#line 15408 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" a_addr=", vm_out); printarg_a_(a_addr);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_a_2Cell(a_addr,spTOS);
LABEL2(_p4)
NEXT_P1_5;
LABEL3(_p4)
DO_GOTO;
}

LABEL(_p5) /* over ( w1 w2 -- w1 w2 w1 ) S0 -- S1  */
/*  */
NAME("over")
{
DEF_CA
MAYBE_UNUSED Cell w1;
MAYBE_UNUSED Cell w2;
NEXT_P0;
vm_Cell2w(sp[2],w1);
vm_Cell2w(sp[1],w2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w1=", vm_out); printarg_w(w1);
fputs(" w2=", vm_out); printarg_w(w2);
}
#endif
{
#line 2559 "prim"
#line 15443 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w1,spTOS);
LABEL2(_p5)
NEXT_P1_5;
LABEL3(_p5)
DO_GOTO;
}

LABEL(_p6) /* 2dup ( w1 w2 -- w1 w2 w1 w2 ) S0 -- S1  */
/*  */
NAME("2dup")
{
DEF_CA
MAYBE_UNUSED Cell w1;
MAYBE_UNUSED Cell w2;
NEXT_P0;
vm_Cell2w(sp[2],w1);
vm_Cell2w(sp[1],w2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w1=", vm_out); printarg_w(w1);
fputs(" w2=", vm_out); printarg_w(w2);
}
#endif
sp += -1;
{
#line 2559 "prim"
#line 15478 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w1,sp[1]);
vm_w2Cell(w2,spTOS);
LABEL2(_p6)
NEXT_P1_5;
LABEL3(_p6)
DO_GOTO;
}

LABEL(_p7) /* drop ( w -- ) S1 -- S0  */
/*  */
NAME("drop")
{
DEF_CA
MAYBE_UNUSED Cell w;
NEXT_P0;
vm_Cell2w(spTOS,w);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w=", vm_out); printarg_w(w);
}
#endif
{
#line 2559 "prim"
#line 15510 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
LABEL2(_p7)
NEXT_P1_5;
LABEL3(_p7)
DO_GOTO;
}

LABEL(_p8) /* drop ( w -- ) S0 -- S0  */
/*  */
NAME("drop")
{
DEF_CA
MAYBE_UNUSED Cell w;
NEXT_P0;
vm_Cell2w(sp[1],w);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w=", vm_out); printarg_w(w);
}
#endif
sp += 1;
{
#line 2559 "prim"
#line 15541 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
LABEL2(_p8)
NEXT_P1_5;
LABEL3(_p8)
DO_GOTO;
}

LABEL(_p9) /* >r ( w -- R:w ) S1 -- S0  */
/*  */
NAME(">r")
{
DEF_CA
MAYBE_UNUSED Cell w;
NEXT_P0;
vm_Cell2w(spTOS,w);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w=", vm_out); printarg_w(w);
}
#endif
rp += -1;
{
#line 2559 "prim"
#line 15572 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w,rp[0]);
LABEL2(_p9)
NEXT_P1_5;
LABEL3(_p9)
DO_GOTO;
}

LABEL(_p10) /* r> ( R:w -- w ) S0 -- S1  */
/*  */
NAME("r>")
{
DEF_CA
MAYBE_UNUSED Cell w;
NEXT_P0;
vm_Cell2w(rp[0],w);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w=", vm_out); printarg_w(w);
}
#endif
rp += 1;
{
#line 2559 "prim"
#line 15604 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w,spTOS);
LABEL2(_p10)
NEXT_P1_5;
LABEL3(_p10)
DO_GOTO;
}

LABEL(_p11) /* lp+!# ( #noffset -- ) S0 -- S0  */
/* used with negative immediate values it allocates memory on the
local stack, a positive immediate argument drops memory from the local
stack */
NAME("lp+!#")
{
DEF_CA
MAYBE_UNUSED Cell noffset;
NEXT_P0;
vm_Cell2n(IMM_ARG(IPTOS,305397814 ),noffset);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" noffset=", vm_out); printarg_n(noffset);
}
#endif
INC_IP(1);
{
#line 2559 "prim"
lp += noffset;
#line 15639 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
LABEL2(_p11)
NEXT_P1_5;
LABEL3(_p11)
DO_GOTO;
}

LABEL(_p12) /* unloop ( R:w1 R:w2 -- ) S0 -- S0  */
/*  */
NAME("unloop")
{
DEF_CA
MAYBE_UNUSED Cell w1;
MAYBE_UNUSED Cell w2;
NEXT_P0;
vm_Cell2w(rp[1],w1);
vm_Cell2w(rp[0],w2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w1=", vm_out); printarg_w(w1);
fputs(" w2=", vm_out); printarg_w(w2);
}
#endif
rp += 2;
{
#line 2559 "prim"
/* !! alias for 2rdrop */
#line 15674 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
LABEL2(_p12)
NEXT_P1_5;
LABEL3(_p12)
DO_GOTO;
}

LABEL(_p13) /* lp+ ( -- ) S0 -- S0  */
/*  */
NAME("lp+")
{
DEF_CA
NEXT_P0;
#ifdef VM_DEBUG
if (vm_debug) {
}
#endif
{
#line 2559 "prim"
lp += sizeof(Float);
#line 15702 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
LABEL2(_p13)
NEXT_P1_5;
LABEL3(_p13)
DO_GOTO;
}

LABEL(_p14) /* @local0 ( -- w ) S0 -- S1  */
/*  */
NAME("@local0")
{
DEF_CA
Cell w;
NEXT_P0;
#ifdef VM_DEBUG
if (vm_debug) {
}
#endif
{
#line 2559 "prim"
w = ((Cell *)lp)[0];
#line 15731 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" w=", vm_out); printarg_w(w);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w,spTOS);
LABEL2(_p14)
NEXT_P1_5;
LABEL3(_p14)
DO_GOTO;
}

LABEL(_p15) /* nip ( w1 w2 -- w2 ) S0 -- S1  */
/*  */
NAME("nip")
{
DEF_CA
MAYBE_UNUSED Cell w1;
MAYBE_UNUSED Cell w2;
NEXT_P0;
vm_Cell2w(sp[2],w1);
vm_Cell2w(sp[1],w2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w1=", vm_out); printarg_w(w1);
fputs(" w2=", vm_out); printarg_w(w2);
}
#endif
sp += 2;
{
#line 2559 "prim"
#line 15767 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w2,spTOS);
LABEL2(_p15)
NEXT_P1_5;
LABEL3(_p15)
DO_GOTO;
}

LABEL(_p16) /* lp- ( -- ) S0 -- S0  */
/*  */
NAME("lp-")
{
DEF_CA
NEXT_P0;
#ifdef VM_DEBUG
if (vm_debug) {
}
#endif
{
#line 2559 "prim"
lp += -sizeof(Cell);
#line 15796 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
LABEL2(_p16)
NEXT_P1_5;
LABEL3(_p16)
DO_GOTO;
}

LABEL(_p17) /* @local# ( #noffset -- w ) S0 -- S1  */
/*  */
NAME("@local#")
{
DEF_CA
MAYBE_UNUSED Cell noffset;
Cell w;
NEXT_P0;
vm_Cell2n(IMM_ARG(IPTOS,305397815 ),noffset);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" noffset=", vm_out); printarg_n(noffset);
}
#endif
INC_IP(1);
{
#line 2559 "prim"
w = *(Cell *)(lp+noffset);
#line 15829 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" w=", vm_out); printarg_w(w);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w,spTOS);
LABEL2(_p17)
NEXT_P1_5;
LABEL3(_p17)
DO_GOTO;
}

LABEL(_p18) /* 2drop ( w1 w2 -- ) S0 -- S0  */
/*  */
NAME("2drop")
{
DEF_CA
MAYBE_UNUSED Cell w1;
MAYBE_UNUSED Cell w2;
NEXT_P0;
vm_Cell2w(sp[2],w1);
vm_Cell2w(sp[1],w2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w1=", vm_out); printarg_w(w1);
fputs(" w2=", vm_out); printarg_w(w2);
}
#endif
sp += 2;
{
#line 2559 "prim"
#line 15865 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
LABEL2(_p18)
NEXT_P1_5;
LABEL3(_p18)
DO_GOTO;
}

LABEL(_p19) /* 2drop ( w1 w2 -- ) S1 -- S0  */
/*  */
NAME("2drop")
{
DEF_CA
MAYBE_UNUSED Cell w1;
MAYBE_UNUSED Cell w2;
NEXT_P0;
vm_Cell2w(sp[1],w1);
vm_Cell2w(spTOS,w2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w1=", vm_out); printarg_w(w1);
fputs(" w2=", vm_out); printarg_w(w2);
}
#endif
sp += 1;
{
#line 2559 "prim"
#line 15899 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
LABEL2(_p19)
NEXT_P1_5;
LABEL3(_p19)
DO_GOTO;
}

LABEL(_p20) /* @local2 ( -- w ) S0 -- S1  */
/*  */
NAME("@local2")
{
DEF_CA
Cell w;
NEXT_P0;
#ifdef VM_DEBUG
if (vm_debug) {
}
#endif
{
#line 2559 "prim"
w = ((Cell *)lp)[2];
#line 15928 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" w=", vm_out); printarg_w(w);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w,spTOS);
LABEL2(_p20)
NEXT_P1_5;
LABEL3(_p20)
DO_GOTO;
}

LABEL(_p21) /* rdrop ( R:w -- ) S0 -- S0  */
/*  */
NAME("rdrop")
{
DEF_CA
MAYBE_UNUSED Cell w;
NEXT_P0;
vm_Cell2w(rp[0],w);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w=", vm_out); printarg_w(w);
}
#endif
rp += 1;
{
#line 2559 "prim"
#line 15961 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
LABEL2(_p21)
NEXT_P1_5;
LABEL3(_p21)
DO_GOTO;
}

LABEL(_p22) /* @local1 ( -- w ) S0 -- S1  */
/*  */
NAME("@local1")
{
DEF_CA
Cell w;
NEXT_P0;
#ifdef VM_DEBUG
if (vm_debug) {
}
#endif
{
#line 2559 "prim"
w = ((Cell *)lp)[1];
#line 15990 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" w=", vm_out); printarg_w(w);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w,spTOS);
LABEL2(_p22)
NEXT_P1_5;
LABEL3(_p22)
DO_GOTO;
}

LABEL(_p23) /* i' ( R:w R:w2 -- R:w R:w2 w ) S0 -- S1  */
/*  */
NAME("i'")
{
DEF_CA
MAYBE_UNUSED Cell w;
MAYBE_UNUSED Cell w2;
NEXT_P0;
vm_Cell2w(rp[1],w);
vm_Cell2w(rp[0],w2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w=", vm_out); printarg_w(w);
fputs(" w2=", vm_out); printarg_w(w2);
}
#endif
{
#line 2559 "prim"
#line 16025 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w,spTOS);
LABEL2(_p23)
NEXT_P1_5;
LABEL3(_p23)
DO_GOTO;
}

LABEL(_p24) /* laddr# ( #noffset -- c_addr ) S0 -- S1  */
/*  */
NAME("laddr#")
{
DEF_CA
MAYBE_UNUSED Cell noffset;
Char * c_addr;
NEXT_P0;
vm_Cell2n(IMM_ARG(IPTOS,305397816 ),noffset);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" noffset=", vm_out); printarg_n(noffset);
}
#endif
INC_IP(1);
{
#line 2559 "prim"
/* this can also be used to implement lp@ */
c_addr = (Char *)(lp+noffset);
#line 16060 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" c_addr=", vm_out); printarg_c_(c_addr);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_c_2Cell(c_addr,spTOS);
LABEL2(_p24)
NEXT_P1_5;
LABEL3(_p24)
DO_GOTO;
}

LABEL(_p25) /* j ( R:w R:w1 R:w2 -- w R:w R:w1 R:w2 ) S0 -- S1  */
/*  */
NAME("j")
{
DEF_CA
MAYBE_UNUSED Cell w;
MAYBE_UNUSED Cell w1;
MAYBE_UNUSED Cell w2;
NEXT_P0;
vm_Cell2w(rp[2],w);
vm_Cell2w(rp[1],w1);
vm_Cell2w(rp[0],w2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w=", vm_out); printarg_w(w);
fputs(" w1=", vm_out); printarg_w(w1);
fputs(" w2=", vm_out); printarg_w(w2);
}
#endif
{
#line 2559 "prim"
#line 16098 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_w2Cell(w,spTOS);
LABEL2(_p25)
NEXT_P1_5;
LABEL3(_p25)
DO_GOTO;
}

LABEL(_p26) /* 2rdrop ( R:d -- ) S0 -- S0  */
/*  */
NAME("2rdrop")
{
DEF_CA
MAYBE_UNUSED DCell d;
NEXT_P0;
vm_twoCell2d(rp[1], rp[0], d)
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" d=", vm_out); printarg_d(d);
}
#endif
rp += 2;
{
#line 2559 "prim"
#line 16130 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
LABEL2(_p26)
NEXT_P1_5;
LABEL3(_p26)
DO_GOTO;
}

LABEL(_p27) /* >l ( w -- ) S1 -- S0  */
/*  */
NAME(">l")
{
DEF_CA
MAYBE_UNUSED Cell w;
NEXT_P0;
vm_Cell2w(spTOS,w);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" w=", vm_out); printarg_w(w);
}
#endif
{
#line 2559 "prim"
lp -= sizeof(Cell);
*(Cell *)lp = w;
#line 16162 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
LABEL2(_p27)
NEXT_P1_5;
LABEL3(_p27)
DO_GOTO;
}

LABEL(_p28) /* noop ( -- ) S1 -- S0  */
/*  */
NAME("noop")
{
DEF_CA
sp[0] = spTOS;
#ifdef VM_DEBUG
if (vm_debug) {
}
#endif
sp += -1;
{
#line 2559 "prim"
#line 16190 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
LABEL2(_p28)
NEXT_P1_5;
LABEL3(_p28)
DO_GOTO;
}

LABEL(_p29) /* noop ( -- ) S0 -- S1  */
/*  */
NAME("noop")
{
DEF_CA
#ifdef VM_DEBUG
if (vm_debug) {
}
#endif
sp += 1;
{
#line 2559 "prim"
#line 16216 "prim-fast.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
spTOS = sp[0];
LABEL2(_p29)
NEXT_P1_5;
LABEL3(_p29)
DO_GOTO;
}

GROUPADD(13)
GROUP( end, 348)
