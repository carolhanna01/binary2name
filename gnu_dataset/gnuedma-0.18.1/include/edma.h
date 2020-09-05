/*
 * EDMA: Entorno de Desarrollo Modular y Abierto
 * Object Oriented and Componetware Framework
 * Copyright (C) 1998, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2010
 *    David Mart.nez Oliveira
 *
 * This file is part of EDMA.
 *
 * EDMA is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * EDMA is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with EDMA.  If not, see <http://www.gnu.org/licenses/>.
 */

/*
  Entorno de Desarrollo Modular y Abierto
  (c) David Martínez Oliveira
  Versión Beta WIN32
  15 de Octubre de 1997
  
  ENlace dinámico de EDMA
  
  REVISIONES:-------------------------------------------------------------
  * September, 12th,2001
  * Added new primitives to manage static methods, casting and clonning
  * --------------------------------------------------------------
  * October, 14th,2001
  * Added EBufferRealloc and PRealloc function
  * ------------------------------------------------------------------
  * November, 24th, 2001
  * Update and code cleanup
  * -------------------------------------------------------------------
  * March, 3th,2002
  * File cleanup
  * -------------------------------------------------------------------
  * July, 13th, 2002
  * Added functions to query multiversiones classes
  * --------------------------------------------------------------------
  * August, 19th, 2004
  * Added functions and types for multithreading
  */

#ifndef EDMA_H
#define EDMA_H

#include <stdio.h>
#include <setjmp.h>
#include <portable.h>

#ifdef __cplusplus
extern "C"{
#endif

#define EDMA_TRY { OBJID __valXX; \
                   jmp_buf __envXX; \
                   edma_exception_try (&__envXX); \
                   if ((__valXX = setjmp (__envXX)) == 0) { 
#define EDMA_CATCH(e)  } else {\
                         e = __valXX - 1 ;
#define EDMA_TRY_END if ((__valXX-1)!=-1) edma_free_obj(__valXX - 1);\
                     } edma_exception_clean (__valXX);}
#if 0
#define EDMA_TRY_END edma_exception_clean (__valXX); \
												 if ((__valXX-1)!=-1) edma_free_obj(__valXX - 1);\
                     }}
#endif
#if 0													 
#define EDMA_CATCH(e)  edma_exception_clean (__valXX); \
                       } else {\
                         e = __valXX - 1 ;
#define EDMA_TRY_END if ((__valXX-1)!=-1) edma_free_obj(__valXX - 1);\
                     }}
#endif
//#define EDMA_TRY_END edma_free_obj (val);}
  /* Generic GNU/EDMA Error Constant. This will change*/
#define EDMA_ERROR     -1
  /*
  ** Class State Constants
  */
#define  CLASS_FREE    0     /* Free Entry */
#define  CLASS_DEF     1     /* Class is defined but not loaded */
#define  CLASS_ILOADED 2     /* Class is defined and interface loaded */
#define  CLASS_LOADED  3     /* Class is full in memory */
  
  /*
  ** Object State Constants
  */

#define OBJ_FREE        0
#define OBJ_EXIST       1
#define VIRTUAL_OBJECT	2

  /*
  ** Machines *************************************
  */

#define MAQ_NUM		64

#define I386		0
#define	I386_4		1
#define	I386_8		2
#define	I486		4
#define	I486_8		5
#define	I486_16		6
#define	I486_32		7
#define PENT		8
#define	PENT_8		9
#define	PENT_16		10
#define	PENT_32		11
#define	PENT_64		12

#define	VIRTUAL		13
#define	JAVA		14
#define ARM             15

  /*
  ** Operating Systems ************************************
  */

#define	SO_NUM		16

#define	WINDOWS		0
#define	WIN311		1
#define	WIN95		2
#define	WIN_NT		3
#define SYS_LINUX       4   
#define SYS_BSD     5
#define SYS_SOLARIS     6

  /* GNU EDMA Datatypes */

#define DT_EUINT8      0
#define DT_EUINT16     1
#define DT_EUINT32     2
#define DT_ESINT8      3
#define DT_ESINT16     4
#define DT_ESINT32     5
#define DT_EBYTE       6
#define DT_EWORD       7
#define DT_EDWORD      8
#define DT_ECHAR       9
#define DT_EBOOL      10

#define DT_EREAL64    12
#define DT_EZSTRING   13
#define DT_EBUFFER    14
#define DT_EOBJECT    15
#define DT_EUSER      16
#define DT_EPOINTER   17

  typedef struct {
    HMEM     h;
    EUint32  Size;
    EPVoid   dat;
  } EDMAT_BUFFER;
  
                                                    
#define E		0
#define L		1
#define E_L		2

  /* Field sizes */
#define EDMA_CLASS_NAME_LEN     80
#define EDMA_CLASS_MAKER_LEN    80
#define EDMA_CLASS_IMPL_LEN     80
#define EDMA_CLASS_MODULE_LEN   80
#define EDMA_CLASS_NAMESPACE_LEN 80

#define EDMA_ARCH_LEN           50
#define EDMA_SO_LEN             50

#define EDMA_MET_SIG_LEN        50
#define EDMA_GENERAL_ID_LEN     80

#define EDMA_MET_NAME_LEN       EDMA_GENERAL_ID_LEN
#define EDMA_PROP_NAME_LEN      EDMA_GENERAL_ID_LEN
#define EDMA_TYPE_NAME_LEN      EDMA_GENERAL_ID_LEN

#define EDMA_TYPE_SIG_LEN       50
#define EDMA_PATH_LEN		2048

#define EDMA_SHARED_CLASS       1
#define EDMA_LOCAL_CLASS        2

  /* Operation Identifiers... Virtual Object operations*/
  
#define	WRITE_PROP	0
#define READ_PROP	1
#define RUN_MET		2
#define GET_DATA	3

  /* Repository identifiers */
#define EDMA_SHARED_REPO        0
#define EDMA_LOCAL_REPO         1



/* Some additional constants for easy code reading */
#define IS_SIU_PROXY    1

  typedef ESint32     SOID;
  typedef ESint32     MAQID;
  typedef ESint32     CLASSID;
  typedef ESint32     VERSIONID;
  typedef ESint32     GROUPID;
  typedef ESint32     LEVELID;
  
  typedef ESint32     OBJID;
  typedef EPVoid      POBJ;
  
  typedef HMEM        BSEM;
  typedef void        (*PPROC)();

  typedef void *EDMA_REPO;
  
#if 0
  typedef struct {
    EPVoid	a;	/* Parameter */
    EPfChar	Id;	/* Call identifier */ 
    EUint16	IdPrim;	/* Priomitive Identifier*/
    OBJID	IdObj;	/* Object getting primitive. */
    EUint32	Ind;	/* Property-Method Index. To use level 1 primitives*/
  } VIR_MET_PARAMS;

  /* Not yet implemented.. Maybe removed in future versions*/
  typedef struct t_item {
    HMEM              hBlock;
    ESint32           IdObj;
    HMEM              hBuffer;
    EPVoid            Buffer;
    EUint32           Size;
    EPVoid            *n;
    EPVoid            *p;
  } NOT_ITEM;
#endif
  /* Class Information for dynamic class registering*/
  typedef struct {
    EPChar		ClassName;
    EPChar              NameSpace;
    EPChar		SOName;
    EPChar		MaqName;
    EPChar		IDFName;
    EPChar		SIUName;
    EByte		IsIDFParser;
    EByte		IsSIUProxy;
    EByte		IsEMIComp;
		ESint32	MajorVer;
		ESint32	MinorVer;
    EPChar              UpdateScript;
  } CLASS_INFO;

#ifndef INIMAN_C
  typedef void *PINIFILE;
#endif


  /* GNU/EDMA API functions*/
  ESint32 EDMAPROC edma_get_class_name (CLASSID,EPChar);   
  SOID	  EDMAPROC edma_get_class_so_id (CLASSID);
  MAQID	  EDMAPROC edma_get_class_arch_id (CLASSID);
  ESint32 EDMAPROC edma_get_class_module (CLASSID,EPChar);
  ESint32 EDMAPROC edma_get_class_namespace (CLASSID,EPChar);

  ESint32 EDMAPROC edma_get_class_major_version (CLASSID);
  ESint32 EDMAPROC edma_get_class_minor_version (CLASSID);
  ESint32 EDMAPROC edma_get_class_current_version (CLASSID);

  EPChar EDMAPROC  edma_get_class_repo_dir (CLASSID cid);
  EPChar EDMAPROC  edma_get_class_repo_name (CLASSID cid);
  ESint32 EDMAPROC edma_get_class_repo_type (CLASSID cid);

  EUint32 EDMAPROC edma_get_num_reg_classes (void);
  CLASSID EDMAPROC edma_get_next_class (CLASSID); 

  CLASSID EDMAPROC edma_get_real_id (EPChar, CLASSID);
  CLASSID EDMAPROC edma_get_class_id_with_version (EPChar, ESint32, ESint32);
  CLASSID EDMAPROC edma_get_class_id (EPChar);
  EPChar EDMAPROC edma_get_idf_file_path (CLASSID cid);
  EPChar EDMAPROC edma_get_impl_file_path (CLASSID cid);



  ESint32 EDMAPROC edma_is_class_IDF_parser (CLASSID id);
  ESint32 EDMAPROC edma_is_class_SIU_proxy (CLASSID id);
  ESint32 EDMAPROC edma_is_class_EMI_handler (CLASSID id); 
  ESint32 EDMAPROC edma_get_class_num_superclasses (CLASSID id);
  CLASSID EDMAPROC edma_get_class_superclass (CLASSID id, ESint32 Indx);
  CLASSID EDMAPROC edma_get_class_superclass (CLASSID id, ESint32 Indx);

  ESint32 EDMAPROC edma_get_prop_num (CLASSID);
  ESint32 EDMAPROC edma_get_prop_name (CLASSID,EUint32,EPChar);
  ESint32 EDMAPROC edma_get_prop_indx (CLASSID ,EPChar);
  ESint32 EDMAPROC edma_get_prop_type (CLASSID,EUint32,EPChar);
  
  ESint32 EDMAPROC edma_get_met_num (CLASSID);
  ESint32 EDMAPROC edma_get_met_name (CLASSID,EUint32,EPChar);
  ESint32 EDMAPROC edma_get_met_sig (CLASSID,EUint32,EPChar);
  ESint32 EDMAPROC edma_is_met_virtual (CLASSID,EUint32);
  ESint32 EDMAPROC edma_is_met_abstract (CLASSID,EUint32);
  ESint32 EDMAPROC edma_is_met_static (CLASSID,EUint32);
  ESint32 EDMAPROC edma_get_met_indx (CLASSID ,EPChar);
  PPROC*  EDMAPROC edma_get_met_func (CLASSID ,EPChar);
  ESint32 EDMAPROC edma_set_met_func (CLASSID ,EPChar, PPROC*);
  ESint32 EDMAPROC edma_get_all_met_func (OBJID IdObj, PPROC **list);
  
 
  /* Shared Memory Primitives */
  HMEM          EDMAPROC edma_salloc (EUint32,EPChar);
  void          EDMAPROC edma_sfree (HMEM,EPVoid);
  EPVoid        EDMAPROC edma_sget (HMEM);
  EPVoid        EDMAPROC edma_sunget (EPVoid);

  /* Private Memory Primitives */
  HMEM         EDMAPROC edma_palloc (EUint32);
  HMEM         EDMAPROC edma_prealloc (HMEM,EUint32);
  EPVoid       EDMAPROC edma_pget (HMEM);
  void         EDMAPROC edma_pfree (HMEM,EPVoid);

  EUint32 EDMAPROC edma_get_so_id (EPChar);
  EUint32 EDMAPROC edma_get_arch_id (EPChar);
  EUint32 EDMAPROC edma_get_so_name (EUint32,EPChar*);
  EUint32 EDMAPROC edma_get_arch_name (EUint32,EPChar*);
  EUint32 EDMAPROC edma_get_so_num (void);
  EUint32 EDMAPROC edma_get_arch_num (void);

  OBJID   EDMAPROC edma_new_obj (EPChar,...);
  OBJID   EDMAPROC edma_new_obj_with_version (EPChar,ESint32,ESint32,...);
  OBJID EDMAPROC edma_new_simple_obj (EPChar, EPVoid);
  EUint32 EDMAPROC edma_free_obj (OBJID);
  ESint32 EDMAPROC edma_obj_commit_suicide (OBJID id);

  ESint32 EDMAPROC edma_swap_obj (OBJID, OBJID);
  ESint32 EDMAPROC edma_attach_proxy (OBJID IdObj, EPChar proxy);
  ESint32 EDMAPROC edma_deattach_proxy (OBJID IdObj);

  EDWord  EDMAPROC edma_make_obj_virtual (OBJID );
  EUint32 EDMAPROC edma_set_obj_final (OBJID,EUint32);

  /* Object information retriving */
  POBJ    EDMAPROC edma_get_obj_pobj  (OBJID Id);
  OBJID	  EDMAPROC edma_get_obj_father_id (OBJID);
  OBJID   EDMAPROC edma_get_obj_pseudofather_id (OBJID);
  EPChar  EDMAPROC edma_get_obj_class_name (OBJID ,EPChar *);
  CLASSID EDMAPROC edma_get_obj_class_id (OBJID);
  
  ESint32 EDMAPROC edma_get_obj_status (OBJID);   
  EUint32 EDMAPROC edma_get_num_objects (void);
  ESint32 EDMAPROC edma_get_obj_num_superobjects (OBJID);
  OBJID   EDMAPROC edma_get_obj_superobject (OBJID, ESint32);
  ESint32 EDMAPROC edma_get_obj_superobject_ap (OBJID Id, ESint32 i, EPChar ap);
  ESint32 EDMAPROC edma_get_obj_num_subobjects (OBJID);
  OBJID   EDMAPROC edma_get_obj_subobject (OBJID, ESint32);
  ESint32 EDMAPROC edma_get_obj_subobject_ap (OBJID Id, ESint32 i, EPChar ap);
  OBJID   EDMAPROC edma_get_obj_app (OBJID);
  EUint32 EDMAPROC edma_get_app_id (void);
  ESint32 EDMAPROC edma_obj_report (OBJID);
  
  /* Level 1 primitives */
  EPVoid  EDMAPROC edma_wprop1 (OBJID,EUint32,...);
  EPVoid  EDMAPROC edma_rprop1 (OBJID,EUint32,...);
  EUint32 EDMAPROC edma_met1 (OBJID,EUint32,EByte,EPVoid);

  EPVoid  EDMAPROC _edma_wprop1_pargs (OBJID,EUint32,EPVoid);
  EPVoid  EDMAPROC _edma_rprop1_pargs (OBJID,EUint32,EPVoid);
  EUint32 EDMAPROC _edma_met1_pargs (OBJID,EUint32,EByte,EPVoid);

  EUint32 EDMAPROC edma_prop1_size (OBJID IdObj,EUint32 Ind) ;

  /* Dynamic Inheritance */
  ESint32 EDMAPROC edma_derive_class (EPChar,EPChar*,EPChar *,EPChar *);

  ESint32 EDMAPROC edma_add_superclasses_obj (POBJ,EPChar*,EPChar*);
  ESint32 EDMAPROC edma_over_superclasses_obj (POBJ,EUint32,EPChar,EPChar);
  ESint32 EDMAPROC edma_mutate_obj (POBJ,EPChar*,EPChar*);

  ESint32 EDMAPROC edma_merge_superclass_obj (OBJID,EPChar,OBJID);
  ESint32 EDMAPROC edma_free_superclass_obj (OBJID,EPChar);

  ESint32 EDMAPROC edma_add_subclasses_obj (POBJ,EPChar*,EPChar*);
  ESint32 EDMAPROC edma_over_subclasses_obj (POBJ,EUint32,EPChar,EPChar);
  ESint32 EDMAPROC edma_mutate_obj1 (POBJ,EPChar*,EPChar*);
  ESint32 EDMAPROC edma_merge_subclass_obj (OBJID,EPChar,OBJID);
  ESint32 EDMAPROC edma_free_subclass_obj (OBJID,EPChar);

  /* Level 3 Primitives */  
  EPVoid  EDMAPROC edma_wprop3_old (OBJID,EPChar,...);
  EPVoid  EDMAPROC edma_rprop3_old (OBJID,EPChar,...);
  EPVoid  EDMAPROC edma_met3s_old (OBJID,EPChar,EPChar,...);
  EPVoid  EDMAPROC edma_met3_old  (OBJID,EPChar,...);
  EPVoid  EDMAPROC edma_get_data_ref (OBJID);

  /* Virtual Methods */
  EUint32 EDMAPROC edma_over_met (OBJID,EPChar,POBJ,PPROC);
  EUint32 EDMAPROC edma_over_met3 (OBJID,EPChar,EPChar);
  EUint32 EDMAPROC edma_restore_met (OBJID,EPChar);
  EUint32 EDMAPROC edma_old_met3 (OBJID,EPChar,...);
  
  /* Multi IDF Interface. IDF Parsers */
//CLASSID EDMAPROC edma_idf_get_class_id ();
  CLASSID EDMAPROC edma_idf_get_free_class_id (ESint32);
  ESint32 EDMAPROC edma_idf_set_general (CLASSID ,EPChar,EPChar,EPChar,EPChar);

  ESint32 EDMAPROC edma_idf_set_class_name (CLASSID, EPChar);
  ESint32 EDMAPROC edma_idf_set_class_namespace (CLASSID, EPChar);
  ESint32 EDMAPROC edma_idf_set_class_version (CLASSID, ESint32, ESint32);
  ESint32 EDMAPROC edma_idf_set_class_attribs (CLASSID, ESint32, ESint32, ESint32);
  ESint32 EDMAPROC edma_idf_set_class_arch (CLASSID, ESint32, ESint32);
  ESint32 EDMAPROC edma_idf_set_class_impl (CLASSID, EPChar);


  ESint32 EDMAPROC edma_idf_set_def (CLASSID,EUint32,EUint32,EUint32);
  ESint32 EDMAPROC edma_idf_set_prop (CLASSID,EUint32,EPChar,EPChar,EPChar,EUint32, EPChar);
  ESint32 EDMAPROC edma_idf_set_met (CLASSID iC,EUint32 iP,EPChar ,EPChar,EByte,EByte,EByte);
  ESint32 EDMAPROC edma_idf_set_sclist (CLASSID ,EPChar	*);
  ESint32 EDMAPROC edma_idf_set_class_id (CLASSID );
  ESint32 EDMAPROC edma_idf_set_class_id1 (CLASSID );

  /* Stock Class management */
  ESint32 EDMAPROC edma_add_stock_class (CLASS_INFO ,EPChar,EPChar);
  ESint32 EDMAPROC edma_add_stock_class2 (EDMA_REPO, CLASS_INFO ,EPChar,EPChar);
  ESint32 EDMAPROC edma_del_stock_class (EPChar);
  ESint32 EDMAPROC edma_del_stock_class_id (CLASSID);

  /* EMI Extensions */
  ESint32 EDMAPROC edma_get_EMI_handler (EPChar);
  ESint32 EDMAPROC edma_add_EMI_handler (EPChar,EPChar);
  ESint32 EDMAPROC edma_hook_get_class (CLASSID);

  /* Miscelanea */
  EUint32 EDMAPROC edma_set_app_name (EPChar); 
  EUint32 EDMAPROC edma_set_debug_level (EUint32 l);
  EPChar  EDMAPROC edma_get_system_path ();

  EUint32 EDMAPROC edma_print (EPChar);
  EUint32 EDMAPROC edma_printf_obj (OBJID IdObj,EPChar f,...);
  EUint32 EDMAPROC edma_printf (EPChar f,...);
  EUint32 EDMAPROC edma_printf_err (EPChar f,...);
  EUint32 EDMAPROC edma_printf_dbg (EUint32 l,OBJID IdObj,EPChar f,...);
  
  /* EDMA Buffer type management */
  ESint32 EDMAPROC edma_buffer_alloc (EDMAT_BUFFER *,EUint32);
  ESint32 EDMAPROC edma_buffer_free (EDMAT_BUFFER*);   
  ESint32 EDMAPROC edma_buffer_realloc (EDMAT_BUFFER*,EUint32);

  /* Level 3 Extended Primitives */
  ESint32 EDMAPROC edma_wprop3_pargs (OBJID,EPChar,EPVoid);
  ESint32 EDMAPROC edma_rprop3_pargs (OBJID,EPChar,EPVoid);
//ESint32 EDMAPROC edma_met3s_pargs (OBJID,EPChar,EPChar,ESint32, EPVoid);  
//ESint32 EDMAPROC edma_met3_pargs (OBJID,EPChar,ESint32, EPVoid);
  ESint32 EDMAPROC edma_met3_pargs (OBJID,EPChar,EPChar, ESint32, EPVoid);
  ESint32 EDMAPROC edma_prop3_size (OBJID IdObj, EPChar Id1);

  /* Aditional Properties query functions */
  ESint32 EDMAPROC edma_get_prop_type_id (CLASSID,EUint32);
  ESint32 EDMAPROC edma_get_prop_type_sig (CLASSID,EUint32,EPChar);
  ESint32 EDMAPROC edma_get_prop_num_elements (CLASSID,EUint32);
  ESint32 EDMAPROC edma_get_type_size (EUint32);
  EUint32 EDMAPROC edma_get_type_sig (EUint32,EPChar);  
  EUint32 EDMAPROC edma_get_type_id (EPChar);
  ESint32 EDMAPROC edma_get_prop_indx3 (OBJID,EPChar,CLASSID*); 

  /* Class Interface Management functions*/
  ESint32 EDMAPROC edma_load_class_int (CLASSID);
  ESint32 EDMAPROC edma_unload_class_int (CLASSID);

  /* Ini file management functions*/
  
  PINIFILE EDMAPROC edma_open_ini (EPChar);
  ESint32  EDMAPROC edma_close_ini (PINIFILE);
  ESint32  EDMAPROC edma_clean_ini_string (EPChar);
  ESint32  EDMAPROC edma_get_ini_int (PINIFILE,EPChar,EPChar,EUint32);
  EUint32  EDMAPROC edma_get_ini_string (PINIFILE,EPChar,EPChar,EPChar,EPChar,EUint32);

  /* Static Methods*/
  EPVoid EDMAPROC edma_smet3s (EPChar,EPChar,EPChar,...);
  EPVoid EDMAPROC edma_smet3 (EPChar,EPChar,...);
  EPVoid EDMAPROC edma_smet3sx (CLASSID,EPChar,EPChar,EPVoid);
  EPVoid EDMAPROC edma_smet3x (CLASSID,EPChar,EPVoid);

  /* Casting primitives */
  OBJID EDMAPROC edma_upcast_obj (OBJID, EPChar);
  OBJID EDMAPROC edma_downcast_obj (OBJID, EPChar);
  OBJID EDMAPROC edma_cast_obj (OBJID, EPChar);
//OBJID EDMAPROC edma_newcast_obj (OBJID, EPChar);

  /* Clonning primitives*/
  OBJID EDMAPROC edma_clone_obj(OBJID);
  OBJID EDMAPROC edma_shallow_clone_obj(OBJID);

  ESint32 EDMAPROC edma_ingridf_get_num_parsers (void);
  CLASSID EDMAPROC edma_ingridf_get_parser_class (ESint32 i);

  ESint32 EDMAPROC edma_siu_get_num_proxies (void);
  CLASSID EDMAPROC edma_siu_get_proxy_class (ESint32);

  ESint32 EDMAPROC edma_get_EMI_num_handlers (void);
  CLASSID EDMAPROC edma_get_EMI_handler_class (ESint32 i);

  ESint32 EDMAPROC  edma_rename_superclass_ap (OBJID IdObj
						  ,EPChar OldName
						  ,EPChar NewName) ;
  ESint32 EDMAPROC  edma_rename_subclass_ap (OBJID IdObj
						,EPChar OldName
						,EPChar NewName);
  OBJID EDMAPROC edma_apply_classpath (OBJID IdObj,EPChar eclass_path);
  OBJID EDMAPROC edma_parse_classpath (OBJID IdObj,EPChar eclass_path, ESint32 len);

  ESint32 EDMAPROC edma_look4_met_ext (POBJ *pObj1,EPChar MetName, EPChar Signature);
  ESint32 EDMAPROC edma_look4_prop_ext (POBJ *pObj1,EPChar PropName);
  ESint32 EDMAPROC edma_show_subobjects_up (OBJID IdObj,EPChar Id, ESint32 level);
  ESint32 EDMAPROC edma_show_subobjects_down (OBJID IdObj,EPChar Id, ESint32 level);
  ESint32 EDMAPROC edma_show_object_interface (OBJID IdObj);

  ESint32 EDMAPROC edma_look4_prop_ext (POBJ *pObj1,EPChar PropName);
  ESint32 EDMAPROC edma_met3 (OBJID IdObj, EPChar Id1,...);
  ESint32 EDMAPROC edma_met3s (OBJID IdObj, EPChar Id1, EPChar Sig,...);
  ESint32 EDMAPROC edma_wprop3 (OBJID IdObj, EPChar Id1,...);
  ESint32 EDMAPROC edma_rprop3 (OBJID IdObj, EPChar Id1,...);

  ESint32 EDMAPROC edma_set_prop_sint32 (OBJID IdObj, EPChar Id1, ESint32 val);
  ESint32 EDMAPROC edma_set_prop_uint32 (OBJID IdObj, EPChar Id1, EUint32 val);
  ESint32 EDMAPROC edma_set_prop_sint16 (OBJID IdObj, EPChar Id1, ESint16 val);
  ESint32 EDMAPROC edma_set_prop_uint16 (OBJID IdObj, EPChar Id1, EUint16 val);
  ESint32 EDMAPROC edma_set_prop_sint8  (OBJID IdObj, EPChar Id1, ESint8 val);
  ESint32 EDMAPROC edma_set_prop_uint8  (OBJID IdObj, EPChar Id1, EUint8 val);
  ESint32 EDMAPROC edma_set_prop_strz   (OBJID IdObj, EPChar Id1, EPChar val);
  ESint32 EDMAPROC edma_set_prop_obj    (OBJID IdObj, EPChar Id1, OBJID val);

  ESint32 EDMAPROC edma_get_prop_sint32 (OBJID IdObj, EPChar Id1);
  EUint32 EDMAPROC edma_get_prop_uint32 (OBJID IdObj, EPChar Id1);
  ESint16 EDMAPROC edma_get_prop_sint16 (OBJID IdObj, EPChar Id1);
  EUint16 EDMAPROC edma_get_prop_uint16 (OBJID IdObj, EPChar Id1);
  ESint8 EDMAPROC  edma_get_prop_sint8  (OBJID IdObj, EPChar Id1);
  EUint8 EDMAPROC  edma_get_prop_uint8  (OBJID IdObj, EPChar Id1);
  EPChar EDMAPROC edma_get_prop_strz (OBJID IdObj, EPChar Id1, EPChar val);
  EDMAT_BUFFER *EDMAPROC edma_get_prop_buffer (OBJID IdObj, EPChar Id1, 
					       EDMAT_BUFFER *buf);
  OBJID EDMAPROC edma_get_prop_obj (OBJID IdObj, EPChar Id1);

  EUint32 EDMAPROC EDMAEnd(void);   
  EUint32 EDMAPROC EDMAInit(void);   

  ESint32 EDMAPROC  edma_add_std_repo (EPChar repo_name);
  ESint32 EDMAPROC   edma_add_app_repos ();

  EPChar  EDMAPROC edma_get_repo_dir (CLASSID cid);
  ESint32 EDMAPROC edma_get_repo_type (CLASSID cid);
  ESint32 EDMAPROC edma_get_repo_name (CLASSID cid);
  ESint32 EDMAPROC edma_load_registry (EPChar, EPChar);
  
  ESint32 EDMAPROC edma_error (jmp_buf*);
  ESint32 EDMAPROC edma_exception_try ();
  ESint32 EDMAPROC edma_exception_throw (OBJID); 
  ESint32 EDMAPROC edma_exception_clean (OBJID id);


  OBJID EDMAPROC edma_add_superclass (OBJID id, CLASSID cid, 
				      EPChar apoint1, EPChar apoint2);
  ESint32 EDMAPROC edma_add_superobject (OBJID id, OBJID subid, 
					 EPChar apoint);
  ESint32 EDMAPROC edma_add_subobject (OBJID id, OBJID subid, 
				       EPChar apoint);
  OBJID EDMAPROC edma_add_subclass (OBJID id, CLASSID cid, 
				    EPChar apoint1, EPChar apoint2);

  OBJID EDMAPROC edma_insert_superclass (OBJID id, CLASSID cid, 
    EPChar apoint1, EPChar apoint2);
  OBJID EDMAPROC edma_insert_superobject (OBJID id, OBJID superobj, 
					 EPChar apoint1, EPChar apoint2);
  OBJID EDMAPROC edma_insert_subclass (OBJID id, CLASSID cid, 
				       EPChar apoint1, EPChar apoint2);
  OBJID EDMAPROC edma_insert_subobject (OBJID id, OBJID cid, 
				       EPChar apoint1, EPChar apoint2);

  ESint32 EDMAPROC
  edma_remove_superclass_ap (OBJID IdObj, EPChar anchor_point);

  ESint32 EDMAPROC
  edma_remove_subclass_ap (OBJID IdObj, EPChar anchor_point);


  OBJID EDMAPROC edma_set_sub_ap (OBJID id, EPChar apoint, OBJID new_id);
  OBJID EDMAPROC edma_set_super_ap (OBJID id, EPChar apoint, OBJID new_id);




  ESint32 EDMAPROC edma_actualize_cfg ();

  /* LOCAL Class manipulation primitives*/
//CLASSID EDMAPROC edma_get_local_class_id ();

#if 0
  ESint32 EDMAPROC edma_set_local_class_name (CLASSID cid, EPChar classname);
  ESint32 EDMAPROC edma_set_local_class_namespace (CLASSID cid, EPChar name_space);
  ESint32 EDMAPROC edma_set_local_class_version (CLASSID cid, ESint32 major, ESint32 minor);
  ESint32 EDMAPROC edma_set_local_class_attribs (CLASSID cid, 
						 ESint32 is_siu, ESint32 is_idf, ESint32 is_emi);
#endif
  ESint32 EDMAPROC edma_add_local_class_property (CLASSID cid, EPChar name, EUint32 type, 
						  ESint32 access, EUint32 nelems);
  ESint32 EDMAPROC edma_add_local_class_method (CLASSID cid, EPChar name, EPChar sig, 
						PPROC f, 
						ESint32 mvirtual, 
						ESint32 mstatic, 
						ESint32 mabstract);
  ESint32 EDMAPROC edma_add_local_class_superclass (CLASSID cid, CLASSID supercid, EPChar pap, EPChar pap1);
  ESint32 EDMAPROC edma_add_local_class_superclass_by_name (CLASSID cid, EPChar classname, EPChar pap, EPChar pap1);

  ESint32 EDMAPROC edma_local_class_finish (CLASSID cid);

/* Repositories */
  EDMA_REPO EDMAPROC   edma_repo_new (ESint32 type, EPChar base_dir, EPChar fname);
  ESint32   EDMAPROC   edma_repo_free (EDMA_REPO repo);
  ESint32   EDMAPROC   edma_repo_load (EDMA_REPO repo);
  ESint32   EDMAPROC   edma_repo_save (EDMA_REPO repo);

  /* Accessors */
  ESint32   EDMAPROC   edma_repo_set_type (EDMA_REPO repo, ESint32 type);
  ESint32   EDMAPROC   edma_repo_set_base_dir (EDMA_REPO repo, EPChar base_dir);
  ESint32   EDMAPROC   edma_repo_set_file (EDMA_REPO repo, EPChar fname);

  ESint32   EDMAPROC   edma_repo_get_type (EDMA_REPO repo);
  EPChar    EDMAPROC   edma_repo_get_base_dir (EDMA_REPO repo);
  EPChar    EDMAPROC   edma_repo_get_file (EDMA_REPO repo);
  ESint32   EDMAPROC   edma_repo_get_id (EDMA_REPO repo);
  ESint32   EDMAPROC   edma_repo_add_class (EDMA_REPO repo);
  ESint32   EDMAPROC   edma_repo_del_class (EDMA_REPO repo);

  ESint32 EDMAPROC edma_load_registry (EPChar, EPChar);
  ESint32 _edma_load_registry (EDMA_REPO repo);


  /* Repository Manager */
  ESint32    EDMAPROC  edma_repo_manager_add_repo (EDMA_REPO);
  ESint32    EDMAPROC  edma_repo_manager_del_repo (EPChar);
  EDMA_REPO  EDMAPROC  edma_repo_manager_get_repo (ESint32);
  EDMA_REPO  EDMAPROC  edma_repo_manager_get_repo_by_name (EPChar, EPChar);
  EPChar     EDMAPROC  edma_repo_manager_get_repo_dir (ESint32);
  ESint32    EDMAPROC  edma_repo_manager_get_repo_type (ESint32);
  ESint32    EDMAPROC  edma_repo_manager_num_repos ();


/* THreads */

ESint32 EDMAPROC edma_thread_register (void);
ESint32 EDMAPROC edma_thread_unregister (void);
ESint32 EDMAPROC edma_thread_list (void);
/* Thread Functions*/
ESint32 EDMAPROC edma_thread_create (ETHREAD *et, EPROC func, ETHREAD_PARAMS par);
ETHREAD EDMAPROC edma_thread_self ();
/* TO REMOVE*/
/*ESint32 EDMAPROC edma_thread_registry (ETHREAD et);*/


/* Mutex Functions */
ESint32 EDMAPROC edma_mutex_create (EMUTEX emux);
ESint32 EDMAPROC edma_mutex_destroy (EMUTEX emux);
ESint32 EDMAPROC edma_mutex_lock (EMUTEX emux);
ESint32 EDMAPROC edma_mutex_unlock (EMUTEX emux);

/* Conditional Variables FUnctions*/
ESint32 EDMAPROC edma_cond_create (ECOND econd);
ESint32 EDMAPROC edma_cond_destroy (ECOND econd);
ESint32 EDMAPROC edma_cond_signal (ECOND econd);
ESint32 EDMAPROC edma_cond_broadcast (ECOND econd);
ESint32 EDMAPROC edma_cond_wait (ECOND econd, EMUTEX emux);

/* Thread Local Storage Functions */
ESint32 EDMAPROC edma_thread_key_create (ETKEY *etls);
ESint32 EDMAPROC edma_thread_key_destroy (ETKEY etls);
ESint32 EDMAPROC edma_tsd_set_data (ETKEY etls, EPVoid dat);
EPVoid EDMAPROC edma_tsd_get_data (ETKEY etls);



#ifdef __cplusplus
}
#endif

#endif
