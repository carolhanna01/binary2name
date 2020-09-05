/* -*-c-*- ---------------- mixvm_command.h :
 * Declarations for commands accepted by the mix virtual machine
 * ------------------------------------------------------------------
 * $Id: mixvm_command.h,v 1.4 2002/04/08 00:32:06 jao Exp $
 * ------------------------------------------------------------------
 * Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
 *  
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *  
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *  
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *  
 */


#ifndef MIXVM_COMMAND_H
#define MIXVM_COMMAND_H

#include <mixlib/mix_config.h>
#include <mixlib/mix_vm_command.h>

extern mix_vm_cmd_dispatcher_t *
mixvm_cmd_init (mix_config_t *config, char *arg, gboolean use_emacs);

extern gboolean
mixvm_cmd_exec (char *line);

#endif /* MIXVM_COMMAND_H */

