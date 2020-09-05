/* -*-c-*- ---------------- mixgtk_config.h :
 * Configuration functions declarations.
 * ------------------------------------------------------------------
 *  $Id: mixgtk_config.h,v 1.6 2004/06/30 14:07:53 jao Exp $
 * ------------------------------------------------------------------
 * Copyright (C) 2001, 2004 Free Software Foundation, Inc.
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


#ifndef MIXGTK_CONFIG_H
#define MIXGTK_CONFIG_H

#include <mixlib/mix.h>
#include <mixlib/mix_config.h>

/* load configuration */
extern gboolean
mixgtk_config_load (void);

/* get mix config */
extern mix_config_t *
mixgtk_config_get_mix_config (void);

/* autosave state */
extern gboolean
mixgtk_config_is_autosave (void);

extern void
mixgtk_config_set_autosave (gboolean autosave);

/* update config item */
extern void
mixgtk_config_update (const gchar *key, const gchar *value);

/* get config item */
extern const gchar *
mixgtk_config_get (const gchar *key);

/* remove config item */
extern void
mixgtk_config_remove (const gchar *key);

/* save configuration */
extern void
mixgtk_config_save (void);

/* shared config params */
extern gboolean
mixgtk_config_show_toolbars (void);

extern void
mixgtk_config_set_show_toolbars (gboolean show);

#endif /* MIXGTK_CONFIG_H */

