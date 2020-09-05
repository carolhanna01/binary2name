/*  GCompris -- This files comes from XMMS
 *
 *  XMMS - Cross-platform multimedia player
 *  Copyright (C) 1998-2000  Peter Alm, Mikael Alm, Olle Hallnas, Thomas Nilsson and 4Front Technologies
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */
#ifndef BOARD_H
#define BOARD_H

struct BoardPluginData
{
  GList		*board_list;
  BoardPlugin	*current_board_plugin;
  GcomprisBoard	*current_gcompris_board;
  gboolean	 playing;
  gboolean	 paused;
};

GList		*get_board_list(void);
BoardPlugin	*get_current_board_plugin(void);
void		 set_current_board_plugin(BoardPlugin * ip);
GcomprisBoard	*get_current_gcompris_board(void);
void		 set_current_gcompris_board(GcomprisBoard * gcomprisBoard);
gboolean	 board_check_file(GcomprisBoard *gcomprisBoard);
void		 board_play(GcomprisBoard *gcomprisBoard);
void		 board_stop(void);
void		 board_pause(void);
gboolean	 get_board_playing(void);
gboolean	 get_board_paused(void);
gchar		*board_get_info_text(void);
void		 board_about(gint index);
void		 board_configure(gint index);

void		 board_set_info_text(gchar * text);
void		 board_file_info_box(gchar * filename);

#endif
