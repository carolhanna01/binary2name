/* grecs - Gray's Extensible Configuration System
   Copyright (C) 2007-2019 Sergey Poznyakoff

   Grecs is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 3 of the License, or (at your
   option) any later version.

   Grecs is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along
   with Grecs. If not, see <http://www.gnu.org/licenses/>. */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif
#include <errno.h>
#include <grecs.h>

int
grecs_assert_value_type(const grecs_value_t *value, int type,
			grecs_locus_t *refloc)
{
	if (GRECS_VALUE_EMPTY_P(value)) {
		grecs_error(refloc, 0, _("expected %s"),
			    gettext(grecs_value_type_string(type)));
		return 1;
	}
	if (value->type != type) {
		grecs_error(&value->locus, 0, _("expected %s, but found %s"),
			    gettext(grecs_value_type_string(type)),
			    gettext(grecs_value_type_string(value->type)));
		return 1;
	}
	return 0;
}

int
grecs_assert_scalar_stmt(grecs_locus_t *locus, enum grecs_callback_command cmd)
{
	if (cmd != grecs_callback_set_value) {
		grecs_error(locus, 0, _("unexpected block statement"));
		return 1;
	}
	return 0;
}

int
grecs_assert_node_value_type(enum grecs_callback_command cmd,
			     grecs_node_t *node, int type)
{
	return grecs_assert_scalar_stmt(&node->locus, cmd)
		|| grecs_assert_value_type(node->v.value, type,
					   &node->locus);
}
