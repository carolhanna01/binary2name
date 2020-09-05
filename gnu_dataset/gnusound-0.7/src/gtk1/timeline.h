/* GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
#ifndef __TIMELINE_H__
#define __TIMELINE_H__


#include <gdk/gdk.h>
#include <gtk/gtkadjustment.h>
#include <gtk/gtkwidget.h>


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


#define TIMELINE(obj)          GTK_CHECK_CAST (obj, timeline_get_type (), Timeline)
#define TIMELINE_CLASS(klass)  GTK_CHECK_CLASS_CAST (klass, timeline_get_type (), TimelineClass)
#define IS_TIMELINE(obj)       GTK_CHECK_TYPE (obj, timeline_get_type ())


typedef struct _Timeline        Timeline;
typedef struct _TimelineClass   TimelineClass;

typedef void (*TimelineLabelGenerator)(gdouble value, char *s, size_t n, void *label_generator_data);

struct _Timeline
{
    GtkWidget widget;

    GdkPixmap *backing_store;

    gdouble units_per_pixel;
    gdouble lower;
    gdouble upper;
    gdouble mark_interval;
    gdouble mark_factor;
    gdouble label_interval;
    gdouble label_factor;

    TimelineLabelGenerator label_generator;
    void *label_generator_data;

};

struct _TimelineClass
{
  GtkWidgetClass parent_class;
};


GtkWidget*     timeline_new                    ();
guint          timeline_get_type               (void);
void           timeline_configure              (Timeline *timeline,
                                                gdouble units_per_pixel,
                                                gdouble lower,
                                                gdouble upper,
                                                gdouble mark_interval,
                                                gdouble mark_factor,
                                                gdouble label_interval,
                                                gdouble label_factor);
void           timeline_set_label_generator    (Timeline *timeline, 
                                                TimelineLabelGenerator generator, 
                                                void *label_generator_data);

#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* __TIMELINE_H__ */
