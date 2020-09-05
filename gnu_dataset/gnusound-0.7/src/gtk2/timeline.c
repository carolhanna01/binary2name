/*
 * GTK timeline widget -- a part of GNUsound.
 * Copyright (C) 2004  Pascal Haakmat
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
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <math.h>
#include <stdio.h>
#include <gtk/gtk.h>
#include "timeline.h"

/* Forward declarations */

static void timeline_class_init               (gpointer         g_class,
                                               gpointer         class_data);
static void timeline_init                     (GTypeInstance    *instance,
                                               gpointer         g_class);
static void timeline_realize                  (GtkWidget        *widget);
static void timeline_size_request             (GtkWidget        *widget,
                                               GtkRequisition   *requisition);
static void timeline_size_allocate            (GtkWidget        *widget,
                                               GtkAllocation    *allocation);
static gint timeline_expose                   (GtkWidget        *widget,
                                               GdkEventExpose   *event);

/* Local data */

GType
timeline_get_type ()
{
    static GType timeline_type = 0;
    
    if (!timeline_type) {
        static const GTypeInfo timeline_info = {
            sizeof (TimelineClass),
            NULL, /* base_init */
            NULL, /* base_finalize */
            timeline_class_init,
            NULL, /* class_finalize */
            NULL, /* class_data */
            sizeof(Timeline),
            0,
            timeline_init
        };

        timeline_type = g_type_register_static (GTK_TYPE_WIDGET,
                                                "Timeline", &timeline_info, 0);
    }

    return timeline_type;
}

static void
timeline_class_init (gpointer g_class,
                     gpointer class_data) 
{
    GObjectClass *object_class;
    GtkWidgetClass *widget_class;
    
    object_class = (GObjectClass*) g_class;
    widget_class = (GtkWidgetClass*) g_class;
    
    widget_class->realize = timeline_realize;
    widget_class->expose_event = timeline_expose;
    widget_class->size_request = timeline_size_request;
    widget_class->size_allocate = timeline_size_allocate;
}

static void 
timeline_init(GTypeInstance    *instance,
              gpointer         g_class)
{
    Timeline *timeline = TIMELINE (instance);

    timeline->units_per_pixel = 0;
    timeline->lower = 0;
    timeline->upper = 0;
    timeline->label_interval = 0;
    timeline->label_factor = 0;
    timeline->mark_interval = 0;
    timeline->mark_factor = 0;
}

GtkWidget *
timeline_new ()
{
    GObject *self;

    self = g_object_new (timeline_get_type (), NULL);

    return GTK_WIDGET (self);
}

static void
timeline_realize (GtkWidget *widget)
{
    Timeline *timeline;
    GdkWindowAttr attributes;
    gint attributes_mask;

    g_return_if_fail (widget != NULL);
    g_return_if_fail (IS_TIMELINE (widget));

    GTK_WIDGET_SET_FLAGS (widget, GTK_REALIZED);
    timeline = TIMELINE (widget);

    attributes.x = widget->allocation.x;
    attributes.y = widget->allocation.y;
    attributes.width = widget->allocation.width;
    attributes.height = widget->allocation.height;
    attributes.wclass = GDK_INPUT_OUTPUT;
    attributes.window_type = GDK_WINDOW_CHILD;
    attributes.event_mask = gtk_widget_get_events (widget) | 
        GDK_EXPOSURE_MASK;
    attributes.visual = gtk_widget_get_visual (widget);
    attributes.colormap = gtk_widget_get_colormap (widget);

    attributes_mask = GDK_WA_X | GDK_WA_Y | GDK_WA_VISUAL | GDK_WA_COLORMAP;
    widget->window = gdk_window_new (widget->parent->window, &attributes, attributes_mask);

    widget->style = gtk_style_attach (widget->style, widget->window);

    gdk_window_set_user_data (widget->window, widget);

    gtk_style_set_background (widget->style, widget->window, GTK_STATE_ACTIVE);
}

static void 
timeline_size_request (GtkWidget      *widget,
		       GtkRequisition *requisition)
{
    Timeline *timeline;
    PangoLayout *layout;
    PangoRectangle ink_rect, logical_rect;
    char label[20];

    g_return_if_fail (widget != NULL);
    g_return_if_fail (IS_TIMELINE (widget));
    g_return_if_fail (requisition != NULL);

    timeline = TIMELINE (widget);

    g_return_if_fail (timeline->label_generator);

    timeline->label_generator (0,
                               label, sizeof label,
                               timeline->label_generator_data);
    
    layout = gtk_widget_create_pango_layout (widget, label);    
    pango_layout_get_pixel_extents (layout, &ink_rect, &logical_rect);
    g_object_unref (G_OBJECT (layout));

    requisition->height = logical_rect.height + (logical_rect.height / 3);
}

static void
timeline_size_allocate (GtkWidget     *widget,
			GtkAllocation *allocation)
{
    Timeline *timeline;
    
    g_return_if_fail (widget != NULL);
    g_return_if_fail (IS_TIMELINE (widget));
    g_return_if_fail (allocation != NULL);
    
    widget->allocation = *allocation;
    timeline = TIMELINE (widget);
    
    if (GTK_WIDGET_REALIZED (widget)) {
        
        gdk_window_move_resize (widget->window,
                                allocation->x, allocation->y,
                                allocation->width, allocation->height);
        
    }
}

static gdouble 
timeline_calc_step( Timeline *timeline,
                    gdouble   width,
                    gdouble   interval,
                    gdouble   factor)
{
    int x = 0;
    
    while (interval * MAX (1, factor * x) < width * timeline->units_per_pixel)
        x++;
    
    return interval * MAX (1, factor * x);
}

static gint
timeline_expose (GtkWidget      *widget,
		 GdkEventExpose *event)
{
    Timeline *timeline;
    PangoLayout *layout;
    PangoRectangle ink_rect, logical_rect;
    gdouble lower, upper, pos, step, markpos, markstep;
    char label[20];
    gint x, odd_mark = 0, height;

    g_return_val_if_fail (widget != NULL, FALSE);
    g_return_val_if_fail (IS_TIMELINE (widget), FALSE);
    g_return_val_if_fail (event != NULL, FALSE);

    timeline = TIMELINE (widget);

    g_return_val_if_fail (timeline->label_generator, FALSE);
    g_return_val_if_fail (timeline->units_per_pixel != 0, FALSE);

    gtk_paint_box (widget->style, widget->window,
                   widget->state, GTK_SHADOW_OUT, 
                   NULL, widget, "timeline",
                   0, 0, 
                   widget->allocation.width, widget->allocation.height);

    lower = timeline->lower;
    upper = timeline->upper;

    markstep = timeline_calc_step (timeline, 
                                   2,
                                   timeline->mark_interval,
                                   timeline->mark_factor);

    /* Labels. */
  
    timeline->label_generator (upper,
                               label, sizeof label,
                               timeline->label_generator_data);
    
    layout = gtk_widget_create_pango_layout (widget, label);
    
    pango_layout_get_pixel_extents (layout, &ink_rect, &logical_rect);

    step = timeline_calc_step (timeline,
                               logical_rect.width + 4,
                               timeline->label_interval,
                               timeline->label_factor);

    for (pos = lower - fmod (lower, step); pos < upper; pos += step) {

        x = (pos - lower) / timeline->units_per_pixel;

        timeline->label_generator (pos,
                                   label, sizeof label,
                                   timeline->label_generator_data);
        
        pango_layout_set_text (layout, label, -1);
        pango_layout_get_pixel_extents (layout, &ink_rect, &logical_rect);
        
        gdk_draw_layout (widget->window,
                         widget->style->fg_gc[widget->state],
                         x + logical_rect.x + 2,
                         0,
                         layout);
        gdk_draw_line (widget->window, widget->style->fg_gc[widget->state],
                       x, widget->allocation.height, 
                       x, widget->allocation.height / 3);

        /* Marks */

        for (markpos = pos; markpos < pos + step; markpos += markstep) {
            
            x = (markpos - lower) / timeline->units_per_pixel;
            height = widget->allocation.height / (odd_mark++ % 2 == 0 ? 4 : 6);
            gdk_draw_line (widget->window, widget->style->fg_gc[widget->state],
                           x, widget->allocation.height, 
                           x, widget->allocation.height - height);
                           //                               (widget->allocation.height / 4)));
            
        }

    }

    g_object_unref (G_OBJECT (layout));
  
    return FALSE;
}

/**
 * Configures the Timeline display parameters. The Timeline will
 * attempt to place marks and labels every "interval" units. If that
 * is not possible because there is not enough space, it will place
 * them every x * "factor" * "interval" units, by figuring out a value
 * for x such that no 2 marks or labels overlap.
 *
 * @param timeline The timeline.
 * @param units_per_pixel How many units are represented by 1 pixel.
 * @param lower The lower bound of the timeline.
 * @param upper The upper bound of the timeline.
 * @param mark_interval Preferred interval to place marks at.
 * @param mark_factor Factor to apply to the interval if short on space.
 * @param label_interval Preferred interval to place labels at.
 * @param label_factor Factor to apply to the interval if short on space.
 */

void
timeline_configure (Timeline *timeline,
                    gdouble   units_per_pixel,
                    gdouble   lower,
                    gdouble   upper,
                    gdouble   mark_interval,
                    gdouble   mark_factor,
                    gdouble   label_interval,
                    gdouble   label_factor) 
{
    
    g_return_if_fail (timeline != NULL);
    g_return_if_fail (IS_TIMELINE (timeline));
    
    timeline->units_per_pixel = units_per_pixel;
    timeline->lower = lower;
    timeline->upper = upper;
    timeline->mark_interval = mark_interval;
    timeline->mark_factor = mark_factor;
    timeline->label_interval = label_interval;
    timeline->label_factor = label_factor;

    gtk_widget_queue_draw (GTK_WIDGET (timeline));
}

/**
 * Sets a formatting callback, used by the Timeline to the generate
 * the labels that appear in the Timeline.
 * @param timeline The timeline.
 * @param generator The callback which generates a label.
 * @param user_data Data to pass to the callback.
 */                       

void
timeline_set_label_generator(Timeline               *timeline,
                             TimelineLabelGenerator  generator,
                             void                   *user_data) 
{

    g_return_if_fail (timeline != NULL);
    g_return_if_fail (IS_TIMELINE (timeline));
    
    timeline->label_generator = generator;
    timeline->label_generator_data = user_data;
}
