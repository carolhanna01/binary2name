#include <config.h>
#include "cmd.h"
#include "snd.h"

static struct cmd_value *
cmd_snd_delete(const char *name,
                struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    int map = cmd_int(args->argv[1]);
    long start = cmd_long(args->argv[2]);
    long count = cmd_long(args->argv[3]);

    snd *del_sr = snd_delete(shl->clip->sr,
                             map,
                             start, 
                             count);
    
    if(SND_HAS_ERROR(shl->clip->sr))
        goto recover_delete;
    
    history_remember(shl->history,
                     cmd_new("insert-and-set-selection",
                             cmd_new_argv(4,
                                          cmd_new_shellp_val(shl),
                                          cmd_new_sndp_val_with_dtor(del_sr, cmd_sndp_dtor),
                                          cmd_new_int_val(map),
                                          cmd_new_long_val(start))));
    
    shl->select_start = start;
    shl->select_end = start - snd_frame_count(del_sr);
    view_redraw(shl->view);
    return cmd_new_void_val();
    
 recover_delete:
    err = cmd_new_err_val("Unable to delete (%s)",
                          snd_error_get(shl->clip->sr));
    snd_error_free(shl->clip->sr);
    if(del_sr)
        snd_insert(shl->clip->sr,
                   del_sr,
                   map,
                   start);
    return err;
}

static struct cmd_value *
cmd_edit_insert_and_set_selection(const char *name,
                                  struct cmd_argv *args) {
    shell *shl = cmd_shellp(args->argv[0]);
    snd *ins_sr = cmd_sndp(args->argv[1]);
    int map = cmd_int(args->argv[2]);
    long start = cmd_long(args->argv[3]);
    long count = snd_frame_count(ins_sr);

    map = snd_insert(shl->clip->sr,
                     ins_sr,
                     map,
                     start);

    if(SND_HAS_ERROR(shl->clip->sr)) 
        goto recover_insert;

    history_remember(shl->history,
                     cmd_new("delete-and-set-selection",
                             cmd_new_argv(4,
                                          cmd_new_shellp_val(shl),
                                          cmd_new_int_val(map),
                                          cmd_new_long_val(start),
                                          cmd_new_long_val(count))));
    
    shl->select_start = start;
    shl->select_end = start + count;
    shl->select_channel_map = map;
    view_redraw(shl->view);
    return cmd_new_void_val();

 recover_insert:
    err = cmd_new_error_val("Unable to insert (%s)",
                            snd_error_get(shl->clip->sr));
    snd_error_free(shl->clip->sr);
    return err;
}
#endif
