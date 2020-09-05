#ifndef AUDIO_H
#define AUDIO_H

/* 
   audio.H
   include file for xlogmaster audio
*/


/*{{{  Variables  */
// size of audio buffer:
#define AUDIO_BSIZE 4096

gint sound;
char* alert_sound;
char* notice_sound;
char* uniconify_sound;

char* sound_device = "/dev/dsp";

/*}}}*/

/*{{{  Subroutines  */
void play(gint);


/*}}}*/


#endif /* AUDIO_H */
