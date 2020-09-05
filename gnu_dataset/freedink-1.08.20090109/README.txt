This is GNU FreeDink, a portable and enhanced version of the Dink
Smallwood game engine.

FreeDink is free software, and you are welcome to redistribute it
under certain conditions; see the GNU GPL for details.
( cf. COPYING and http://gnu.org/licenses/gpl.html )


About Dink Smallwood
--------------------

Dink Smallwood is an adventure/role-playing game, similar to Zelda,
made by RTsoft. Besides twisted humour, it includes the actual game
editor, allowing players to create hundreds of new adventures called
Dink Modules or D-Mods for short. The Dink Network
(http://www.dinknetwork.com/) hosts a copy of almost all of them.


GNU FreeDink
------------

GNU FreeDink is a new and portable version of the game engine, which
runs the original game as well as its D-Mods, with close
compatibility, under multiple platforms.

FreeDink can run in 2 modes:

* v1.08 (default): this matches the latest release of the Dink engine,
  which includes some new features and some backward-incompatible
  changes. Use it for recent D-Mods.

* v1.07 (with option '--v1.07'): this matches v1.07 of the Dink
  engine, essentially unmodified since 1998. Use it to play D-Mods
  released before 2006.

Next step is extending the engine while preserving accurate support
for released D-Mods.

On the technical level, the internals of the engines experience a
clean-up to allow for portability and later improvements.

Check out FreeDink's website at http://www.freedink.org/ .


GNU
---

FreeDink is part of the GNU project, whose aim is to create "GNU", a
completely free (as in freedom) operating system. Its most famous
variant these days is GNU/Linux. A engine to play the Dink Smallwood
game and D-Mods is definitely a must for the GNU OS ;) Check
http://www.gnu.org/ for more information.


Building
--------

If you are compiling GNU FreeDink from sources, check BUILD for
instructions.


Game data
---------

Get the free game data separately at:

  http://www.freedink.org/releases/dink_108_game_data.zip

and unzip it in '/usr/share/dink/dink':

  mkdir -p /usr/share/dink/dink/
  unzip -d /usr/share/dink/dink/ dink_108_game_data.zip

Note:
The original game contained sounds that were not owned by RTsoft and
hence could not be released under a free license.  We're looking for
free replacements!  Please send contributions at bug-freedink@gnu.org.


Other FreeDink applications
---------------------------

DFArc2, a graphical front-end run the game and manage D-Mods, was also
ported and extended. Get new version 3 at http://www.freedink.org/ .


Technical information
---------------------

Check the 'doc' directory for technical/developer considerations.


---
Copying and distribution of this file, with or without modification,
are permitted in any medium without royalty provided the copyright
notice and this notice are preserved.
