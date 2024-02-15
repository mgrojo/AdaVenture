


# AdaVenture 
## using GLFW & OpenAL



## Recent Changes



**ver 2.4.0 -- 11feb2024**

* Improved/generalized OSX build scripts.
* Command line params clarified: 1=allowHiDpi(default); 0=forceLoDpi. 
(Use low DPI to smooth out action graphics.)
* Added waterfall from castle roof into cistern, using both particle & ribbon textures.
* Added a wandering toxic cloud in labyrinth.
* Embellished castle interior.


**ver 2.3.9 -- 21oct2023**

* Added check for adequate window size when displaying list of saved games.
* Changed sort of saved games to show most recent first.


**ver 2.3.8 -- 06oct2023**

* Restored OSX build...but without a bundle.


**ver 2.3.7 -- 17sep2023**

* Fixed a problem with the h-key toggle of intro screen/music.
* Implemented a ghost-sword on ceiling, similar to ghost-key.
* Bat now grabs dropped sword as planned, ch 1 & ch 3.
* In Ch1/Ch3 (as originally planned) if sword is dropped to pickup key, bat grabs sword instead. Otherwise it grabs the black key.


## More change-history at end of file



## AdaVenture Game Description
AdaVenture is a kid-friendly retro point & click game, a reincarnation in 3D of the original Atari game named "Adventure", with various artistic extrapolations.  The mazes have interconnections that are impossible in 3 dimensions, but are painstakingly exact reproductions of those in the original game.  In fact, the "mapRoom" subdirectory contains the original maps to help guide you.  

Runs on Windows, OSX, and GNU/Linux. Includes all source code, build scripts & resources.

-----------------------------------------------------------
Featuring

	* no installation
	* no dependencies
	* simply unzip in your Downloads directory, and run;
	* or unzip onto a USB flash drive [w/same file format] and run.
-----------------------------------------------------------

## AdaVenture Introduction
Set in ancient Persia, it begins outside the castle of the young King Xerxes, who inherited a magical golden chalice from his father, Darius the Great.  Coveted by Greek foes King Leonidas of Sparta and King Minos of Crete, the chalice has been stolen.

Your quest is to seek and return the royal chalice to its pedestal within the castle of Xerxes...a stealth mission to preclude open hostilities.  But, there will be obstacles to overcome.  You must find the keys to various realms, defend yourself against dragons and the Minotaur, avoid snakes and pesky bats who steal things only to drop them in random locations, and survive the maze of the green mamba and crazed, flesh-eating scarabs.




## AdaVenture Game Features

* When looking closely at a pickable object, a hand will appear indicating that a click will pick up the object.  When holding an object, another click will drop it at the current location.  Only one object at a time may be carried.

* Works on PCs or laptops running Windows, OSX, or GNU/Linux.  And if Ada is installed you can build it yourself!  But first try the delivered binaries.

* Windows, OSX & Linux binaries provided, as well as full source. 

* Laptop friendly controls;  supports HiDpi displays.

* The game has four levels.  Level 1 : fairly easy campaign in Sparta, with a more difficult variant [ level 3] due to thick dark fog & randomized key locations.  These are the top two.  Similarly, level 2 is a tricky campaign in Crete, and an even more difficult dark variant [level 4].  These are the bottom two, showing Minotaurs.  You select the desired campaign at the beginning of the game.  

* You can explicitly save the current game by using the v-key.  This allows resumption later.  You must replay from the beginning if you die before saving. (Game state is saved to the directory ~/savedGames/).

* In the unlikely event a game aborts before you save a game, you might be able to recover as follows.
Go to the "data" subdirectory and copy the file "gamestatex.txt" to the ./savedGames/ directory, 
then choose resume at startup and pick gamestatex.txt.
Note that this recover file is NOT created if you die, but only if the game malfunctions.

* Maze/Labyrinth hint: For a younger generation that is not familiar with the original Atari Adventure game, note that there is a magenta colored moveable "bridge" that may be picked up and carried to another location where a shortcut is needed with the following shape: **][**  Its use is required to access the chalice, and to escape the labyrinth in levels 2 & 4.

* To change the appearance of the avatar, simply download your favorite MineCraft Skin, rename it to "skin.png" and put it into the ./data/ directory.  You should probably backup the current skin before you do this, in case of trouble.  See http://www.minecraftskins.net/.  See also ./data/avatars/ for a small selection.

* For developers, serves as a great example of modern OpenGL programming using GLSL 330, shaders, uniforms and Freetype fonts.

* The thin Ada bindings to OpenGL & GLFW3, and the minimal binding to OpenAL in this app are usable as standalone libraries for most any modern Ada graphics project.

* The sound system enables a build with surprising portability across various linux distros, as well as across various platforms.



## mouse/touchpad/keyboard controls

[You might need to disconnect unused gamecontrollers to prevent spinning!]

Look direction is controlled by touch pad or mouse;

The mouse wheel controls camera zoom.  On laptops, a 2-finger swipe simulates the mouse wheel; 
Zoom can also be controlled with keys n, f, z [Nearer,Further,default];  Note that Zoom changes now show immediately, even when stopped, but Lazy Camera change does not show immediately.


Movement is controlled by the WASD keys or the arrow keys:

       (Up)
(Lt)   (Dn)   (Rt)

---------------------------

*	(v)-key			=> saVe game state to resume later
*  (esc)-key 		=> exit;
*  (i)-key			=> toggle Intro/Help screen
*  (h)-key			=> toggle Intro/Help screen
*  (space)-key		=> pick or drop
*  mouse-click		=> pick or drop
*  (m)-key		 	=> toggle Mouse-view (1st-person) or avatar(3rd-person)
*  (l)-key			=> toggle camera type:  1)Lazy(default), or 2)Tight

### Game Notes:
* only one object may be carried. 
* If you want to easily inspect some curious feature (like Jupiter in the night sky), temporarily switch to 1st-person mode with the (m)-key.
* When looking for the black key, it might help to try to find the "ghost" key, a white key image on the ceiling above the actual key location. In levels 3 & 4, the fog makes this difficult to see.

### joystick
* joystick:  attitude
* center thumb btn:  forward
* trigger btn:  backward
* side thumb btns:  pick or drop items

------------------------------------------------------------
### gamecontroller
* Lpaddle/Lhat:  movement
* Rpaddle :  attitude
* L/R Shoulder btns:  pick or drop items

Note that regression testing of these controller functions has not been done for quite a while.  Any volunteers?

------------------------------------------------------------
### controller settings
If the need arises, copy the file "default_settings.txt" to "./data/settings.txt".  Then you can manually edit the floats that define the sensitivity for mouse, keyboard, gamepad & joystick, as well as forward speed of the avatar.


------------------------------------------------------------
------------------------------------------------------------


## System Requirements for running delivered executables:

* graphics card with ample memory & driver that supports OpenGL version 3.3 or later;
* Windows, OSX(>=10.13), GNU/Linux(glibc > v2.17, ~2012)
* optional game controller or joystick.


## Notes on saved games:

Saved games are "txt" files named with the date & time of the save, followed by the chapter number 1..4.
When resuming, the most recently saved games are at the top of the displayed list. 
If the list gets too long for the terminal window, then some older games are not shown. 
So delete or move very old game files periodically under the "savedGames" directory, if needed.



## Setup & Running Adaventure:

The application's root directory [./avent/] contains files for deployment on 3 platforms:  1)windows (64 bit), 2)linux, 3)OSX, in addition to source code.  
* If you are NOT running windows, you do not need .dll files.  

Mac users please read "osx-setup.txt".
Windows users see "windows-setup.txt".


Unzip the archive.  

* On Linux & Windows, 7z [www.7-zip.org] works well for this. The proper command to extract the archive and maintain the directory structure is "7z x filename".

* On OSX, Keka works well for 7Z files. The command-line for Keka is:
	* /Applications/Keka.app/Contents/MacOS/Keka --cli 7z x (filename.7z)

After the archive is unzipped...

Open a commandline terminal, and cd to the install directory, and type: 

adaventure64.bat (Windows 64-bit)
adaventure_gnu (Linux)

adaventure_osx (Mac) [ "adaventure_osx 0" indicates using Low-Dpi video mode; default=High-Dpi ]
	note that Low-Dpi should be used if graphic response is poor.


**OSX note: When resuming a game, there seems to be a quirk with window focus that requires a deliberate cursor-click on the saved-games window prior to selection. Not a big problem if you're expecting it.**

The linux executable has been recently tested, and runs well on Trisquel, ScientificLinux, OpenSuse, and Mint.

**If an older Linux system complains that /dev/dsp/ cannot be opened, prepend the command with "padsp",EG:  "padsp (ExeName)".**


Also, the Windows executable can be run on linux using wine thusly:
	* wine binw64/adaventure64.exe


Windows users note: DO NOT try running the linux executables under WSL [Windows Subsystem for Linux]; that mode is not supported. Simply use the windows version.


Remember to save the game [ v-key ] prior to risky behavior or quitting, in order to easily resume later.
This writes enough information into a file to be able to resume later.

In case of trouble moving, switch to 1st person.

Note that adjustable OpenGL settings should favor performance.  OTOH, this game runs fine on an Intel NUC with embedded Intel graphics, so the graphics demands are modest.


--------------------------------------------------------------------------


## Included Open Source libraries that allow rebuilding:
* GLFW, openal, freetype
* glext.lib for Windows
* the included "bindings" directory contains Ada interfaces for:
	* gl
	* glfwada
	* OpenAL
	* Stephen Sanguine's AdaPngLib
	* Dmitriy Anisimkov's Zlib for Ada
	* Felix Krause's FreeTypeAda
	* Dmitry Kazakov's strings, tables

## Rebuild Requirements:
* systems:  Windows, OSX, or GNU/Linux
* a recent Ada compiler;  eg. GNU-Ada...try this link:
	* https://github.com/alire-project/GNAT-FSF-builds/releases



## Build instructions for AdaVenture:

Three [pre-compiled] binary executables are delivered, two for Windows, & one for gnu/linux.  I believe the Windows executables are fairly portable.  They were built on Windows 10.  

The distributed linux executable requires glibc v2.17 or newer.  That means if your distribution is older than 2012, it may not run, and you might need to recompile. But if you have wine, you can simply run the Windows executables.

Build scripts for GNU Ada, which comes with its own g++ compiler, are provided.

Of course, the build scripts need to be adjusted to reference your actual installation directory for the Ada compiler.

-------------------------------------------------------

**msWin64** => setpath64.bat + w64cmp.bat (read ~docs\gnuAdaOnWindows.txt)

------------------------------------------------------
**MacOSX** => ocmp.sh

------------------------------------------------------
**GNU/Linux** => lcmp.sh:

uses dynamic linking for some common shared libraries that are delivered in this bundle under ./libs/gnu/.  These are used to build the [gnu/linux] executable, which should run in the presence of ./libs/gnu/, whether or not your system has those shared libraries installed.

If the delivered linux binary does not run...

* Manually install GNU Ada.
* Rerun the compile script lcmp.sh.


### Fixable link problems during linux build:

On a linux build machine, you might get a few fixable link errors, depending on its configuration.  If you are missing "libxxx.so", you might simply need to create a proper softlink so it can be found.

----------------------------------------------------------------------
## For Developers Only:  Portable Avatar Using Shaders

* This approach encapsulates the details of avatar shape, color, and movement within GLSL shaders and a related code object that defines vertices and texture maps.  The object may be an Ada package or C++ class.

* Programmatic inputs include uniforms for time, position, attitude, & type of motion.  The shaders then offload the realtime computational burdens onto the graphics processor.

* Data that defines shape and color, as well as the uniforms and functions that define behavior, reside completely within the object and shaders.  This data can ultimately be as detailed and refined as your imagination permits.  And any refinements made are not obfuscated in some esoteric or proprietary format with a limited audience, but remain fully portable and easily enhanced by most any developer using Free Open Source tools and compilers.

* One approach would be to completely define the avatar within the shaders alone, possibly without using any texture files.  Just look at the creatures in (glslsandbox.com).  This would require advanced GLSL skills.

* But a huge selection of available MineCraft skins lead to the present avatar object design.

* In this application, the texture object is a cube with radius one that is defined as 6 disjoint cubelets (the precise name is rectangular cuboid or parallelpiped).  The 2 upper quarters map to the head and torso.  The lower half is divided into 4 cubelets that are mapped to arms and legs. See "cuboid.txt". The Minecraft images used for the texture also have 6 parts that map to the limbs, head and torso.

* The result is an utterly portable avatar defined by an image and 4 text files:
	* texture object body, avatarobj.adb
	* texture object specification, avatarobj.ads
	* vertex shader, avatarobj.vs
	* fragment shader, avatarobj.fs
	* any MineCraft Skin png file

* Interfacing game code with such an avatar is simple.  Essentially you need only pass the current uniform values prior to drawing, including time, position, attitude, motion-type.

* Of course one still needs a decent camera positioning and pointing policy within the game code in order to fully appreciate and exhibit the avatar. The details are beyond the scope of this brief introduction, but generally the current policy is a damped and delayed move toward some fixed ideal camera angle and position above and behind the avatar. Two possible ideal camera angles are: a) pointing toward the look-direction of the avatar; b) pointing toward the avatar. The ideal camera angles would depend upon the immediate game environment and goals.



----------------------------------------------------------------------
## For Developers Only:  Fancy Shaders, FX

This app demonstrates how to use fancy fragment shaders from glslsandbox.com to make starry maze skies. See below (Media Files). It also demonstrates the use of coherent noise to create the rolling fireball in the labyrinth.

----------------------------------------------------------------------
## For Developers Only:  OpenAL portable sound package

This app uses a cross-platform sound-playing package for Ada apps that can asynchronously start and stop music loops, as well as initiate transient sounds, allowing unlimited concurrency.

It plays WAV files, via OpenAL, on Windows, OSX and linux platforms.

It is suitable for <u>any</u> Ada application that needs music, sound loops or transient sound effects; eg. games.



-----------------------------------------------------------------------
## What is special about this project?

The linux-build of this app is among very few modern OpenGL games with sound where a single pre-built executable can run on multiple Linux distros without any 3rd party add-ons! It is routinely tested on OpenSuse[2022], ScientificLinux[redhat derivative from 2012], & Mint.

For developers, this project can serve as a testbed for learning modern OpenGL and GLSL.

It uses the Ada programming language and modern OpenGL methods, with textures, shaders and uniforms.  Compiles and runs on Windows, OSX, & GNU/Linux systems.

Focusing on portability, transparency, and open source freedom, this project relies exclusively on F.O.S.S. tools:  a thin glfw3 binding, a thin OpenGL binding, a PNG reader by Stephen Sanguine and Dimitry Anisimkov, OpenAL-Audio with a homebrew binding, a FreeTypeAda binding by Felix Krause, plus string & table utilities by Dmitry Kazakov.

The Ada bindings are thin, so the relationship to C++ methodology is transparent.  Developers should note that these Ada bindings can be used for your own OpenGL Ada project.

For the C++ programmer the code should be easy to comprehend; and for the experienced Ada programmer there are many ada-specializations yet to be made.  

This game is a work in progress, so please excuse any scaffolding and debugging code has not been removed.

Open source Ada developers are welcome to help improve or extend this app.
Developer or not, send comments, suggestions or questions to:

* fastrgv@gmail.com

and please include the file gamestatex.txt from the data directory if you are reporting a freeze or abort.

--------------------------
## License:


This app is covered by the GNU GPL v3 as indicated in the sources:


 Copyright (C) 2024  fastrgv@gmail.com

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You may read the full text of the GNU General Public License
 at <http://www.gnu.org/licenses/>.


## Media Files:

### General Note
The particular choices of shaders, sounds & images are not essential to the function of the game and are easily replaced. 

It is my intention to use media with copyrights or licenses that are compatible with GPLv3. Please notify me if you believe there is an incompatibility, and it will be removed ASAP, eg a CC-by-NC license is NOT GPL compatible.


### SoundFiles
Most sounds are from freesound.org and are covered by the Creative Commons CC0 license. A few others with CC-by-3.0 license have accompanying text files with attributions. See ./docs/otherLicenses/. The small waterfall sound (CC4 license) was created by Giovanni Provenzale.

Others [eg. shriek] are from http://www.freesfx.co.uk, which have an attribution requirement, but no legal restrictions otherwise.

Some original Atari sounds [public domain] were also used.

Credit and thanks to Kevin Macleod [CC-by-3.0] for his great compositions Cephalopod, AngevinB, & Ibn-Al-Noor.

Credit and thanks to the Godfather of Exotica, Korla Pandit, for the excellent renditions of Turkish Dance and Miserlou...a song so old that its origins are vague, yet was known to have been popular in ancient Persia and the middle-east, as well as to all us fans of Dick Dale!


### ImageFiles 
* the GPL2.0/GPL3.0-only section of OpenGameArt.Org.  
* http://www.mayang.com/textures.
* pixabay.com with a CC0 or CC3 license.
* See ./docs/otherLicenses/.
* http://all-free-download.com/free-photos/ (public domain).


### ShaderFiles 
Several fragment shader files used were downloaded from http://glslsandbox.com/ and put under ./data/.  All frag. shaders from glslsandbox are under the MIT license (see ./docs/mit_license.txt).  Existing comments or any identifying information was retained.

In order to make these usable, I had to modernize them to glsl version 330 specifications, and adapt them to utilize some programatic uniforms for input.


### SkyBoxes 

* www.custommapmakers.org/skyboxes.php
* www.OpenGameArt.org;  thanks to Pieter Spiney Verhoeven (cloudy)
* Some beautiful hi-res skyboxes used [from OpenGameArt.org] are the work of Heiko Irrgang <hi@93-interactive.com> and licensed under the Creative Commons Attribution-ShareAlike 3.0 Unported License.  To view a copy of this license, visit (http://creativecommons.org/licenses/by-sa/3.0/) or send a letter to Creative Commons, 444 Castro Street, Suite 900, Mountain View, California, 94041, USA.  See also the accompanying file ./docs/ccsa3_license.txt.


### Bindings & Utilities

Thanks to Dmitry Kazakov, Dimitry Anisimkov, Stephen Sanguine and Felix Krause, for their software tools, as mentioned above.


## Download Sites for my games:

* https://github.com/fastrgv?tab=repositories
* https://www.indiedb.com/members/fastrgv/games
* https://fastrgv.itch.io
* https://sourceforge.net/u/fastrgv/profile/
* https://gamejolt.com/@fastrgv/games


## Video Level 1:
https://youtu.be/Pr0IhvHXvFQ

## Tags
kids,retro,adventure,dragon,castle,maze,labyrinth

-------------------------------------------------------------------------
-------------------------------------------------------------------------

## Update History:


**ver 2.3.6 -- 5sep2023**
* Fixed problem where saved games could not be restored.

**ver 2.3.5 -- 2sep2023**
* Added exception-handler to save state if a freeze or abort error occurs.
* Made minotaur bipedal, because he is actually half-man.
* Current game state can now be saved at any time to a time-stamped file, allowing restoration during startup from a select-list.
* No longer delivering 32-bit Windows code.

**ver 2.3.4 -- 23feb2023**
* Fixed graphical anomaly in castle; & other code improvements.
* Documented ghost-key.
* Bat now grabs sword if it is dropped to pickup black key.


**ver 2.3.3 -- 8feb2023**
* Refined labyrinth actions & sounds.
* Added theme music for intro-screen [from P.I.R.]
* Added more Kevin Macleod music at finale.

