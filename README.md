![screenshot](https://github.com/fastrgv/AdaVenture/blob/master/trim_smallMinotaur.gif)


![screenshot](https://github.com/fastrgv/AdaVenture/blob/master/mcraftAV.jpg)

![screenshot](https://github.com/fastrgv/AdaVenture/blob/master/nuAV.jpg)

![screenshot](https://github.com/fastrgv/AdaVenture/blob/master/copper.png)

![screenshot](https://github.com/fastrgv/AdaVenture/blob/master/av1.jpg)


Click on the large 7z file under releases to download all source & binaries (Windows,Mac & Linux) or try this link:

https://github.com/fastrgv/AdaVenture/releases/download/v2.2.7/av18jan22.7z

Type "7z x filename" to extract the archive.


### GitHub downloaders: Please ignore the "Source code" zip & tar.gz files. (They are auto-generated by GitHub). Click on the large 7z file under releases to download all source & binaries (Windows,Mac & Linux). Then, type "7z x filename" to extract the archive. 


==============================================================

Video:  New Open Doorways + Mamba:
https://youtu.be/lXkuM0z0JqA

Video:  Walkthru level 1:  Spartan Temple
https://youtu.be/FNE20lw1AZo

Long Video:  Walkthru level 2:  Minoan Labyrinth
https://youtu.be/oVW205APsS0

Short Video:  Charge of the Minotaur:
https://youtu.be/iRj_SnqXbZc

Short Video:  Omar escapes mamba thru moveable bridge:
https://youtu.be/8qbAJ-JvvXs

Carnivorous Beetles:
https://youtu.be/428fRdu-fZs




# AdaVenture GLFW/OpenAL version



## Whats new:

**ver 2.2.8 -- 16apr2022**

* Improved zoom function.
* Converted linux libraries to exclusively shared format for portability.
* Alternate script for GNU-Ada also works, now.


**ver 2.2.7 -- 18jan2022**

* Elliminated unused Frameworks directory
* Updated Windows builds to freetype v2.11.1 DLLs (w32,w64).
* Updated linux libs to use static libfreetype.a & libpng16.a
* 
**ver 2.2.6 -- 17dec2021**
* The (h)-key now, also, toggles Intro/Help screen;
* Added more example build scripts, including one for OSX that does not require Xcode.
* Replaced all cc-by-nc-licensed sound files due to incompatibility with GPLv3.
* Improved sound-tracks in Labyrinth.

**ver 2.2.5 -- 17nov2021**
* Updated all GLFW libs to newest [static] version(v3.3.5), & scripts.
* Elliminated OpenGL-mipmap error on nvidia nouveau drivers.
* Now include library fix & build scripts for GNU-Ada, as well as AdaCore.

**ver 2.2.4 -- 25oct2021**
* Improved adaOpenAL binding code...AdaVenture is now buildable with [GNU Compiler Collection] GNAT, as well as all AdaCore versions.
* Updated glext64.lib (Win64).
* In addition to the Win64 build, now added a Win32 build.

## More change-history at end of file



## AdaVenture Game Description
AdaVenture is a kid-friendly retro point & click game, intended as a reincarnation in 3D of the original Atari game named "Adventure", with various artistic extrapolations.  The mazes have interconnections that are impossible in 3 dimensions, but are painstakingly exact reproductions of those in the original game.  In fact, the "./mapRoom" subdirectory contains the original maps to help guide you.  

Runs on Windows, OSX, and GNU/Linux.

-----------------------------------------------------------
Featuring

	* no installation
	* no dependencies
	* simply unzip in your Downloads directory, and run.
-----------------------------------------------------------

## AdaVenture Introduction
Set in ancient Persia, it begins outside the castle of the young King Xerxes, who inherited a magical golden chalice from his father, Darius the Great.  Coveted by Greek foes King Leonidas of Sparta and King Minos of Crete, the chalice has been stolen.

Your quest is to seek and return the royal chalice to its pedestal within the castle of Xerxes...a stealth mission to preclude open hostilities.  But, there will be obstacles to overcome.  You must find the keys to various realms, defend yourself against dragons and the Minotaur, avoid snakes and pesky bats who steal things only to drop them in random locations, and survive the maze of the green mamba and crazed, flesh-eating scarabs.

Be sure to look to the sky as you return to the castle with the chalice.



## AdaVenture Game Features

* When looking closely at a pickable object, a hand will appear indicating that a click will pick up the object.  When holding an object, another click will drop it at the current location.  Only one object at a time may be carried.

* Works on PCs or laptops running Windows, OSX or GNU/Linux.  And if Ada is installed you can build it yourself!  But first try the delivered binaries.

* Windows, GNU/Linux and OSX binaries provided, as well as full source. 

* Note that a 64 bit build for Windows is delivered.

* Laptop friendly controls;  supports Mac Retina displays in high DPI mode.

* The game has four levels.  One fairly easy campaign in Sparta, with a more difficult variant due to thick dark fog.  These are the top two.  Similarly, there is a tricky campaign in Crete, and an even more difficult dark variant.  These are the bottom two, showing Minotaurs.  You select the desired campaign at the beginning of the game.  You can save the current game by using the v-key.  This allows resumption later.  You must replay from the beginning if you die before saving.  Saving the game state is a relatively new feature that only saves one game.  If a saved game exists, a fifth option appears at screen center to resume that game.

* To change the appearance of the avatar, simply download your favorite MineCraft Skin, rename it to "skin.png" and put it into the ./data/ directory.  You should probably backup the current skin before you do this, in case of trouble.  See http://www.minecraftskins.net/.  See also ./data/avatars/ for a small selection.

* For developers, serves as a great example of modern OpenGL programming in Ada or C++ using GLSL 330, shaders, uniforms and Freetype fonts.

* The Ada bindings to OpenGL, OpenAL & GLFW3 in this app are usable as a standalone library for most any modern Ada graphics project.



## mouse/touchpad/keyboard controls

[You might need to disconnect unused gamecontrollers to prevent spinning!]

Look direction is controlled by touch pad or mouse;

The mouse wheel controls camera zoom.  On MacBooks, a 2-finger swipe simulates the mouse wheel; Zoom can also be controlled with keys n, f, z [Nearer,Further,default];  Note that Zoom changes now show immediately, even when stopped, but Lazy Camera change does not show immediately.


Movement is controlled by the WASD keys or the arrow keys:

       (Up)
(Lt)   (Dn)   (Rt)

---------------------------

*	(v)-key			=> save game state to resume later
*  (esc)-key 		=> exit;
*  (i)-key			=> toggle Intro/Help screen
*  (h)-key			=> toggle Intro/Help screen
*  (space)-key		=> pick or drop
*  mouse-click		=> pick or drop
*  (m)-key		 	=> toggle Mouse-view (1st-person) or avatar(3rd-person)
*  (l)-key			=> toggle camera type:  1)Lazy, 2)Tight

In case of unexpected control problems with the game, or if you want to easily inspect some curious feature (like Jupiter in the night sky), temporarily switch to 1st-person mode with the (m)-key.


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

------------------------------------------------------------
### controller settings
If the need arises, copy the file "default_settings.txt" to "./data/settings.txt".  Then you can manually edit the floats that define the sensitivity for mouse, keyboard, gamepad & joystick, as well as forward speed of the avatar.


------------------------------------------------------------
------------------------------------------------------------


## required for running:

* graphics card with ample memory & driver that supports OpenGL version 3.3 or later;
* Windows, GNU/Linux(glibc > v2.14) or OSX >= 10.13(sep2017);
* optional game controller or joystick.


## Setup & Running Adaventure:

The application's root directory [./avent/] contains files for deployment on 3 platforms:  1)windows (64bit), 2)OS-X, 3)linux, in addition to source code.  If you are NOT running windows, you do not need .dll files.  If you are NOT running OS-X, you do NOT need the subdirectory named ./adaventure.app/.


Mac users see "osx-setup.txt".
Windows users see "windows-setup.txt".


Unzip the archive.  On Windows, 7z [www.7-zip.org] works well for this.
The proper command to extract the archive and maintain the directory structure is "7z x filename".

Open a commandline terminal, and cd to the install directory, and type: 

adaventure.bat (Windows 64-bit)
adaventure32.bat (Windows 32-bit)
adaventure_osx (Mac)
adaventure_gnu (Linux, AdaCore build)


The linux executable has been recently tested on Trisquel, ScientificLinux, and Mint.

Also, the Windows executable can be run on linux using wine thusly:
	* wine binw64/adaventure64.exe


Windows users note: I suggest that you DO NOT try running the linux executables under WSL [Windows Subsystem for Linux]; that mode is not supported. Simply use the windows version.



Mac users can also navigate to the installation directory in Finder and click the "adaventure.app" icon named "AdaVenture".  Note that any jerkiness experienced while running at HiDpi can be elliminated by editting the bundle-controls to force LowDpi.

Save the game [ v-key ] prior to risky behavior or quitting, in order to resume later.

In case of trouble moving, switch to 1st person.

Note that adjustable OpenGL settings should favor performance.  OTOH, this game runs fine on an Intel NUC with embedded Intel graphics, so the graphics demands are modest.


--------------------------------------------------------------------------
Open source Ada developers are welcome to help improve or extend this game.

Please send improvements, comments, suggestions or questions to:

fastrgv@gmail.com



## Included Open Source libraries that allow rebuilding:
* GLFW, openal, freetype
* glext.lib for Windows
* the included "bindings" directory contains Ada interfaces for:
	* AdaPngLib
	* gl
	* glfwada
	* OpenAL
	* FreeTypeAda
	* Kazakov strings, tables

## Rebuild Requirements:
* systems:  Windows, OSX or GNU/Linux
* a recent Ada compiler;  eg. AdaCore-Ada, GNU-Ada




## Build instructions for AdaVenture:

Three [pre-compiled] binary executables are delivered, one for Windows, one for gnu/linux and one for OSX.  I believe the Windows executables are fairly portable.  They were built on Windows 10.  The Mac binary, adaventure_osx, should run on most any standard Mac with a recent version of OSX.  

The distributed linux executable requires glibc v2.14 or newer.  That means if your distribution is older than june 2011, it may not run, and you will need to recompile.

Build scripts for AdaCore Ada [with its own g++] are provided. But should also work for GNAT from the GNU Compiler Collection, with minor changes. See avgcmp.sh.

-------------------------------------------------------

**msWin32** => w32cmp.bat

or

**msWin64** => wcmp.bat

Note that wcmp.bat assumes the 64bit AdaCore compiler is on your path. Slight modifications might be needed to use [the GNU Compiler Collection ] GNAT compiler.

Note also that the [hard-to-find] 64-bit library file glext64.lib was built using the AdaCore g++ compiler versus glext-src code obtained from Source Forge. You can use it as-is; you need not recreate it. See glext64.7z.


-------------------------------------------------------
**MacOSX** => ocmp.sh:

build script for generating a portable executable that will run on most OSX platforms whether or not they have non-standard library GLFW installed.  This is used to build the executable named adaventure_osx.  Macs with a recent but standard configuration of OSX should be able to rebuild using these scripts, assuming you have Ada installed, as well as g++ from Xcode.

Note: ./alternateBuildScripts/GNocmp.sh shows how to build on OSX without Xcode.

------------------------------------------------------
**GNU/Linux** => lcmp.sh:

uses mostly dynamic linking, especially for the non-standard library GLFW, as well as other more common shared libraries that are delivered in this bundle under ./libs/gnu/.  These are used to build the [gnu/linux] executable, which should run in the presence of ./libs/gnu/, whether or not your system has those shared libraries installed.

If the delivered linux binary does not run...

* Manually install [community] Ada from adacore.com/download/.
* Rerun the compile script lcmp.sh.


### Link Problems during linux build:

On a linux build machine, you might get fixable link errors, depending on its configuration.  If you are missing "libxxx.so", you might need to create a proper softlink so it can be found.

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

* Of course one still needs a decent camera positioning and pointing policy within the game code in order to fully appreciate and exhibit the avatar. The details are beyond the scope of this brief introduction, but generally the current policy is a damped and delayed move toward some fixed ideal camera position above and behind the avatar. 



----------------------------------------------------------------------
## For Developers Only:  Fancy Shaders

This app demonstrates how to use fancy fragment shaders from glslsandbox.com to make starry maze skies. See below (Media Files). It also demonstrates the use of coherent noise to create the rolling fireball in the labyrinth.

----------------------------------------------------------------------
## For Developers Only:  OpenAL portable sound package

This app uses a cross-platform sound-playing package for Ada apps that can asynchronously start and stop music loops, as well as initiate transient sounds.

It plays WAV files, via OpenAL, on Windows, OSX, and linux platforms.

It is suitable for any Ada application that needs music, sound loops or transient sound effects; eg. games.



-----------------------------------------------------------------------
## What is special about this project?

The linux-build of this app is among very few modern OpenGL games with sound where a single pre-built executable can run on multiple Linux distros without any 3rd party add-ons! It has been tested on OpenSuse, ScientificLinux, Mint and CentOS.

For developers, this project can serve as a testbed for learning modern OpenGL and GLSL.

It uses the Ada programming language and modern OpenGL methods, with textures, shaders and uniforms.  Compiles and runs on Windows, GNU/Linux and Mac OSX systems.

Focusing on portability, transparency, and open source freedom, this project relies exclusively on F.O.S.S. tools:  a thin glfw3 binding, a thin OpenGL binding, a PNG reader by Stephen Sanguine and Dimitry Anisimkov, OpenAL-Audio with a homebrew binding, a FreeTypeAda binding by Felix Krause, plus string & table utilities by Dmitry Kazakov.

The Ada bindings are thin, so the relationship to C++ methodology is transparent.  Developers should note that these Ada bindings can be used for your own OpenGL Ada project.

For the C++ programmer the code should be easy to comprehend; and for the experienced Ada programmer there are many ada-specializations yet to be made.  

This game is a work in progress, so please excuse any scaffolding and debugging code has not been removed.

If you make improvements, please send then to <fastrgv@gmail.com>



--------------------------
## License:


AdaVenture itself is covered by the GNU GPL v3 as indicated in the sources:


 Copyright (C) 2021  fastrgv@gmail.com

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
Most sounds are from freesound.org and are covered by the Creative Commons CC0 license documented in the accompanying file ./docs/otherLicenses/creativeCommonsCC0.txt. A few others with CC-by-3.0 license have accompanying text files with attributions.

Others [eg. shriek] are from http://www.freesfx.co.uk, which have an attribution requirement, but no legal restrictions otherwise.

Some original Atari sounds [public domain] were also used.

Credit and thanks to the Godfather of Exotica, Korla Pandit, for the excellent renditions of Turkish Dance and Miserlou...a song so old that its origins are vague, yet was known to have been popular in ancient Persia and the middle-east, as well as to all us fans of Dick Dale!


### ImageFiles 
* the GPL2.0/GPL3.0-only section of OpenGameArt.Org.  
* http://www.mayang.com/textures.  See ./docs/mayang_license.txt.  
* pixabay.com with a CC0 license.
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

https://github.com/fastrgv?tab=repositories
https://www.indiedb.com/members/fastrgv/games
https://fastrgv.itch.io
https://sourceforge.net/u/fastrgv/profile/
https://gamejolt.com/@fastrgv/games


## Video Level 1:
https://youtu.be/Pr0IhvHXvFQ

## Tags
kids,retro,adventure,dragon,castle,maze,labyrinth

-------------------------------------------------------------------------
-------------------------------------------------------------------------

## Update History:

**ver 2.2.3 -- 18apr21**
* Improved movement, playability & random key locations [ch3,ch4].
* Improved maps to show beetles' domain as well as possible key locations.
* Maze entrance now has Arabic warning, & Zoroastrian symbol.
* Trees now impenetrable.

**ver 2.2.2 -- 10apr21**
* Improved castle exterior & lighting on interior alcove.
* Upgraded to glfw v3.3.4.
* Fixed bad logic that forced a death by beetles.
* Enhanced maze maps.

**ver 2.2.1 -- 28mar21**
* Added beetle warning so gamers don't dawdle among them.
* Added decorative metallic copper brackets to castle pool as a demonstration of multi-texturing and reflective lighting.

**ver 2.2.0 -- 07nov20**
* Installed completely new cross-platform sound system using OpenAL.
* Now deliver only a single Windows build (64-bit).

**ver 2.1.8 -- 18sep20**
* Added Windows launcher "adaventure.bat".


**ver 2.1.7 -- 02jul20**
* Updated all GLFW libs to v3.3.2.
* Added extra drama if angered beetles decide to attack.
* Added explicit safeguards to GLFW binding.

**ver 2.1.6 -- 30apr20**
* Fixed/restored full-screen on all operating systems;


**ver 2.1.5 -- 18apr20**
* Changes in shaders now assure that OpenGL v3.3 is sufficient to run this app.  This is an issue for older graphics drivers.
* Resolved glfw full-screen problem on RedHat-derived linux distros by running "windowed full screen" within desktop border.
* Updated to glfw v3.3.2.


**ver 2.1.4 -- 31mar20**
* Fixed rare linux soundLoop overruns due to wrong PID.


**ver 2.1.3 -- 27jan20**
* Fixed occasional task-related aborts (linux version).


