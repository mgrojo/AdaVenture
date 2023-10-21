
/*
--
-- Copyright (C) 2021  <fastrgv@gmail.com>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You may read the full text of the GNU General Public License
-- at <http://www.gnu.org/licenses/>.
--
*/


#ifndef OAL_HPP
#define OAL_HPP

#include <al.h>
#include <alc.h>

ALCdevice * opendevice(void);

ALCcontext * createcontext( ALCdevice * dev );

void makecontextcurrent( ALCcontext * con);

void genbuffers( ALuint * buf );

void bufferdata( 
	ALuint buf, 
	ALuint fmt, 
	const ALvoid * dat, 
	int sz, 
	int rt );

void gensources( ALuint * sid );

void sourcei( ALuint sid, ALuint buf );

void cleanup1( 
	ALuint * sid, 
	ALuint * buf
	);


void cleanup2( 
	ALCcontext * con, 
	ALCdevice * dev
	);



void sourceplay( ALuint sid );
void sourcestop( ALuint sid );

bool is_stopped( ALuint sid );

bool is_true( void ); // 18oct21 fastrgv

#endif

