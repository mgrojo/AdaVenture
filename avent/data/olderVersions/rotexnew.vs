#version 330 core

// vertex shader

layout(location = 0) in vec3 vertexPosition;
layout(location = 1) in vec2 vertexUV;
layout(location = 2) in vec3 vertexNormal; // for lighting

smooth out vec2 UV;
smooth out vec4 aPos;

smooth out vec3 vNormal;      // for lighting
smooth out mat3 NormalMatrix; // for lighting


uniform mat4 MVP;
uniform mat3 NM = mat3(1.0); // needed only for changing MM

uniform int lightFlag=0; // 0=>no, 1=>yes   vs#4



uniform vec3 wPos;
uniform vec3 wRad;

uniform float horiAng; //new



void main(){

	//first we twist & move:////////////////////////////

	vec3 opos = vertexPosition;
	vec3 pos;

	opos.x *= wRad.x;
	opos.y *= wRad.y;
	opos.z *= wRad.z;
	// we assume original setup with unitary radii

	pos.y = opos.y;
	pos.x = +cos(horiAng)*opos.x + sin(horiAng)*opos.z;
	pos.z = -sin(horiAng)*opos.x + cos(horiAng)*opos.z;

	pos += wPos;
	// we assume original setup @ origin


	//second, we proceed as before://///////////////////


	aPos = vec4(pos,1.0);
	gl_Position =  MVP * aPos;
	//eyeCoords =  MV * aPos;
	UV = vertexUV; 

	// lighting addendum:
	if( lightFlag>0 )	
	{
		vNormal = normalize(vertexNormal);

		// used only if changing ModelMatrix,
		// i.e. moving/rotating objects
		NormalMatrix = NM;
	}
	else // do not require meaningful values
	{
		vNormal = vec3(0.0, 1.0, 0.0);
		NormalMatrix=mat3(1.0); //identity
	}

}


//--
//-- Copyright (C) 2020  <fastrgv@gmail.com>
//--
//-- This program is free software: you can redistribute it and/or modify
//-- it under the terms of the GNU General Public License as published by
//-- the Free Software Foundation, either version 3 of the License, or
//-- (at your option) any later version.
//--
//-- This program is distributed in the hope that it will be useful,
//-- but WITHOUT ANY WARRANTY; without even the implied warranty of
//-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//-- GNU General Public License for more details.
//--
//-- You may read the full text of the GNU General Public License
//-- at <http://www.gnu.org/licenses/>.
//--

