#version 330 core

// vertex shader

layout(location = 0) in vec3 vertexPosition;
layout(location = 1) in vec2 vertexUV;
layout(location = 2) in vec3 vertexNormal; // for lighting

smooth out vec2 UV;
smooth out vec4 aPos;

smooth out vec3 vNormal;      // for lighting
//smooth out mat3 NormalMatrix; // for lighting


uniform mat4 MVP;
//uniform mat3 NM = mat3(1.0); // default=identity

uniform int lightFlag=0; // 0=>no, 1=>yes   vs#4


void main(){


	aPos = vec4(vertexPosition,1.0);
	gl_Position =  MVP * aPos;
	UV = vertexUV; 

	// lighting addendum:
	if( lightFlag>0 )	
	{
		vNormal = normalize(vertexNormal);

		// used only if changing ModelMatrix,
		// i.e. moving/rotating objects
		//NormalMatrix = NM;
	}
	else // do not require meaningful values
	{
		vNormal = vec3(0.0, 1.0, 0.0);
		//NormalMatrix=mat3(1.0); //identity
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

