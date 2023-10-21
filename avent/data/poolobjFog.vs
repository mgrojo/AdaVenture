#version 330 core
layout(location = 0) in vec3 vertexPos;
out vec2 UV;
out vec4 aPos;

uniform mat4 MVP;
uniform float mytime;

uniform vec3 wPos;
uniform vec3 wRad;

uniform float waterlevel;

//const float pi = 3.14159;
const float pi = 3.141592654;
const float amplitude=0.005; //was 0.005
const float npeaks=4;
const float speed=0.1; //0.4;

float wave(float r) {
    return amplitude * sin( npeaks*(mytime*speed+r)*2*pi);
}

float waveHeight(float x, float z) {
	float rad = sqrt(x*x+z*z);
   float height = 0.0;
   height += wave(rad);
   return height;
}


void main(){
	vec3 pos = vertexPos;

	aPos = vec4(pos,1.0);

	float xmin=wPos.x-wRad.x;
	float xmax=wPos.x+wRad.x;
	float zmin=wPos.z-wRad.z;
	float zmax=wPos.z+wRad.z;
	UV = vec2( (pos.x-xmin)/(xmax-xmin), (pos.z-zmin)/(zmax-zmin) );

	pos.y = waterlevel + waveHeight(vertexPos.x-wPos.x, vertexPos.z-wPos.z);
	gl_Position =  MVP * vec4(pos,1.0);
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

