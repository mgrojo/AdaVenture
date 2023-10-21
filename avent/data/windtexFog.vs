#version 330 core

// vertex shader

layout(location = 0) in vec3 vertexPosition_modelspace;
layout(location = 1) in vec2 vertexUV;

out vec2 UV;

out vec4 aPos;

uniform mat4 MVP;




uniform float mytime;

uniform vec3 wPos;
uniform vec3 wRad;

// each id moves differently
uniform int palmID; 
// 1,2=>short palms
// 3..8=> tall palms
// 9..11 => bamboo
// 12... => grass

uniform float sandLevel; // somewhat below zero

//const float pi = 3.14159;
const float pi = 3.141592654;
const float pamp = 0.01; //0.02;

float zwind(float y) {

  	float amp, ht, ht2;
	
	float tfac;
	if( palmID >= 12 )       tfac=1.5; // grasses
	else if( (palmID>=3) && (palmID<=8) ) tfac=0.5; // tpalm
	else                   tfac=0.8; // spalms, bamboo

	ht = y - sandLevel;
	ht2 = 5*ht*ht;

  	if( palmID >= 12 ) amp=pamp*ht2; // grasses
  	else amp=pamp*ht;

	float phase=float(palmID);

	return amp * sin(2.0*pi*(mytime*tfac+phase)/5.0);

} // end zwind

float xwind(float y) {

   float amp, ht, ht2;

	float tfac;
	if( palmID >= 12 )       tfac=1.5; // grasses
	else if( (palmID>=3)&&(palmID<=8) ) tfac=0.5; // tpalm
	else                   tfac=0.8;

	ht = y - sandLevel;
	ht2 = 5*ht*ht;

  	if( palmID >= 12 ) amp=pamp*ht2; // grass
  	else amp=pamp*ht;

	float phase=float(palmID);

	return amp * sin(2.0*pi*(mytime*tfac+phase)/3.0);

} // end xwind


void main(){

	//on entry, trees are centered @ origin, radius=1.0

	vec3 pos = vertexPosition_modelspace;

	// add a slight vertical motion [ddv] to outer branches
	float sr = length(pos.xz);  // currently can only be zero or one
	clamp( sr, 0.0, 1.0);
	float vamp = pamp*wRad.y;
	float ddv = 0.5*(1.0+sin(2.0*pi*mytime*0.25))*vamp*sr;



	pos.x *= wRad.x;
	pos.y *= wRad.y;
	pos.z *= wRad.z;
	// we assume original setup with unitary radii

	pos.x += wPos.x;
	pos.y += wPos.y;
	pos.z += wPos.z; 
	// we assume original setup @ origin

	// horizontal wind displacements:
	float dz=zwind(pos.y);
	float dx=xwind(pos.y);

	// if alpha = angle from vertical,
	// then cos(alpha) = ht/hypot = dh/(hypot-ht)

	float ht = pos.y - sandLevel;
	float hypo = sqrt( ht*ht + dx*dx + dz*dz );
	float hypo1 = 0.1+hypo;
	float mt = max(0,ht);
	float dh = (hypo-ht)*(mt/hypo1);

	pos.z = pos.z + dz;
	pos.x = pos.x + dx;
	pos.y = pos.y - dh + ddv;

	aPos = vec4( pos, 1.0 );

	gl_Position =  MVP * vec4(pos,1.0);
	UV = vertexUV; 
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

