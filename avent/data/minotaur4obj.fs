#version 330 core

in vec2 UV;
out vec4 color;

uniform float mytime;
uniform sampler2D myTextureSampler;

//const vec4 vFogColor=vec4(0.9,0.9,0.9,1.0);
const vec4 vFogColor=vec4(0.7,0.7,0.9,1.0); //bluish tinge

void main(){

	color = texture( myTextureSampler, UV).rgba;

	// time the heavy breathing:
	const float period = 2.0; // seconds
	float et = period*fract(mytime); // [0..2)
	float opac = (period-et)/period; // [0..1)


	// exact nostril coords:
	//const float lx=10.5/64.0;
	//const float rx=13.5/64.0;
	const float ny=14.5/32.0;

	// put smoke centroids more outboard:
	const float lx=9.5/64.0;
	const float rx=14.5/64.0;

	// pixel (texel) distance from nostrils:
	//vec2 ldist = vec2( 64.0*(UV.x-lx), 32.0*(UV.y-ny) );
	//vec2 rdist = vec2( 64.0*(UV.x-rx), 32.0*(UV.y-ny) );

	// this generates [e=2] ellipses w/horizontal major axis:
	vec2 ldist = vec2( 64.0*(UV.x-lx), 64.0*(UV.y-ny) );
	vec2 rdist = vec2( 64.0*(UV.x-rx), 64.0*(UV.y-ny) );

	float ld = length(ldist); // pixel-radius from lf nostril
	float rd = length(rdist); // pixel-radius from rt nostril

	// use inverse fog, i.e. fog very near nostril:
	const float pixrad = 2.0; // texel-radius of fog
	float dist = clamp( min(ld,rd)/pixrad, 0.0, 1.0 );

	//assuming dist in 0..1 :
	color = mix(color,  vFogColor, opac*(1.0-dist) );

	if( color.a < 0.1 ) discard;

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

