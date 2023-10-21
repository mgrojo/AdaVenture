#version 330 core

in float opacity;
in vec2 UV;
out vec4 color;

uniform int darkness=0; // 0..4=darkest
const vec4 night = vec4(0.0,0.0,0.0,1.0);

uniform sampler2D myTextureSampler;

void main(){

	color = texture( myTextureSampler, UV).rgba;
	// UV [above] is smooth;

	// uv [below] gives the pixelation 
	// expected of a MineCraft texture:
	//vec2 uv = floor(0.5+UV*64.0)/64.0;
	//color = texture( myTextureSampler, uv).rgba;


	float df=0;
	if( darkness < 1 ) df=0.0;
	else if( darkness==1 ) df=0.3;
	else if( darkness==2 ) df=0.6;
	else if( darkness==3 ) df=0.8;
	else if( darkness>3 ) df=0.9;
	float asave=color.a;
	color = mix(color, night, df);



	if( asave < 0.1 ) discard;

	color.a = opacity;

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

