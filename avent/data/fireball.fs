#version 330 core


in float mynoise;
in vec4 aPos;

out vec4 color;

uniform sampler2D myTextureSampler;

uniform float opacity; // 12jan17 addendum

// color assigned to match radial perturbations




void main(){

   vec2 tPos = vec2( 0,  6.0 * mynoise +0.4 ); //explosion2.png
	// note:  6.0 = color "spread" factor
	//        0.4 = average color [mean(mynoise)=0]

	color = texture( myTextureSampler, tPos).rgba;

	if( (opacity>0.1) && (opacity<0.99) ) color.a *= opacity;

}

/*
--
-- Copyright (C) 2020  <fastrgv@gmail.com>
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

