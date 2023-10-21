#version 330 core

// fragment shader reflective water pool

in vec3 position;    // water pos from vert.shader
in vec3 worldNormal; // water nrm from vert.shader
out vec4 color;

uniform samplerCube envMap;
uniform vec3 eyePos;

void main(){

   vec3 eye = normalize(position-eyePos);
   vec3 r = reflect(eye, worldNormal);

   color = texture(envMap, r);

	// adjust hue of water in brownish marble
	color.b = 1.5*color.b; //  +50% blue
	color.g = 1.3*color.g; //  +30% green

	// set opacity @ 50%
	color.a = 0.5;

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

