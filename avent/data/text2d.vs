#version 330 core

layout(location = 0) in vec2 vertexPosition_screenspace;
layout(location = 1) in vec2 vertexUV;

uniform ivec2 screenCen;

out vec2 UV;

void main(){

	// Output position of the vertex, in clip space
	// map screen to [-1..1][-1..1]
	vec2 vertexPosition_homoneneousspace = vertexPosition_screenspace - screenCen;
	vertexPosition_homoneneousspace /= screenCen;
	gl_Position =  vec4(vertexPosition_homoneneousspace,0,1);
	
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

