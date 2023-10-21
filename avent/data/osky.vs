#version 330 core

layout(location = 0) in vec3 pos;

//uniform vec3 CameraPosition;
uniform mat4 MVP;

out vec3 texcoord;

void main()
{
    texcoord = normalize(pos);
    //gl_Position = MVP * vec4(pos.xyz + CameraPosition, 1.0);
    gl_Position = MVP * vec4(pos, 1.0);
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

