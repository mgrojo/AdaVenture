#version 330 core

uniform samplerCube CubeMap;

uniform int fogcolr=1; //  1=>white, 2=>brownish, 3=>purple, 4=>gray
uniform int foglevl=0; // 0=>noFog, 1=>Fog, 2=>heavy

// NOTE:  this shader is used for skybox.  (no darkness uniform)
//        Darkness is typically set to match skybox.

in vec3 texcoord;

in vec4 aPos;

out vec4 color;



const vec4 vFogColor=vec4(0.7,0.7,0.7,1.0); //normal white
const vec4 vDfogColor=vec4(0.3,0.3,0.3,1.0); //normal gray

const vec4 vSootColor=vec4(50.0/255,30.0/255,10.0/255,1.0); // brownish soot
const vec4 vMystColor=vec4(0.7,0.5,0.7,1.0); // purplish fog



void main()
{
   color = texture(CubeMap, texcoord);

	if(foglevl>0) {
		float angl = atan( aPos.y, length(aPos.xz) );
		//clamp(angl, 0.0, 1.0 ); // 1 radian =~ 57deg
		//float dist = 1.0 - angl;

		clamp(angl, 0.0, 0.5 ); // 1 radian =~ 57deg
		float dist = 2.0 * (0.5 - angl); //limit @ 30deg

		clamp(dist, 0.0, 1.0);

		if( fogcolr==4 )
			color = mix(color,  vDfogColor, dist);
		else if( fogcolr==3 )
			color = mix(color, vMystColor, dist);
		else if( fogcolr==2 )
			color = mix(color, vSootColor, dist);
		else if( fogcolr==1 )
			color = mix(color,  vFogColor, dist);

		//if(foglevl>1) color = mix(color, vFogColor, dist);
		//else        color = mix(color, vDfogColor, dist);
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

