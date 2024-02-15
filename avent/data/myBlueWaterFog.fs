#version 330 core
// as of 8feb24 this is not in use
// because I am using a reflective pool instead.

in vec2 UV;
in vec4 aPos;
out vec4 color;

uniform vec3 eyePos;


const float fStart=0.0;
const float fEnd=16.0; // 13dec17

const vec4 vFogColor=vec4(0.9,0.9,0.9,1.0);
const vec4 vDfogColor=vec4(0.3,0.3,0.3,1.0); //gray
const vec4 vSootColor=vec4(50.0/255,30.0/255,10.0/255,1.0); // brownish soot
const vec4 vMystColor=vec4(0.7,0.5,0.7,1.0); // purplish fog


float getFogFactor(float fFogCoord)
{
	float fResult = 0.0;
	fResult = (fEnd-fFogCoord)/(fEnd-fStart);
	fResult = 1.0-clamp(fResult, 0.0, 1.0);
	
	return fResult;
}



uniform float mytime;
uniform int fogcolr=1; //  1=>white, 2=>brownish, 3=>purple, 4=>gray
uniform int foglevl=0; // 0=>noFog, 1=>Fog, 2=>heavy
uniform int darkness=0; // 0...4=darkest (bkgd)

const vec4 night = vec4(0.0,0.0,0.0,1.0);


#define MAX_ITER 8
void main() {

	vec4 vEyePos = vec4( aPos.xyz-eyePos, 1.0 );

	vec2 sp = vec2( 2*UV.x-1, 2*UV.y-1 );
	vec2 p = sp*5.0 - vec2(10.0);
	vec2 i = p;
	float c = 1.0;
	
	float inten = .1;

	for (int n = 0; n < MAX_ITER; n++) 
	{
		float t = 0.5*mytime * (1.0 - (3.0 / float(n+1)));
		i = p + vec2(cos(t - i.x) + sin(t + i.y), sin(t - i.y) + cos(t + i.x));
		c += 1.0/length(vec2(p.x / (sin(i.x+t)/inten),p.y / (cos(i.y+t)/inten)));
	}
	c /= float(MAX_ITER);
	c = 1.5-sqrt(c);
	color = vec4(vec3(c*c*c*c), 999.0) + vec4(0.0, 0.3, 0.5, 1.0);
	color.a = 0.9;


	//float dist2 = pow(vEyePos.x,2) + pow(vEyePos.z,2);
	//float fFogCoord = abs( sqrt(dist2)/vEyePos.w );
	float fFogCoord = length( vEyePos.xz );

	if( fogcolr==4 )
		color = mix(color, vDfogColor, getFogFactor(fFogCoord));
	else if( fogcolr==3 )
		color = mix(color, vMystColor, getFogFactor(fFogCoord));
	else if( fogcolr==2 )
		color = mix(color, vSootColor, getFogFactor(fFogCoord));
	else if( fogcolr==1 )
		color = mix(color, vFogColor, getFogFactor(fFogCoord));

	float df=0;
	if( darkness < 1 ) df=0.0;
	else if( darkness==1 ) df=0.3;
	else if( darkness==2 ) df=0.6;
	else if( darkness==3 ) df=0.8;
	else if( darkness>3 ) df=0.9;

	color = mix( color, night, df );

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

