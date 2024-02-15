#version 330 core


in vec2 UV;
in vec4 aPos;

uniform vec3 eyePos;

out vec4 color;

uniform sampler2D myTextureSampler;

uniform int fogcolr=1; //  0=>noFog, 1=>white, 2=>brownish, 3=>purple, 4=>gray
uniform int foglevl=0; // 0=>noFog, 1=>light, 1=>Fog, 2=>heavy

uniform int darkness=0; // 0...4=darkest (=bkgd#)
const vec4 night = vec4(0.0,0.0,0.0,1.0);






const float fStart=0.0;
const float fEnd=30.0; // normal fog
const float sEnd=12.0; // heavy fog
const float xEnd= 4.0; // extreme fog


const vec4 vFogColor=vec4(0.7,0.7,0.7,1.0); //normal white/gray
const vec4 vDfogColor=vec4(0.3,0.3,0.3,1.0); //normal white/gray
const vec4 vSootColor=vec4(50.0/255,30.0/255,10.0/255,1.0); //soot
const vec4 vMystColor=vec4(0.7,0.5,0.7,1.0); // purplish fog


float getFogFactor(float rng)
{

	//float fResult = (fEnd-rng)/(fEnd-fStart);
	float fResult = 0.0;

	if(foglevl==3) // extreme fog
		fResult = (xEnd-rng)/(xEnd-fStart);

	else if(foglevl==2) // heavy fog
		fResult = (sEnd-rng)/(sEnd-fStart);

	else if(foglevl==1) // normal fog
		fResult = (fEnd-rng)/(fEnd-fStart);
	else
		fResult=1.0;



	fResult = 1.0-clamp(fResult, 0.0, 1.0);
	
	return sqrt(fResult);
}






void main(){
	color = texture( myTextureSampler, UV).rgba;
	if( color.a < 0.1 ) discard;
	float asave=color.a;

	float df=0;
	if( darkness < 1 ) df=0.0;
	else if( darkness==1 ) df=0.3;
	else if( darkness==2 ) df=0.6;
	else if( darkness==3 ) df=0.8;
	else if( darkness>3 ) df=0.9;
	color = mix(color, night, df);

	if(foglevl>0) {

		const float yc=-0.5; // -0.5 = min land_alt
		float dist = exp(-1.0*abs(aPos.y-yc)) * length( aPos.xyz-eyePos );

		if( foglevl>2 ) 
			dist = exp(-0.5*abs(aPos.y-yc)) * length( aPos.xyz-eyePos );

		if( fogcolr==4 )
			color = mix(color,  vDfogColor, getFogFactor(dist));
		else if( fogcolr==3 )
			color = mix(color, vMystColor, getFogFactor(dist));
		else if( fogcolr==2 )
			color = mix(color, vSootColor, getFogFactor(dist));
		else if( fogcolr==1 )
			color = mix(color,  vFogColor, getFogFactor(dist));

	}

	color.a = asave;

}


//--
//-- Copyright (C) 2024  <fastrgv@gmail.com>
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

