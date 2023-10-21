#version 330 core

// fragment shader

smooth in vec2 UV;
//smooth in vec4 eyeCoords;
smooth in vec4 aPos;

smooth in vec3 vNormal;
smooth in mat3 NormalMatrix; // only needed for moving objects


out vec4 color;


uniform sampler2D myTextureSampler;

uniform float hrad;
uniform vec3  hole;


/////// fog ////////////////////////////////////////////////////////

uniform int fogcolr=1; //  1=>white, 2=>brownish, 3=>purple, 4=>gray
uniform int foglevl=0; // 0=>noFog, 1=>Fog, 2=>heavy

uniform int darkness=0; // 0...4=darkest (bkgd)
const vec4 night = vec4(0.0,0.0,0.0,1.0);

const float fStart=0.0;
const float fEnd=16.0; // normal fog
const float sEnd= 8.0; // heavy fog
const float xEnd= 4.0; // extreme fog

const vec4 vFogColor=vec4(0.7,0.7,0.7,1.0); //normal white
const vec4 vDfogColor=vec4(0.3,0.3,0.3,1.0); //gray
const vec4 vSootColor=vec4(50.0/255,30.0/255,10.0/255,1.0); //brown
const vec4 vMystColor=vec4(0.7,0.5,0.7,1.0); //purple

float getFogFactor(float rng)
{
	float fResult = 0.0;

	if(foglevl==3) // extreme fog
		fResult = (xEnd-rng)/(xEnd-fStart);

	else if(foglevl==2) // heavy fog
		fResult = (sEnd-rng)/(sEnd-fStart);

	else if(foglevl==1) // normal fog
		fResult = (fEnd-rng)/(fEnd-fStart);
	else
		fResult=1.0;

	// at this point fResult=1 => transparent, fResult=0 => opaque

	
	fResult = 1.0-clamp(fResult, 0.0, 1.0);
	
	// at this point fResult=0 => transparent, fResult=1 => opaque

	return (sqrt(fResult));
}




////// lighting uniforms //////////////////////////////////////////
// NOTE:  some params that could be uniforms are fixed const;
//        Most are defaulted uniforms.
//        Of course, FOG & Darkness interfere with shine!

uniform int lightFlag=0; // 0=>no, 1=>yes

/////// ambient+diffuse light ///////
uniform vec3 ambCol = vec3(1.0,1.0,1.0);
uniform vec3 vPtPos = vec3(0.0, 3.0, 0.0);
uniform vec3 vPtCol = vec3(1.0,1.0,1.0);
const float fracAmb = 0.01;   //ambientCoefficient
const float fracMdif = 0.1;  //diffuseCoefficient (<=0.5)
const float fPtQuadAtt = 1.0; //2.2;

////// specular light //////
// material properties
uniform float Shininess = 64.0; //128.0; // wood=80
uniform float fracMspc=0.1; //0.1; // reflectivity
////// end lighting uniforms ///////////

uniform vec3 eyePos = vec3(0.0,1.0,0.0); //3jan18

//////// lighting ftns /////////////////////////

vec4 getAmbientLight( vec4 texCol ) // ambient
{
	vec4 ambient = vec4(ambCol,1.0)*texCol;
	return ambient;
}

vec4 getBlinnPhong( vec4 texCol, vec3 litePos ) // specular + diffuse
{
	vec3 liteCol=vPtCol;
	vec3 vPosToLight = litePos-aPos.xyz;
	float liteDist = length(vPosToLight);
	float attenuation = 1.0 / (1.0 + fPtQuadAtt*liteDist*liteDist);

// all vectors below here are in the Transformed Space
// i.e. AFTER applying NormalMatrix.

	vec3 surfaceToLight = normalize(NormalMatrix*vPosToLight);
	vec3 surfaceToCamera = normalize(eyePos - NormalMatrix*aPos.xyz);
	vec3 surfaceNormal = normalize(NormalMatrix*vNormal);

	float cosLitAngl = max(0.0, dot(surfaceNormal, surfaceToLight) );

	vec3 fDiffuse = vec3(0.0,0.0,0.0);
	vec3 fSpecular = vec3(0.0,0.0,0.0);

	if( cosLitAngl > 0.0 ) {

		vec3 halfDir = normalize(surfaceToLight + surfaceToCamera);
		float specAngl = max(dot(halfDir, surfaceNormal), 0.0);
		fSpecular = liteCol*fracMspc*pow(specAngl, Shininess);

		fDiffuse= liteCol*fracMdif*texCol.rgb*cosLitAngl;

	}

	vec3 linearColor = attenuation*(fDiffuse+fSpecular);
	//return vec4( linearColor, texCol.a); //no gamma-correction

	// finally, do gamma correction:
	vec3 gamma = vec3(1.0/2.2);
	vec3 finalColor = pow(linearColor,gamma);
	return vec4( finalColor, texCol.a );

}


//////// end lighting ftns /////////////////////////




bool insideSphere( vec3 cc, vec3 pt )
{
	float dist2 = 
		(cc.x-pt.x)*(cc.x-pt.x) +
		(cc.y-pt.y)*(cc.y-pt.y) +
		(cc.z-pt.z)*(cc.z-pt.z);
	if( dist2<hrad*hrad ) return true;
	else return false;
}









void main(){
	color = texture( myTextureSampler, UV).rgba;

	// lighting addendum:
	if( lightFlag > 0 ) { // use light effects
		vec4 vLightColor = 
			getBlinnPhong(color, vPtPos+vec3( 0.0, 0.1, 0.0) ) +
			getBlinnPhong(color, vPtPos+vec3( 0.0, 0.2, 0.0) ) +
			getBlinnPhong(color, vPtPos+vec3( 0.0, 0.3, 0.0) ) +
			getAmbientLight(color);
		color = clamp( vLightColor, 0.0, 1.0);
	}



	if( hrad>0.1 ){
		if( insideSphere(hole,aPos.xyz) ) color.a=0.0;
	}

	if(color.a < 0.01) discard;



	//float dist = length( eyeCoords.xz );
	float dist = length( aPos.xyz-eyePos );
	const float iymax=3.0;
	// note: aPos.y in [-iymax,iymax]
	float altF = exp(-0.9 * (aPos.y+iymax)/iymax );
	dist *= altF;

	if( fogcolr==4 )
		color = mix(color, vDfogColor, getFogFactor(dist));
	else if( fogcolr==3 )
		color = mix(color, vMystColor, getFogFactor(dist));
	else if( fogcolr==2 )
		color = mix(color, vSootColor, getFogFactor(dist));
	else if( fogcolr==1 )
		color = mix(color,  vFogColor, getFogFactor(dist));


	float df=0;
	if( darkness < 1 ) df=0.0;
	else if( darkness==1 ) df=0.3;
	else if( darkness==2 ) df=0.6;
	else if( darkness==3 ) df=0.7;
	else if( darkness>3 ) df=0.8;
	float asave=color.a;
	color = mix(color, night, df); // night=black
	color.a = asave;


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

