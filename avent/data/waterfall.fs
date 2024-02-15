#version 330 core
// waterfall frag.shader

in vec2 UV;
in vec4 aPos;

uniform vec3 eyePos;

out vec4 color;

uniform sampler2D sprite;
uniform int iflag;


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
	color = texture( sprite, UV).rgba;

	if( ( iflag==1 ) && (color.a < 0.3) ) discard; 
	//high threshold necessary using spark.png!


	if( iflag==0 ) //ribbon (wfall.png)
		color.a *= 0.8;

// currently NOT modifying particle waterfall due to fog or darkness...
// which is perfect because particles disappear at a distance anyway.

if( iflag==0 ) { //ribbon wfall.png

	float asave=color.a;

	float df=0.0;
	if( darkness < 1 )     df=0.0;
	else if( darkness==1 ) df=0.3;
	else if( darkness==2 ) df=0.6;
	else if( darkness==3 ) df=0.8;
	else if( darkness>3 )  df=0.9;

	color = mix(color, night, df);



	if(foglevl>0) {

		const float yc=-0.5; // -0.5 = min land_alt
		float dist = exp(-1.0*abs(aPos.y-yc)) * length( aPos.xyz-eyePos );

		if( foglevl>2 ) 
			dist = exp(-0.5*abs(aPos.y-yc)) * length( aPos.xyz-eyePos );

		if     ( foglevl==1 ) asave*=1.2;
		else if( foglevl==2 ) asave*=1.2; //ch2
		else if( foglevl==3 ) asave*=1.5; //ch4,3
		else if( foglevl==4 ) asave*=2.0;

		asave = clamp(asave, 0.0, 1.0);

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

} // end if ribbon



}

