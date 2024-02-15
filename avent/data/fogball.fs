#version 330 core
// fragment shader for [billboarded] fog ball

in vec4 aPos; //actual vertex pos

out vec4 color;

uniform vec3 wPos;  // current centroid
uniform float wRad; // current radius

const vec4 vWhiteColor=vec4(0.7,0.7,0.9,1.0); //bluish tinge
const vec4 vOrangeColor=vec4(255.0/255,165.0/255,0.0/255,1.0); //agentOrange
const vec4 vSootColor=vec4(50.0/255,30.0/255,10.0/255,1.0); // brownish soot
const vec4 vSapGrnColor=vec4(80.0/255,125.0/255,42.0/255,1.0); // sap green
const vec4 vLimGrnColor=vec4(166.0/255,214.0/255,8.0/255,1.0); // lime green
const vec4 vMystColor=vec4(0.7,0.5,0.7,1.0); // purplish fog

void main(){

	color = vSapGrnColor;

	float dist = length( vec3(aPos.xyz-wPos) );

	dist /= wRad;
	dist = clamp( dist, 0.0, 1.0 );

	color.a = (1.0-dist); //awesome!

	if(color.a < 0.1) discard;

}

