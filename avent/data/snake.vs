#version 330 core

layout(location = 0) in vec3 vertexPosition_modelspace;
layout(location = 1) in vec2 vertexUV;

out vec2 UV;

uniform mat4 MVP;
uniform vec3 wPos;  // orbit center
uniform float rad;  // orbit radius
uniform float angl; // angle
uniform float wvel; // wiggle-freq (nom=1.0)
uniform float wamp; // wiggle-amplitude (nom=1.0)

//const float onepi = 3.14159;
const float onepi = 3.141592654;


float wrad( float ang )
{
	//24 wiggles per twopi path around unit radius
	float dr = wamp*0.005*sin(sqrt(rad)*wvel*24*ang)/sqrt(rad); 
	return rad*(1+dr);
}

// wiggle component of final radius as ftn of 
// Z-offset=arclength away from ang:
float zwrad( float ang, float dz )
{
	float dang = atan(dz/rad);
	return wrad(ang+dang);
}


// note:  Z = long dimension => arclength delta from ang

void main(){

	float ang = angl;

	vec3 pos = vertexPosition_modelspace;

	// we assume original setup @ origin
	// ...move to orbit center
	pos.x += wPos.x;
	pos.y += wPos.y;
	pos.z += wPos.z;

	// original vector of current pt from center:
	float ddx = pos.x - wPos.x;
	float ddz = pos.z - wPos.z; //long direction delta
	float ddy = pos.y - wPos.y;

	// rotated vector of pt from center:
	float ddxx = +cos(ang)*ddx - sin(ang)*ddz;
	float ddzz = +sin(ang)*ddx + cos(ang)*ddz;

	// rotate snake to match tangent of trajectory circle:
	pos.x = wPos.x + ddxx;
	pos.z = wPos.z + ddzz;

	float zdr = zwrad(ang,ddz);

	// this displaces entire snake to move in circle:
	pos.x = pos.x + zdr*cos(ang);
	pos.z = pos.z + zdr*sin(ang);


	gl_Position =  MVP * vec4(pos,1.0);
	UV = vertexUV; 
}

