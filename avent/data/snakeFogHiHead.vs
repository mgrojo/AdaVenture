#version 330 core

//Experimental version that attempts to raise the
//snake head high

layout(location = 0) in vec3 vertexPosition_modelspace;
layout(location = 1) in vec2 vertexUV;

out vec2 UV;
out vec4 aPos; // crazy experiment



uniform mat4 MVP;
uniform vec3 wPos;  // orbit center
uniform float rad;  // orbit radius
uniform float angl; // angle
uniform float wvel; // wiggle-freq (nom=1.0)
uniform float wamp; // wiggle-amplitude (nom=1.0)

//const float onepi = 3.14159;
const float onepi = 3.141592654;
const float halfpi = 0.5*onepi;

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


float sqr(float x) { return x*x; }

// note abs(dz)<0.6
// begin lift 0 @ +0.0 [sin(-pi/2)]
// end lift 0.2 @ +0.6 [sin(pi/2)]
float ywrad( float dz, float dy ) // -0.6 ... +0.6, -0.2 ... +0.2
{

if( dz<0.0 ) return 0.0;
else {

	const float r21=sqrt(2.0) - 1.0; // 0.414
	const float dzr=0.6;
	const float dzh=dzr/2.0; // 0.3
	const float mxHt=0.16; // 0.22

	float rawang = (dz-dzh) / dzh; // -1..+1 on [0.0..0.6]
	clamp(rawang, -1.0, 1.0);

		float thickness=1.0 - abs(rawang); // 0..1..0
		float t2 = r21*sqr(thickness); // 0 .. 0.414 .. 0
		float yadj = dy*(1.0+t2); // dy .. sqrt(2)*dy .. dy

	float ang = rawang*halfpi;
	float rawheight = 1.0 + sin(ang);
	//float scaledHeight = yadj + mxHt*rawheight;
	float scaledHeight = mxHt*rawheight;

	return scaledHeight;

}

}



/*

float ywrad( float dz, float dy ) // dz in -0.6 ... +0.6
{
	// enhance thickness of snake near zcen
	float rawht;
	const float mxHt=0.3; //4dec17
	const float zmx=0.6;
	const float zcen=0.5*zmx;

	if( dz<0.0 ) return 0.0;
	else {

		float mp1=(dz-zcen)/(zcen); // -1 .. +1
		clamp(mp1, -1.0, 1.0);

		float thickness=1.0 - abs(mp1); // 0..1..0
		float t2 = sqr(sqr(thickness)); // 0..1..0

		float yadj = dy*(1.0+t2); // dy .. 2dy .. dy

		if( mp1<0.0 ) rawht=-sqrt(1.0-sqr(mp1+1.0)); //-1 .. 0
		else          rawht=+sqrt(1.0-sqr(mp1-1.0)); // 0 .. +1

		float scaledHt = yadj + mxHt*0.5*(rawht+1.0);

		return scaledHt;

	}
}
*/



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
	float ydr = ywrad(ddz,ddy);

	// this displaces entire snake to move in circle:
	pos.x = pos.x + zdr*cos(ang);
	pos.z = pos.z + zdr*sin(ang);
	pos.y += ydr;

	aPos = vec4(pos,1.0);
	gl_Position  =  MVP * aPos;
	UV = vertexUV; 
}

