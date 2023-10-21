#version 330 core
layout(location = 0) in vec3 modelPos;
layout(location = 1) in vec2 modelUV;

out float opacity;
out vec2 UV;

// MVP must match string in glGetUniformLocation(pid,"MVP")
uniform mat4 MVP;
uniform float mytime;
uniform vec3 wPos;
uniform float horiAng;
uniform int direction; //-1=>back, 0=>stop, 1=>forward

// this is for the QuadriPedal Minotaur!

const vec3 hRad = vec3(0.16,0.20,0.12); // head dimensions
const vec3 bRad = vec3(0.16,0.28,0.08); // body dimensions
const vec3 aRad = vec3(0.06,0.24,0.08); // arm dimensions
const vec3 lRad = vec3(0.06,0.24,0.08); // leg dimensions 26aug23 fix
const float fOffz = 0.60; // magnitude of stride



// define a ftn giving zoffsets to feet
float walkingDZ( float tm, int dir ) {
	float x=tm*float(dir);
	float dz = fOffz*sin(x);
	if (dir==0) dz=0.0;
	return dz;
} // end walking

// define a ftn giving Yoffsets to feet
float walkingDY( float tm, int dir ) {
	float x=2.0*tm*float(dir);
	float dy = 0.2*fOffz*cos(x);
	if (dir==0) dy=0.0;
	return dy;
} // end walking




//const float onepi = 3.14159;
const float onepi = 3.141592654;
const float halfpi = 0.5*onepi;

// avatar is defined as a unit radius cube centered on origin
// made up of 6 rectangular parts (colored, not textured):
// Y>0.5 is head
// Y>0.0 is torso
//
// ---------------- bottom layer has 4 parts:
//
// leg leg    ^
// arm arm    |
//            Z
//      <---X
//





void main(){

	vec3 posrot;
	int id;
	float dz;

	float angl = horiAng;
	vec3 pos = modelPos;


	vec3 brad;
	brad.x=bRad.x;
	brad.y=bRad.z;
	brad.z=bRad.y;






// here, we identify body segment

	if (pos.y > 0.5)      id=0; //head
	else if (pos.y > 0.0) id=1; //torso

	else if (pos.z>0.0) //legs
	{
		if (pos.x>0.0) id=2; //left leg
		else           id=3; //right leg
	}

	else  //arms pos.z<0
	{
		if (pos.x>0.0) id=4; //left arm
		else           id=5; //right arm
	}


//////////// begin rotate torso ///////////////////////////////
	if(id==1) {

		pos.y -= 0.25; // Xlate to origin

		pos.x *= bRad.x;
		pos.y *= 4.0*bRad.y;
		pos.z *= bRad.z;
		// now is expanded to proper size

		//swap Y,Z [rotate 90deg forward]
		float sav=pos.y;
		pos.y=-pos.z;
		pos.z=sav;

		pos.y += brad.y; //lift to body pos

	}
//////////// end rotate torso ///////////////////////////////


	else if (id==0) {             //head

		pos.y -= 0.75; // Xlate to origin

		pos.x *= hRad.x;
		pos.y *= 4.0*hRad.y;
		pos.z *= hRad.z;
		// now is expanded to proper size

		pos.y += brad.y; //move upward to body top
		pos.z += hRad.z+brad.z; //move forward to body front

	}

	else if (id==3) {            //right leg

		pos.x+=0.5;  pos.y+=0.5; pos.z-=0.5; // Xlate to origin

		pos.x *= 2.0*lRad.x;
		pos.y *= 2.0*lRad.y;
		pos.z *= 2.0*lRad.z;
		// now is expanded to proper size

		pos.x -= 0.5*brad.x; //outward from center 26aug23 fix
		pos.y -= lRad.y; //lower to leg pos
		pos.z -= brad.z; // back to body rear

	}
	else if (id==2) {            // left leg

		pos.x-=0.5;  pos.y+=0.5; pos.z-=0.5; // Xlate to origin

		pos.x *= 2.0*lRad.x;
		pos.y *= 2.0*lRad.y;
		pos.z *= 2.0*lRad.z;
		// now is expanded to proper size

		pos.x += 0.5*brad.x; //outward from center 26aug23 fix
		pos.y -= lRad.y; //lower to leg pos
		pos.z -= brad.z; // back to body rear

	}

	else if (id==5) {            //right arm

		pos.x+=0.5;  pos.y+=0.5; pos.z+=0.5; // Xlate to origin

		pos.x *= 2.0*aRad.x;
		pos.y *= 2.0*aRad.y;
		pos.z *= 2.0*aRad.z;
		// now is expanded to proper size

		pos.x -= brad.x; //rightward to arm pos
		pos.y -= aRad.y; //lower to arm pos
		pos.z += brad.z; // forward to body front

	}
	else if (id==4) {            // left arm

		pos.x-=0.5;  pos.y+=0.5; pos.z+=0.5; // Xlate to origin

		pos.x *= 2.0*aRad.x;
		pos.y *= 2.0*aRad.y;
		pos.z *= 2.0*aRad.z;
		// now is expanded to proper size

		pos.x += brad.x; //leftward to arm pos
		pos.y -= aRad.y; //lower to arm pos
		pos.z += brad.z; // forward to body front

	}





// 26aug23: made gallop twice as fast...

// rear legs need to walk here, BEFORE rotation
	if ( (id==3 ) ) { //rightleg
		pos.z += pos.y * walkingDZ(10.0*mytime,direction);
		pos.y += 0.5*pos.y * walkingDY(10.0*mytime,direction);
	} else if ( (id==2) ) { //leftleg
		pos.z += pos.y * walkingDZ(10.0*mytime+halfpi,direction);
		pos.y += 0.5*pos.y * walkingDY(10.0*mytime+halfpi,direction);
	}

// front legs need to swing here, BEFORE rotation
	if ( (id==4) ) { // left front leg
		pos.z += 0.7*pos.y * walkingDZ(10.0*mytime+onepi,direction);
		pos.y += 0.3*pos.y * walkingDY(10.0*mytime+onepi,direction);
	} else if ( (id==5) ) { // right front leg
		pos.z += 0.7*pos.y * walkingDZ(10.0*mytime+onepi+halfpi,direction);
		pos.y += 0.3*pos.y * walkingDY(10.0*mytime+onepi+halfpi,direction);
	}

// head bobs in sync with hind legs
	if ( id==0 ) {
		pos.y += 0.05*walkingDZ(10.0*mytime,direction);
	}



	// rotate per horiAng [about Y-axis]
	posrot.y = pos.y;
	posrot.x = +cos(angl)*pos.x + sin(angl)*pos.z;
	posrot.z = -sin(angl)*pos.x + cos(angl)*pos.z;
	// now is rotated to proper look-direction


	// translate into position
	posrot += wPos;
	posrot.y += 0.09+brad.y+lRad.y; // fixed adjustment to put feet on ground



	opacity=1.0;

	gl_Position =  MVP * vec4(posrot,1.0);

	UV = modelUV;

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

