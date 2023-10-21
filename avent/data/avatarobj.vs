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
uniform int direction; //-1=>back, 0=>stop, 1=>forward, 2=>prone
uniform int fade; // 0=>nofade, 1=>fade


const vec3 hRad = vec3(0.04,0.05,0.03); // head dimensions
const vec3 bRad = vec3(0.05,0.07,0.025); // body dimensions
const vec3 aRad = vec3(0.02,0.07,0.02); // arm dimensions
const vec3 lRad = vec3(0.02,0.08,0.02); // leg dimensions
const float fOffz = 0.80; // magnitude of stride



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
	float dy = 0.1*fOffz*cos(x); //subdued magnitude
	if (dir==0) dy=0.0;
	return dy;
} // end walking




//const float onepi = 3.14159;
const float onepi = 3.141592654;

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
	float dz, zsave;

	float angl = horiAng;
	vec3 pos = modelPos;

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
	



	if (id==0) {             //head

		pos.y -= 0.75; // Xlate to origin

		pos.x *= hRad.x;
		pos.y *= 4.0*hRad.y;
		pos.z *= hRad.z;
		// now is expanded to proper size

		pos.y += hRad.y+2.0*bRad.y; //move upward

	}

	else if (id==1) {             //torso

		pos.y -= 0.25; // Xlate to origin

		pos.x *= bRad.x;
		pos.y *= 4.0*bRad.y;
		pos.z *= bRad.z;
		// now is expanded to proper size

		pos.y += bRad.y; // move upward

	}

	else if (id==3) {            //right leg

		pos.x+=0.5;  pos.y+=0.5; pos.z-=0.5; // Xlate to origin

		pos.x *= 2.0*lRad.x;
		pos.y *= 2.0*lRad.y;
		pos.z *= 2.0*lRad.z;
		// now is expanded to proper size

		pos.x -= lRad.x; // move outboard
		pos.y -= lRad.y; // Xlate down

		pos.x -= 0.005; // move outboard slightly

	}
	else if (id==2) {            // left leg

		pos.x-=0.5;  pos.y+=0.5; pos.z-=0.5; // Xlate to origin

		pos.x *= 2.0*lRad.x;
		pos.y *= 2.0*lRad.y;
		pos.z *= 2.0*lRad.z;
		// now is expanded to proper size

		pos.x += lRad.x; // move outboard
		pos.y -= lRad.y; // Xlate down

		pos.x += 0.005; // move outboard slighty

	}

	else if (id==5) {            //right arm

		pos.x+=0.5;  pos.y+=0.5; pos.z+=0.5; // Xlate to origin

		pos.x *= 2.0*aRad.x;
		pos.y *= 2.0*aRad.y;
		pos.z *= 2.0*aRad.z;
		// now is expanded to proper size

		pos.x -= aRad.x+bRad.x; //move outboard
		pos.y -= aRad.y;  // center vertically

	}
	else if (id==4) {            // left arm

		pos.x-=0.5;  pos.y+=0.5; pos.z+=0.5; // Xlate to origin

		pos.x *= 2.0*aRad.x;
		pos.y *= 2.0*aRad.y;
		pos.z *= 2.0*aRad.z;
		// now is expanded to proper size

		pos.x += aRad.x+bRad.x; // move outboard
		pos.y -= aRad.y;      // center vertically

	}



if( direction<2 ) {


	// legs need to walk here, BEFORE rotation
	if ( (id==3 ) ) { //rightleg
		pos.z += pos.y * walkingDZ(15.0*mytime,direction);
	} else if ( (id==2) ) { //leftleg
		pos.z += pos.y * walkingDZ(15.0*mytime+onepi,direction);
	}


	// arms need to swing here, BEFORE rotation...
	// 26aug23 addendum: adjust arm pos.y
	// at both ends of arm swing.
	if ( (id==4) ) { // leftarm
		pos.z += 0.5*pos.y * walkingDZ(15.0*mytime,direction);
		pos.y += 0.5*pos.y * walkingDY(15.0*mytime,direction);
	} else if ( (id==5) ) { // rightarm
		pos.z += 0.5*pos.y * walkingDZ(15.0*mytime+onepi,direction);
		pos.y += 0.5*pos.y * walkingDY(15.0*mytime+onepi,direction);
	}


} // end if direction<2



	// set hands wider than shoulders
	if( id==4 ) //leftarm
		pos.x -= 0.1*pos.y;
	else if( id==5 ) //rightarm
		pos.x += 0.1*pos.y;


	if ( (id==4) || (id==5) ) //move arms upward
		pos.y += 2.0*bRad.y;   //shoulders to torso top


	// rotate per attitude
	posrot.y = pos.y;
	posrot.x = +cos(angl)*pos.x + sin(angl)*pos.z;
	posrot.z = -sin(angl)*pos.x + cos(angl)*pos.z;
	// now is rotated to proper look-direction



	if(direction==2) { // avatar is PRONE & dying from beetles

		// seems Ok:
		zsave = posrot.z;
		posrot.z = posrot.y;
		posrot.y = posrot.x;
		posrot.x = zsave;

	} // end PRONE



	// translate into position; 
	// & micro-adjust to put feet on ground.
	posrot += wPos;

	if(direction<2)
		posrot.y += 0.04+bRad.y+lRad.y;
	else
		posrot.y += 0.04;




	if( fade>0 ) opacity = 0.3; //showing glyph
	else opacity=1.0;

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

