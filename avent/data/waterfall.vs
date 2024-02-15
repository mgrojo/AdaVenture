#version 330 core

// vertex shader for waterfalls

layout(location = 0) in vec2 vertPos; //billboard coords (x,y)
layout(location = 1) in vec2 texUV;

out vec2 UV;
out vec4 aPos; // FS uses only aPos.y (for fog)


////////////////////////////////////////////

uniform mat4 MVP;
uniform int iflag;

uniform vec3 wPos;


// center top of northern castle wall: ( -5.0, 4.0, 0.25)
const float zcenr = 0.27; //slightly away from wall
const float zcenp = 0.272; //slightly away from wall

// see adagate-preplevel.adb (near end)

void main(){

	if( iflag==0 ) //ribbon
		aPos = vec4( vertPos, zcenr, 1.0 );

	else if( iflag==1 ) //particles
		aPos = vec4( vertPos, zcenp, 1.0 );

	gl_Position = MVP * aPos;
	UV = texUV;
}


