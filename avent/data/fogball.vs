#version 330 core
// vertex shader for fog ball

layout(location = 0) in vec2 vertPos;

out vec4 aPos;

uniform mat4 MVP;
uniform vec3 wPos;

uniform vec3 eyePos; ///////////new////////////


void main(){

	// centered @ origin:
	vec3 v3pos = vec3(vertPos,0.0);

	//billboarding...calculate desired normal:
	float angl = //ignoring vertical correction
		atan( eyePos.x-wPos.x, eyePos.z-wPos.z );

	//rotate per planarAngle:
	vec3 vpos;
	vpos.y = v3pos.y;
	vpos.x = +cos(angl)*v3pos.x + sin(angl)*v3pos.z;
	vpos.z = -sin(angl)*v3pos.x + cos(angl)*v3pos.z;

	//Xlate into position:
	vpos += wPos; 

	aPos = vec4(vpos,1.0);

	gl_Position =  MVP * aPos;
}

