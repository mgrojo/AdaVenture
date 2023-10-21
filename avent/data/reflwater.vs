#version 330 core

layout(location = 0) in vec3 vertexPos;

out vec3 position;
out vec3 worldNormal;

uniform mat4 MVP;
uniform float waterlevel;
uniform float mytime;
uniform vec3 wPos;
uniform vec3 wRad;



const float pi = 3.141592654;

const int numWaves=4;


// believable rectangular waves for rectangular pool:
const float amplitude[4] = float[4]( 0.0022, 0.002, 0.0015, 0.0017 );
const float wavelength[4] = float[4]( 0.018, 0.02, 0.024, 0.016 );
const float speed[4]      = float[4]( 0.0055, 0.005, 0.004, 0.0045 );
const float dir[4] = float[4]( 0, pi/2, -pi/2, pi );


const float diry[4] = float[4]( sin(dir[0]), sin(dir[1]), sin(dir[2]), sin(dir[3]) );
const float dirx[4] = float[4]( cos(dir[0]), cos(dir[1]), cos(dir[2]), cos(dir[3]) );


float wave(int i, float x, float y) {
    float frequency = 2*pi/wavelength[i];
    float phase = speed[i] * frequency;
    float theta = dot( vec2( dirx[i], diry[i] ), vec2(x, y));
    return amplitude[i] * sin(theta*frequency+mytime*phase);
}

float waveHeight(float x, float y) {
    float height = 0.0;
    for (int i = 0; i < numWaves; ++i)
        height += wave(i, x, y);
    return height;
}

float dWavedx(int i, float x, float y) {
    float frequency = 2*pi/wavelength[i];
    float phase = speed[i] * frequency;
    float theta = dot( vec2( dirx[i], diry[i]), vec2(x, y));
    float A = amplitude[i] * dirx[i] * frequency;
    return A * cos(theta * frequency + mytime * phase);
}

float dWavedy(int i, float x, float y) {
    float frequency = 2*pi/wavelength[i];
    float phase = speed[i] * frequency;
    float theta = dot( vec2( dirx[i], diry[i]), vec2(x, y));
    float A = amplitude[i] * diry[i] * frequency;
    return A * cos(theta * frequency + mytime * phase);
}

vec3 waveNormal(float x, float y) {
    float dx = 0.0;
    float dy = 0.0;
    for (int i = 0; i < numWaves; ++i) {
        dx += dWavedx(i, x, y);
        dy += dWavedy(i, x, y);
    }
    vec3 n = vec3(-dx, 1.0, -dy); //my Y plays roll of Conrod's Z
    return normalize(n);
}


void main(){
	vec4 pos = vec4(vertexPos,1);

	float nx = (pos.x - wPos.x)/wRad.x;
	float nz = (pos.z - wPos.z)/wRad.z;

  	pos.y = waterlevel + waveHeight(nx,nz);
  	worldNormal = waveNormal(nx, nz);

   position = pos.xyz;
   gl_Position = MVP * pos;

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

