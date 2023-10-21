#version 330 core

uniform float time;

uniform vec2 resolution;



float field(in vec3 p,float s,  int idx) {
   float strength = 7. + .03 * log(1.e-6 + fract(sin(time/2) * 4373.11));
   float accum = s/4.;
   float prev = 0.;
   float tw = 0.;
   for (int i = 0; i < 26; ++i) {
      float mag = dot(p, p);
      p = abs(p) / mag + vec3(-.5, -.4, -1.5);
      float w = exp(-float(i) / 7.);
      accum += w * exp(-strength * pow(abs(mag - prev), 2.2));
      tw += w;
      prev = mag;
   }
   return max(0., 5. * accum / tw - .7);
}


vec3 nrand3( vec2 co )
{
   vec3 a = fract( cos( co.x*8.3e-3 + co.y )*vec3(1.3e5, 4.7e5, 2.9e5) );
   vec3 b = fract( sin( co.x*0.3e-3 + co.y )*vec3(8.1e5, 1.0e5, 0.1e5) );
   vec3 c = mix(a, b, 0.5);
   return c;
}


in vec2 mypos;
out vec4 fragColor;

//const float onepi = 3.14159;
const float onepi = 3.141592654;

// NOTE:  no inputs are passed from vertex shader !
// ...this frag shader shows a [rotating] star field
// wherever a textured surface would normally show
// [ at gl_FragCoord.xy ]


vec2 rotate(vec2 p, float a)
{
	return vec2(p.x * cos(a) - p.y * sin(a), p.x * sin(a) + p.y * cos(a));
}



void main()
	
{

   //vec2 uv = 2. * gl_FragCoord.xy / resolution.xy - 1.; // original line
	vec2 uv = mypos/3.0;


   vec2 uvs = uv * resolution.xy / max(resolution.x, resolution.y);
   vec3 p = vec3(uvs / 4., 0) + vec3(1., -1.3, 0.);
   p += .2 * vec3(sin(time/2 / 16.), sin(time/2 / 12.),  sin(time/2 / 128.));
  
   
   float freqs[4];
   freqs[0] = 0.05;
   freqs[1] = 0.3; 
   freqs[2] = 0.3;
   freqs[3] = 0.7; 
   
   float t = field(p,freqs[3], 26);
   float v = (1. - exp((abs(uv.x) - 1.) * 6.)) * (1. - exp((abs(uv.y) - 1.) * 6.));
   
    //Second Layer
   vec3 p2 = vec3(uvs / (4.+sin(time/2*0.11)*0.2+0.2+sin(time/2*0.15)*0.3+0.4), 1.5) + vec3(2., -1.3, -1.);
   p2 += 0.25 * vec3(sin(time/2 / 16.), sin(time/2 / 12.),  sin(time/2 / 128.));
   float t2 = field(p2,freqs[3], 18);
   vec4 c2 =  mix(.2, 0.2, v) * vec4(1.3 * t2 * t2 * t2 ,1.8  * t2 * t2 , t2* freqs[0], t2);
   
   
   //Let's add some stars
   //Thanks to http://glsl.heroku.com/e#6904.0
   vec2 seed = p.xy * 2.0;   
   seed = floor(seed * resolution.x);
   vec3 rnd = nrand3( seed );
   vec4 starcolor = vec4(pow(rnd.y,20.0));
  
   //Second Layer
   vec2 seed2 = p2.xy * 3.0;
   seed2 = floor(seed2 * resolution.x);
   vec3 rnd2 = nrand3( seed2 );
   starcolor += vec4(pow(rnd2.y,40.0));
   
   fragColor = mix(freqs[3]-.5, 1.,1.0) * vec4(1.5*freqs[2] * t * t* t , 1.2*freqs[1] * t * t, freqs[3]*t, 1.0) +c2+starcolor;
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

