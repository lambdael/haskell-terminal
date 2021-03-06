#version 330

layout(location = 0) in vec3 vPosition;
layout(location = 1) in vec4 vColor;
uniform mat4 mat;
uniform float time;
out vec4 col;


void main()
{
  float t = 1.0 - mod (time, 0.5) + 0.1;
  float tt = t * t;
  col = vColor * vec4(1.0,1.0,1.0,tt);
  vec4 pos = vec4(vPosition, 1.0);

  // gl_Position = mat * pos;

  gl_Position =pos;
}

