#version 330

layout(location = 0) in vec3 vPosition;
layout(location = 1) in vec4 vColor;
uniform mat4 mat;
uniform float time;
out vec4 col;


void main()
{
  col = vColor ;
  vec4 pos = vec4(vPosition, 1.0);

  // gl_Position = mat * pos;

  gl_Position =pos;
}
