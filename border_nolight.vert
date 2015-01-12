#version 130

in vec3 position;
in vec3 normal;
in float a_centerFlag;

uniform mat4 u_mvpMat;

uniform float u_time;

out float v_centerFlag;

void main()
{
    gl_Position = u_mvpMat * vec4(position, 1.0);
    v_centerFlag = a_centerFlag;
}