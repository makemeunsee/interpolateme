#version 130

in vec3 position;
in vec3 normal;
in vec3 a_barycentrics;

uniform mat4 u_mvpMat;

uniform float u_time;

out vec3 v_barycentrics;
out float intensity;

void main()
{
    gl_Position = u_mvpMat * vec4(position, 1.0);
    v_barycentrics = a_barycentrics;
}