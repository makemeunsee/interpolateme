#version 130

attribute vec3 position;
attribute float a_barycentric;

uniform mat4 u_worldView;

varying float v_barycenter;

void main()
{
    gl_Position = u_worldView * vec4(position, 1.0);
    v_barycenter = a_barycentric;
}