#version 130

in vec3 position;
in float a_barycentric;

uniform mat4 u_worldView;

out float v_barycenter;

void main()
{
    gl_Position = u_worldView * vec4(position, 1.0);
    v_barycenter = a_barycentric;
}