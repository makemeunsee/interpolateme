#version 130

in vec3 position;
in vec3 alt_position;
in float a_barycentric;

uniform mat4 u_worldView;
uniform float u_time;

out float v_barycenter;

void main()
{
    float alpha = 0.5 + 0.5 * cos(u_time);
    gl_Position = u_worldView * vec4(alpha*position + (1-alpha)*alt_position, 1.0);
    v_barycenter = a_barycentric;
}