#version 130

in vec3 position;
in vec3 normal;
in float a_centerFlag;
in float a_mazeDepth;

uniform mat4 u_mvpMat;
uniform float u_allowDepth;
uniform float u_time;

out float v_centerFlag;
out float v_mazeDepth;

// TODO: depth scale from uniform
void main()
{
    float d = 0.0;
    if (u_allowDepth > 0.0) {
        d = - a_mazeDepth / 2.0;
    }
    gl_Position = u_mvpMat * vec4((1.0+d) * position, 1.0);
    v_centerFlag = a_centerFlag;
    v_mazeDepth = a_mazeDepth;
}