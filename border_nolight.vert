#version 130

in vec3 position;
in vec3 normal;
in float a_centerFlag;
in float a_mazeDepth;

uniform mat4 u_mvpMat;
uniform float u_time;
uniform float u_depthMode;
uniform float u_depthScale;
uniform float u_explodedFactor;

out float v_centerFlag;
out float v_mazeDepth;

void main()
{
    float d = 0.0;
    if (u_depthMode > 0.0) {
        d = a_mazeDepth * u_depthScale;
    } else if (u_depthMode < 0.0) {
        d = - a_mazeDepth * u_depthScale;
    }
    gl_Position = u_mvpMat * vec4((1.0+d) * (position + ((u_explodedFactor - 1.0) * normal)), 1.0);
    v_centerFlag = a_centerFlag;
    v_mazeDepth = a_mazeDepth;
}