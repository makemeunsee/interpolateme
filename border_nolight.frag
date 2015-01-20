#version 130

uniform vec4 u_color;
uniform vec4 u_borderColor;
uniform float u_borderWidth;
uniform float u_depthMode;

in float v_centerFlag;
in float v_mazeDepth;

out vec4 color;

float edgeFactor(const float thickness, const float centerFlag)
{
    return smoothstep(0.0, fwidth(centerFlag)*thickness, centerFlag);
}

void main()
{
    float f = edgeFactor(u_borderWidth, v_centerFlag);
    float g = 1.0;
    if (u_depthMode > 0.0) {
        g = 0.1 + 0.9 * v_mazeDepth;
    } else if (u_depthMode < 0.0) {
        g = 0.1 + 0.9 * (1.0 - v_mazeDepth);
    }
    color = vec4 (
        g * mix(
            u_borderColor,
            u_color,
            f).rgb,
        1.0
    );
}