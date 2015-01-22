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

vec4 shade(const float depth) {
    if (depth < 0.33) {
        return vec4(0.01 + 3.0 * (0.33 - depth), 0.01 + 3.0 * depth, 0.01, 1.0);
    } else if (depth < 0.66) {
        return vec4(0.01, 0.01 + 3.0 * (0.33 - (depth-0.33)), 0.01 + 3.0 * (depth-0.33), 1.0);
    } else {
        return vec4(0.01 + 3.0 * (depth-0.66), 0.01, 0.01 + 3.0 * (0.33 - (depth-0.66)), 1.0);
    }
}

void main()
{
    float f = edgeFactor(u_borderWidth, v_centerFlag);
    vec4 baseColor = u_color;
    if (u_depthMode > 0.0) {
        baseColor = shade(v_mazeDepth);
    } else if (u_depthMode < 0.0) {
        baseColor = shade(v_mazeDepth);
    }
    color = vec4 (
        mix(
            u_borderColor,
            baseColor,
            f).rgb,
        1.0
    );
}