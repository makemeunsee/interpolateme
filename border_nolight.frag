#version 130

uniform vec4 u_color;
uniform vec4 u_borderColor;

uniform vec3 u_color0;
uniform vec3 u_color1;
uniform vec3 u_color2;
uniform vec3 u_color3;

uniform float u_borderWidth;
uniform float u_depthMode;

uniform float u_litFace;
uniform float u_litDepth;

in float v_centerFlag;
in float v_mazeDepth;
in float v_faceId;

out vec4 color;

float edgeFactor(const float thickness, const float centerFlag)
{
    return smoothstep(0.0, fwidth(centerFlag)*thickness, centerFlag);
}

vec3 colorShade(const float sDepth, const vec3 col) {
    if (sDepth > 1.0) {
        return vec3(0.0,0.0,0.0);
    } else {
        return (1.0 - sDepth) * col;
    }
}

vec4 shade(const float depth) {
    return vec4( colorShade(3.0 * depth, u_color0)
               + colorShade(3.0 * abs(0.33-depth), u_color1)
               + colorShade(3.0 * abs(0.66-depth), u_color2)
               + colorShade(3.0 * (1.0-depth), u_color3)
               + vec3(0.01,0.01,0.01)
               , 1.0);
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
    if (u_litFace == v_faceId && u_litDepth == v_mazeDepth) {
        color = 2*(color + vec4(0.1,0.1,0.1,0.0));
    }
}