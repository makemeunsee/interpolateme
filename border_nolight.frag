#version 130

uniform vec3 u_color;
uniform vec3 u_borderColor;
uniform float u_borderWidth;
uniform float u_depthMode;

uniform float u_litFace;
uniform float u_litDepth;

in float v_centerFlag;
in float v_mazeDepth;
in float v_faceId;
in vec3 v_depthModeColor;

out vec4 color;

float edgeFactor(const float thickness, const float centerFlag)
{
    return smoothstep(0.0, fwidth(centerFlag)*thickness, centerFlag);
}

void main()
{
    float f = edgeFactor(u_borderWidth, v_centerFlag);
    vec3 baseColor = u_color;
    if (u_depthMode != 0.0) {
        baseColor = v_depthModeColor;
    }
    color = vec4 (
        mix(u_borderColor,
            baseColor,
            f),
        1.0
    );
    if (u_litFace == v_faceId && u_litDepth == v_mazeDepth) {
        color = 2*(color + vec4(0.1,0.1,0.1,0.0));
    }
}