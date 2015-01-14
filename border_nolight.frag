#version 130

uniform vec4 u_color;
uniform vec4 u_borderColor;
uniform float u_borderWidth;
uniform float u_allowDepth;

in float v_centerFlag;
in float v_mazeDepth;

out vec4 color;

float edgeFactor(const float thickness, const float centerFlag)
{
    //vec3 d = fwidth(bary);
    //vec3 a3 = smoothstep(vec3(0.0), d*thickness, bary);
    //return min(min(a3.x, a3.y), a3.z);
    return smoothstep(0.0, fwidth(centerFlag)*thickness, centerFlag);
}

void main()
{
    float f = edgeFactor(u_borderWidth, v_centerFlag);
    float g = 1.0;
    if (u_allowDepth > 0.0) {
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