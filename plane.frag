#version 130

uniform vec4 u_color;
uniform vec4 u_borderColor;

in vec3 v_barycentrics;

out vec4 color;

float edgeFactor(const float thickness, const vec3 bary)
{
    vec3 d = fwidth(bary);
    vec3 a3 = smoothstep(vec3(0.0), d*thickness, bary);
    return min(min(a3.x, a3.y), a3.z);
    //return smoothstep(0.0, fwidth(bary)*thickness, bary);
}

void main()
{
    float f = edgeFactor(5, v_barycentrics);
    color = mix(
        u_borderColor,
        u_color,
        f
    );
    // color = vec4(v_barycentrics, 1.0); // barycentrics debug render
}