#version 130

uniform vec4 u_color;
uniform vec4 u_borderColor;

in float v_barycenter;
in float intensity;

out vec4 color;

float edgeFactor(const float thickness, const float bary)
{
    //vec3 d = fwidth(bary);
    //vec3 a3 = smoothstep(vec3(0.0), d*thickness, bary);
    //return min(min(a3.x, a3.y), a3.z);
    return smoothstep(0.0, fwidth(bary)*thickness, bary);
}

void main()
{
    float f = edgeFactor(1.2, v_barycenter);
    vec4 ambient = 0.15 * mix(
        u_color,
        u_borderColor,
        f
    );
    vec4 diffuse = intensity * mix(
        u_borderColor,
        u_color,
        f
    );
    color = vec4(max(diffuse.rgb, ambient.rgb), 1.0);
    //color = vec4(smoothstep(vec3(0.0), fwidth(v_barycenter)*5, v_barycenter) , 1.0);
}