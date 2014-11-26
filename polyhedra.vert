#version 130

in vec3 position;
in vec3 normal;
in vec3 alt_position;
in float a_barycentric;

uniform mat4 u_mvpMat;
uniform mat4 u_pMat;
uniform float u_lightIntensity;

uniform float u_time;

out float v_barycenter;
out float intensity;

void main()
{
    // static light almost from the cam
    vec4 l_dir = u_lightIntensity * u_pMat * vec4(0.0, 0.0, 1.0, 0.0);
    vec4 n = normalize(u_mvpMat * vec4(normal,0.0));
    intensity = max(dot(n, l_dir), 0.0);

    float alpha = 0.5 + 0.5 * cos(u_time);
    gl_Position = u_mvpMat * vec4(alpha*position + (1-alpha)*alt_position, 1.0);
    v_barycenter = a_barycentric;
}