#version 130

in vec3 position;
in vec3 normal;
in vec3 alt_position;
in vec3 a_barycentrics;

uniform mat4 u_mvpMat;
uniform mat4 u_vMat;
uniform vec4 u_lightDirection;
uniform float u_lightIntensity;

uniform float u_time;

out vec3 v_barycentrics;
out float intensity;

void main()
{
    // for specular like lighting
    // vec4 n = normalize(u_mvpMat * vec4(normal,0.0));

    vec4 n = normalize(u_vMat * vec4(normal,0.0));
    intensity = max(dot(n, u_lightIntensity * u_lightDirection), 0.0);

    float alpha = 0.5 + 0.5 * cos(u_time);
    gl_Position = u_mvpMat * vec4(alpha*position + (1-alpha)*alt_position, 1.0);
    v_barycentrics = a_barycentrics;
}