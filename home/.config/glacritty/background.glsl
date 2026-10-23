float hash(vec2 p) {
    return fract(sin(dot(p, vec2(12.9898, 78.233))) * 43758.5453);
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec2 uv = fragCoord / iResolution.y;
    float t = iTime * 0.05;
    vec2 a = vec2(0.4 + 0.3 * sin(t * 1.3), 0.5 + 0.3 * cos(t * 0.9));
    vec2 b = vec2(1.2 + 0.4 * cos(t * 0.7), 0.4 + 0.3 * sin(t * 1.1));
    float warm = exp(-3.0 * dot(uv - a, uv - a));
    float cool = exp(-3.0 * dot(uv - b, uv - b));
    vec3 col = vec3(0.91, 0.88, 0.85);
    col += vec3(0.06, 0.03, -0.015) * warm + vec3(-0.03, 0.0, 0.045) * cool;
    col += (hash(floor(fragCoord)) - 0.5) * 0.015;
    fragColor = vec4(col, 1.0);
}
