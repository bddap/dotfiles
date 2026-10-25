void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec2 uv = fragCoord / iResolution.xy;
    float t = iTime * 0.15;
    float band = 0.0;
    for (int i = 0; i < 3; i++) {
        float fi = float(i);
        float y = 0.55 + 0.12 * sin(uv.x * (2.0 + fi) + t * (1.0 + 0.4 * fi) + fi * 1.7);
        band += exp(-pow((uv.y - y) * (9.0 + 3.0 * fi), 2.0)) * (0.6 - 0.15 * fi);
    }
    vec3 sky = mix(vec3(0.01, 0.02, 0.05), vec3(0.03, 0.05, 0.10), uv.y);
    vec3 glow = mix(vec3(0.05, 0.45, 0.30), vec3(0.30, 0.15, 0.50), uv.x + 0.3 * sin(t));
    fragColor = vec4(sky + glow * band * 0.5, 1.0);
}
