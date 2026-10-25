void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec2 p = (fragCoord - 0.5 * iResolution.xy) / iResolution.y * 3.0;
    float t = iTime * 0.3;
    float v = sin(p.x + t) + sin(p.y * 1.3 - t) + sin(length(p + vec2(sin(t), cos(t))) * 2.0);
    vec3 col = 0.5 + 0.5 * cos(v + vec3(0.0, 2.1, 4.2));
    fragColor = vec4(col * 0.18, 1.0);
}
