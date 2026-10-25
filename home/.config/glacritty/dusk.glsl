void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec2 uv = fragCoord / iResolution.xy;
    vec3 col = mix(vec3(0.10, 0.04, 0.10), vec3(0.02, 0.05, 0.12), uv.x * 0.4 + uv.y * 0.6);
    col *= 1.0 - 0.5 * dot(uv - 0.5, uv - 0.5);
    fragColor = vec4(col, 1.0);
}
