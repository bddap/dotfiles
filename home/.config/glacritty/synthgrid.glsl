void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec2 uv = (fragCoord - 0.5 * iResolution.xy) / iResolution.y;
    float horizon = -0.05;
    vec3 col = mix(vec3(0.06, 0.01, 0.08), vec3(0.01, 0.0, 0.03), uv.y + 0.5);
    float sun = smoothstep(0.24, 0.23, length(uv - vec2(0.0, 0.12)));
    float stripes = step(0.5, fract((uv.y - 0.12) * 28.0)) + step(0.12, uv.y);
    col += vec3(0.20, 0.04, 0.09) * sun * min(stripes, 1.0) * step(horizon, uv.y);
    if (uv.y < horizon) {
        float depth = 0.18 / (horizon - uv.y);
        vec2 g = vec2(uv.x * depth, depth + iTime * 0.6);
        vec2 line = abs(fract(g) - 0.5) / fwidth(g);
        float grid = 1.0 - min(min(line.x, line.y), 1.0);
        col += vec3(0.30, 0.05, 0.35) * grid * exp(-depth * 0.08);
    }
    fragColor = vec4(col, 1.0);
}
