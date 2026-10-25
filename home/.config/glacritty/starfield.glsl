float hash(vec2 p) {
    return fract(sin(dot(p, vec2(127.1, 311.7))) * 43758.5453);
}

vec3 layer(vec2 uv, float scale, float speed) {
    uv = uv * scale + vec2(iTime * speed, 0.0);
    vec2 cell = floor(uv);
    vec2 f = fract(uv) - 0.5;
    float h = hash(cell);
    vec2 offset = vec2(hash(cell + 1.3), hash(cell + 7.1)) - 0.5;
    float d = length(f - offset * 0.7);
    float star = step(0.85, h) * smoothstep(0.08, 0.0, d);
    float twinkle = 0.6 + 0.4 * sin(iTime * 3.0 + h * 40.0);
    return vec3(0.8, 0.85, 1.0) * star * twinkle;
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec2 uv = fragCoord / iResolution.y;
    vec3 col = vec3(0.0, 0.0, 0.02);
    col += layer(uv, 12.0, 0.08) * 0.7;
    col += layer(uv, 24.0, 0.04) * 0.45;
    col += layer(uv, 48.0, 0.02) * 0.3;
    fragColor = vec4(col, 1.0);
}
