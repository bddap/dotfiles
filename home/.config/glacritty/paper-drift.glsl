float hash(vec2 p) {
    vec3 q = fract(vec3(p.xyx) * 0.1031);
    q += dot(q, q.yzx + 33.33);
    return fract((q.x + q.y) * q.z);
}

float noise(vec2 p) {
    vec2 i = floor(p);
    vec2 f = fract(p);
    f = f * f * (3.0 - 2.0 * f);
    return mix(mix(hash(i), hash(i + vec2(1.0, 0.0)), f.x),
               mix(hash(i + vec2(0.0, 1.0)), hash(i + vec2(1.0, 1.0)), f.x), f.y);
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec2 p = fragCoord;
    float t = iTime * 0.04;
    float cloud = 0.6 * noise(p / 150.0 + vec2(t, 0.3 * t)) + 0.4 * noise(p / 40.0 - vec2(0.5 * t, t)) - 0.5;
    float weave = 0.5 * (noise(p * vec2(0.55, 0.07)) + noise(p * vec2(0.07, 0.55))) - 0.5;
    float grain = hash(floor(p)) - 0.5;
    vec3 paper = vec3(0.910, 0.882, 0.851);
    vec3 light = vec3(0.996, 0.961, 0.933);
    vec3 shade = vec3(0.847, 0.765, 0.647);
    vec3 col = paper + (light - shade) * (0.30 * cloud + 0.25 * weave + 0.10 * grain);
    vec2 uv = fragCoord / iResolution.y;
    float ochre = noise(uv * 1.5 + vec2(0.7 * t, 0.0));
    float teal = noise(uv * 1.2 + vec2(5.0, -0.5 * t));
    col = mix(col, vec3(0.580, 0.361, 0.008), 0.16 * ochre * ochre);
    col = mix(col, vec3(0.173, 0.443, 0.482), 0.14 * teal * teal);
    fragColor = vec4(col, 1.0);
}
