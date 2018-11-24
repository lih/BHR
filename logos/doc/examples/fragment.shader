#version 330 core

in vec4 fragmentColor;
in vec2 fragmentUV;

uniform sampler2D tileTexture;

void main() {
  vec4 texCol = texture(tileTexture,vec2(1) - fragmentUV);
  gl_FragColor = vec4((fragmentColor.rgb * fragmentColor.a + texCol.rgb) / (1+fragmentColor.a),texCol.a);
  // gl_FragColor = fragmentColor;
}
