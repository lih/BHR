#version 330 core

in vec4 fragmentColor;
in vec2 fragmentUV;
in vec3 fragmentNormal;

uniform sampler2D tileTexture;
uniform sampler2D tileTextureNormal;
uniform vec4 lightVect;

vec3 reflect(vec3 u,vec3 v) {
  float duv = dot(u,v);
  if(duv!=0) 
    return (2*dot(u,u)/duv)*v - u;
  else
    return -u;
}

void main() {
  vec4 texCol = texture(tileTexture,fragmentUV);
  
  vec3 texNorm_raw = texture(tileTextureNormal,fragmentUV).xyz;
  vec3 texNorm = reflect(reflect(texNorm_raw,vec3(0,0,1)),vec3(0,0,1)+fragmentNormal);

  float luminosity = clamp(dot(normalize(texNorm),lightVect.xyz),0,1);

  gl_FragDepth = gl_FragCoord.z / gl_FragCoord.w;
  gl_FragColor = vec4((fragmentColor.rgb * fragmentColor.a + texCol.rgb * luminosity) / (1+fragmentColor.a),1);
}
