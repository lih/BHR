#version 330 core

in vec4 fragmentColor;
in vec2 fragmentUV;
in vec3 fragmentNormal;

uniform sampler2D tileTexture;
uniform sampler2D tileTextureNormal;
uniform vec4 lightVect;
uniform mat4 projMat;
uniform mat4 viewMat;
uniform mat4 modelMat;

vec3 reflect(vec3 u,vec3 v) {
  float duv = dot(u,v);
  if(duv!=0) 
    return (2*dot(u,u)/duv)*v - u;
  else
    return -u;
}

void main() {
  vec4 texCol = texture(tileTexture,vec2(1) - fragmentUV);
  
  vec3 texNorm_raw = texture(tileTextureNormal,vec2(1) - fragmentUV).xyz;
  vec3 texNorm = reflect(reflect(texNorm_raw,vec3(0,0,1)),vec3(0,0,1)+fragmentNormal);

  // vec3 reflLight = normalize(reflect((viewMat * modelMat * lightVect).xyz,texNorm));

  float luminosity = clamp(dot(texNorm,(projMat * viewMat * modelMat * lightVect).xyz),0,1);
  
  // gl_FragColor = vec4(reflLight.xyz,1);
  gl_FragColor = vec4((fragmentColor.rgb * fragmentColor.a + texCol.rgb) * luminosity / (1+fragmentColor.a),texCol.a);
  // gl_FragColor = fragmentColor;
}
