#version 330 core

in vec3 vertexPosition;
in vec3 vertexNormal;
in vec4 vertexColor;
in vec2 vertexUV;
out vec4 fragmentColor;
out vec2 fragmentUV;
out vec3 fragmentNormal;

uniform mat4 viewMat;
uniform mat4 modelMat;
uniform mat4 projMat;

void main() {
    gl_Position = projMat * viewMat * modelMat * vec4(vertexPosition,1);
    fragmentUV = vertexUV;
    fragmentColor = vertexColor;
    fragmentNormal = (modelMat * vec4(vertexNormal,0)).xyz;
}
