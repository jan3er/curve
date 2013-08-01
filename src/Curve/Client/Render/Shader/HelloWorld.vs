#version 130

uniform mat4 uModelMatrix;
uniform mat4 uViewMatrix;
uniform mat4 uProjectionMatrix;

in vec3 vPosition;
in vec3 vNormal;
in vec2 vTexCoord;

out vec3 fNormal;
out vec2 fTexCoord;

void main()
{
    gl_Position =  uProjectionMatrix * uViewMatrix * uModelMatrix * vec4(vPosition,1);
    fNormal = mat3(uProjectionMatrix * uViewMatrix * uModelMatrix) * vNormal;
    fTexCoord = vTexCoord;
}
