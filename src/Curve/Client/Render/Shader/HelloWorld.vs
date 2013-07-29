#version 130

uniform mat4 uModelMatrix;
uniform mat4 uViewMatrix;
uniform mat4 uProjectionMatrix;

in vec3 vPosition;

void main()
{
    gl_Position = uProjectionMatrix * uViewMatrix * uModelMatrix * vec4(vPosition,1);
//	gl_Position = uViewMatrix * uModelMatrix * vPosition;
}
