#version 130

uniform vec3 uColor;

in vec3 fNormal;
in vec2 fTexCoord;

out vec4 oColor;

void main() 
{
    oColor = vec4(fNormal,1);
}

