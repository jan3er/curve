#version 130

uniform vec3 uColor;

in vec3 fNormal;
in vec2 fTexCoord;

out vec4 oColor;

void main() 
{
    
    oColor = vec4(vec3(0.5) + 0.5*fNormal,1);
}

