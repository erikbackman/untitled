#shader vertex
#version 330 core

layout (location = 0) in vec3 position;

void main()
{
    gl_Position = vec4(position, 1.0);
}

#shader fragment
#version 330 core

out vec4 color;
uniform vec3 u_Color;

void main()
{
    color = vec4(u_Color, 1.0);
}
