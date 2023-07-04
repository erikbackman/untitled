#shader vertex
#version 330 core

layout (location = 0) in vec3 position;
layout (location = 1) in vec3 a_color;

uniform mat4 u_MVP;

out vec3 in_color;

void main()
{
    gl_Position = u_MVP * vec4(position, 1.0);
    in_color = a_color;
}

#shader fragment
#version 330 core

out vec4 frag_color;
in vec3 in_color;
uniform vec3 u_Color;

void main()
{
    // frag_color = vec4(in_color, 1.0);
  frag_color = vec4(1.0, 1.0, 1.0, 1.0);
}
