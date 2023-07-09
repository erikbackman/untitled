#shader vertex
#version 330 core

layout (location = 0) in vec3 position;
layout (location = 1) in vec4 a_color;

uniform mat4 u_MVP;

uniform mat4 u_model;
uniform mat4 u_proj;
uniform mat4 u_view;

out vec4 my_color;

void main()
{
    //gl_Position = u_MVP * vec4(position, 1.0);
    gl_Position = u_proj * u_view * u_model * vec4(position, 1.0);
    my_color = a_color;
}

#shader fragment
#version 330 core

out vec4 frag_color;
in vec4 my_color;

void main()
{
    frag_color = my_color;
    //frag_color = vec4(1.0, 1.0, 1.0, 1.0);
}
