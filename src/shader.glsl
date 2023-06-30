#shader vertex
#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aColor;

out vec3 myColor;

void main()
{
    gl_Position = vec4(aPos, 1.0);
    myColor = aColor;
}

#shader fragment
#version 330 core
out vec4 FragColor;
in vec3 myColor;

void main()
{
    FragColor = vec4(myColor, 1.0);
}
