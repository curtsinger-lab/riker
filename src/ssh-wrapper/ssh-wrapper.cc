#include <iostream>

int main(int argc, char** argv)
{
// Hacky way to ensure correct ssh is used, need to ensure this is where user's ssh is installed
    std::string commandbuild = "/usr/bin/ssh";
    for (int i = 1; i < argc - 1; ++i)
        commandbuild = commandbuild + " " + argv[i];
    
    //commandbuild = commandbuild + " " + "\"cd ~/work; ./falsessh\"";
    commandbuild = commandbuild + " " + argv[argc - 1];
    const char *command = commandbuild.c_str();
    system(command);
    //cout << command;
    return 0;
}
