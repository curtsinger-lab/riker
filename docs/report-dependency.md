# End of Summer Report for Dependency Project

**1. What was the goal for this task?**

The goal for this task is to fully utilize the dependency data collected by Riker through tracking every command that was ran in a build. This task includes adding a subcommand that can generate a list of dependent ubuntu packages that need to be installed before building the program and another subcommand that automatically all of these packages. Additionally, it also includes a subcommand that can automatically generate a Dockerfile that is fully compatible with vscode and can be used to build a docker container where the program can be built and run independently of its environment. 

**2. Why is this useful?**

Having an easy access to the dependent packages for the program is useful for developers because when the program is big and complicated, it usually takes lots of effort to keep track of all the dependencies. Also the cost of making a mistake is high because when the users of the program do not have all the dependencies installed, they usually will encounter exotic errors that do not directly lead to the solution. It could take hours to debug these issues. Additionally, using and creating Dockerfile have relatively steep learning curve. However, starting a docker container is much easier in vscode that through the command line. Now with riker, anyone can start and run their program easily in a container by clicking just a few buttons rather than learning all the docker syntax. 

**3. How does your implementation work?**

To improve efficiency, I decided to find the package for each dependent file concurrently. So I first create a special object called `SynchronizedFile` that can facilitate synchronized writing.  Every component that the riker interacts with during a build are stored in an object called artifact. To generate the list of dependencies, I first retrieve the list of artifacts through function `env::getArtifacts()`. 

Then I pass each artifact to a new thread and try to filter out the artifacts that are impossible to be dependencies that could be installed previously. These include directory, special artifacts such as `stdin`, Rikerfile, all the files in the current directory, `/proc/`, and `.gitconfig`. 

Then I try to use command `dpkg` to find the potential package that manages these artifacts. One issue I encountered is that certain artifacts have commited path such as `/usr/bin/ls` but this is a hardlink managed by the package rather than the specific path installed by the package. Usually when the hardlink of a file starts with `/usr/`, `/usr/bin/`, or `/usr/bin/local/`, its specific path will start with one of the other two prefix. So after making sure the alternative paths are the same file as the original file by checking their inode and device number, I check which package manages the two alternative paths when the original path is not found. 

One other possibility when there is no path found could be that the artifact comes from a git repository rather than a package. So I print out the corresponding github repository url in the `stdout` for reference. 

Finally, if the artifact does not lie in a git repository, I simply print the file path in the `stdout` to remind developers to put in additional instructions for installation of that file. All of the names of the packages found are written to a `SynchronizedFile` called `.rkr-deps`. 

As for automatic installation of all the dependencies, I assume that the file `.rkr-deps` already exists and I loop through every package listed in that file. I first use command `dpkg-query` to check if the package has already been installed. If not, I simply use command `sudo apt-get install` to install the package. 

I use the same information in `.rkr-deps` to generate the Dockerfile. I follow the main structure of the Dockerfile for riker while replacing the list of dependencies with the packages in `.rkr-deps`. 

**4. Describe one or two examples that demonstrate the new functionality you added.** 

When developers are finished with the production process, they should first build the program with `../riker/rkr`. This will let riker cache all the dependencies and store them as artifact. Afterwards, to get `.rkr-deps`, they can simply run `../riker/rkr generate-deps`. They can also run `../riker/rkr check-deps` to check the list of pacakges without opening `.rkr-deps`. They should push this file to their github repository, so that when users or other developers download the program, they will also download `.rkr-deps`. The users can run command `../riker/rkr install-deps` to install all the missing packages listed in `.rkr-deps`. To generate a Dockerfile, they simply need to run `../riker/rkr generate-deps -c`. This will generate a folder in the current directory called `.devcontainer` which contains both the `Dockerfile` and `devcontainer.json` which is required for vscode. Then they can use `Rebuild and Reopen in Container` command in vscode to build their container. 

**5. Describe next steps for this task, if there are any.**

Currently the Dockerfile generation is following a pretty strict format and it could be made more flexible to accomodate different needs. This would probably require a better understanding of docker and its options. There are also other functionality that could be achieved using the list of dependencies. One option would be automatic generation of a snap of the program. A snap is a bundle of an app and its dependencies that works without modification across Linux distributions. With the list of dependencies available, we can use snapcraft to create a snap that can be distributed to any users using a Linux distribution. 