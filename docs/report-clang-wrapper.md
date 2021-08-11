# End of Summer Report for Clang-Wrapper Project 

**1. What was the goal for this task?** 

The goal for this task is to improve the performance speed of Riker. Riker is better than the traditional Make build system because it does not require users to specify dependencies, and it performs incremental builds. However, in order to achieve this extra benefit, Riker needs to track every command ran in the Rikerfile, and that slows down the performance speed significantly comparing to traditional Make. Therefore, the goal of this project is to run compile commands in the Rikerfile concurrently rather than sequentially to improve performance.  

**2. Why is this useful?** 

Large programs tend to take a long time to compile. This can be frustrating when trying to fine-tune new features. Our work, combined with the baseline Riker implementation, speeds up the process of building a program, which is useful in any case where speed is important. Our work provides convenience and reduces a common source of frustration: long build times. 


**3. How does your implementation work?** 

When a shell command is invoked, like when typing `clang -o test test.c` in the terminal, the computer searches for the `clang` executable to run by searching a variety of directories. These directories are checked in the order they appear in the `PATH` environment variable, so for our compiler wrapper to take priority over the original compilers (`clang`, `gcc`, `clang++`, etc.) we added a path at the beginning of `PATH` pointing to our wrapper, meaning whenever `clang` in invoked, our wrapper runs instead. 

Once our wrapper is run, replacing a regular compile command, our first step is to parse the arguments. We iterate over each of the command arguments, comparing the string to a variety of cases and handling it accordingly. For example, if we find a `.c` or `.cc` file, we append it to a list of files to be compiled. If we see a `-o` flag, we know that the next argument will either be the name of the program to be linked, or the name of a `.c` file to be compiled when combined with the `-c` flag elsewhere. If a recognized flag is identified, it is stored as a list of flags to be included in compiling/linking. 

After the program has parsed the arguments, we have a list of C/C++ files to be compiled, it compiles each of the C/C++ files in parallel while mimicking the behavior of the original compilers. The wrapper iterates over each C/C++ file, invoking the original compiler in a new thread with the necessary flags and a single file to be compiled. If the `.o` files are to be linked at the end, the C/C++ files are compiled as temporary files and deleted after linking. If not, they are compiled regularly, leaving persistent `.o` files. Finally, the program links if necessary. 

 

**4. Describe one or two examples that demonstrate the new functionality you added.** 

By compiling the C/C++ files in parallel, we dramatically reduce the compile time compared to the prior serial compiling. This means that if a compile command has N files to be compiled, the runtime would be O(1) instead of O(N). 

This functionality will be turned on if one passes in the `--wrapper` flag when calling `./rkr`. For example, if we want to build a program called xz concurrently using Riker, we first need to make sure that there is a Rikerfile in the folder that builds the program. Then we simply run `../riker/rkr --wrapper`, assuming the riker directory is in the same level as the xz directory. Examples of speedup times can be found in [this spreadsheet](https://grinco.sharepoint.com/:x:/r/sites/CS-SystemsandLanguages-Riker/Shared%20Documents/Riker/Performance%20Time.xlsx?d=wf71c8f52f4284445b90c6f427019b12e&csf=1&web=1&e=KZxWug) (only available to those in the Riker Teams group). According to the data that we have collected in the spreadsheet, we can see that the performance time improves a lot when running `../riker/rkr` with wrapper comparing to running it without wrapper.  For example, the build time for the xz project sped up from 8.862 seconds to 5.206 seconds when using our wrapper. 

 

**5. Describe next steps for this task, if there are any.** 

For the next steps, we would want to do more testing by building a variety of programs. This would help us identify a more complete list of recognized flags, allowing the wrapper to be utilized in more projects. More testing would additionally help identify any bugs associated with edge cases that we might have missed. Finally, we could attempt to reduce the overhead of the wrapper in the argument parsing stage, like by evaluating the arguments in parallel when possible (flags) and in serial where necessary (`-o` flag). 