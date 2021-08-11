# End of Summer Report for Environment-Variables Project 
 

**1. What was the goal for this task?** 

The goal for this project was to allow Riker to trace and store changes in the environment build-commands are run in, allowing for subsequent calls to the command to run in the same environment. We attempted to handle many cases for how environment variables could change between runs, creating a variety of test cases for the testing suite. 

**2. Why is this useful?** 

This project is useful because sometimes commands’ behavior changes depending on the environment/environment variables. Previously, Riker did not store the specific environment for each command, running every command in the default environment. The project allows Riker to build projects that contain environment-sensitive commands. 

**3. How does your implementation work?** 

The final version of our implementation first stores the default list of environment variables for the command `rkr` at the beginning of each build. Specifically, this happens in `DefaultTrace.cc`. Then, for each subsequent command, we compare the current list of environment variables with the default list of environment variables in function `getEnvDiff` located in the file `Command.cc`. There are five possible differences between two list of environment variables - add, delete, replace, append, prepend. We first check if a key in current list is already present in the default list. If it is not present, we are adding a new key. If the key is present but the values are different, we use function `analyzeChanges` to check if we are appending or prepending to the default value. 

Inside the `analyzeChanges` function, we first split the default value and the current value into two vectors of elements using their delimiter. Then we calculate their [Levenshtein Distance](https://en.wikipedia.org/wiki/Levenshtein_distance). From the `LevenshteinDistance` function, we get a 2D matrix where the bottom right corner cell denotes the number of edits required to change from default value to current value. To know what edits are performed in this transformation, we simply need to walk in the top right direction through the minimum number route. We found that going up in the beginning means appending and going up in the end means prepending. There are other situations such as going left, going diagonal and the number changes, going diagonal before and after going up. In these situations, we are neither prepending nor appending, so we just give up and record the replacement of the entire value for that environment variable. Otherwise, we record either append or prepend according to the algorithm. We delete each variable in the default list when we find the same one in the current list. In the end, all the variables left in the default list will be the ones that are deleted from the current list, so we record them as deletion.  

These changes to the environment variables will be stored as part of the command object. For subsequent rebuilds, we simply use function `getEnvironment` to retrieve this record and apply the changes to the default list of environment variables stored in the beginning of that rebuild to get an accurate list of environment variables needed for the current command.  

**4. Describe one or two examples that demonstrate the new functionality you added.** 

An example of how the previous Riker would have failed, where our project allows it to succeed, is where a command checks the value of an environment variable, changing the behavior of the command depending on the value present. If the dependent environment variable is modified by other commands, the environment variable might not have the intended value if the modifying command is not run (due to incremental builds) but the command that checks the variable is run. This creates a difference between expected behavior (if Riker ran every command each build) and the outcome. 

Additionally, this project also handles a variety of cases for where a commands environment differs compared to the default. Riker can now recognize when a variable has been added to the environment, if a variable has been deleted, or if a variable has been modified. If the variable contains a list of elements, Riker can detect if the front or the back of the list has had elements added. 

**5. Describe next steps for this task, if there are any.** 

While the handling of changes to environment variables accounts for many cases, it does not account for every possibility. One limitation of Riker is that it cannot check if an environment variable has changed if it does not result in different outcomes. For example, if a command depends on an environment variable, and we change the variable outside of Riker yet don’t change anything else, then Riker will detect no changes in the build and will not rerun any commands, even though the outputs would have been different. This case may be difficult to change, potentially involving fundamental changes to how Riker detects changes in the build, but it could be a worthwhile project for the future. 