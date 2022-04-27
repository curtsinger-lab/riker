# End of Summer Report for Orphan Adoption Project

**1. What was the goal for this task?**\
The goal was to enhance efficiency of Riker. Riker had a bug that whenever there is error compiling files, it treated the next successful build as brand new, i.e. redo every step, even though some files didn’t change. My task was to fix this bug, which is to let Riker skips unnecessary steps in the next successful build.

**2. Why is this useful?**\
Originally, when Riker has error compiling files, it deletes all the generated files and forgets all the links among files. Then even if programmers only edits one file, Riker will still treat the next build as a brand new build, i.e. compile every file again. Redoing unnecessary steps is a waste of time. After my fix, Riker should handle this case as following: it will memorize appropriate files and links when Riker has error compiling, and in the next build, it will skip unnecessary steps.

**3. How does your implementation work? (If you haven't finished, describe how it's meant to work)**\
The implementation hasn’t finished yet. Here is how it should work ideally. I created a variable, `maxChildren` that memorizes the set of children with maximum number throughout all the builds it experiences. Then whenever riker finish running a parent command, it will compare the `maxChildren` with the children of current build. If there are children from previous build which are not in current build, then we mark them as orphans and add them to `deferredCommand` set together with their refs. Storing refs is expected to save the link between parent and child so that, in the next full build, when parent will be able to look for child from `deferredCommand` set. If parent successfully retrieve orphans from deferred command set, then it will be able to skip the unnecessary step, which is the orphan command.

**4. Describe one or two examples that demonstrate the new functionality you added**\
Example: we have a main.c file and we compile successfully for the first time. With some edits, now main.c file contains bugs, so the second compilation fails. After we fix the bug in main.c file, (if main.c now has the same content as the first time compilation), riker should skip `as main.c` and `collect2`. 

**5. Describe next steps for this task, if there are any**\
The goal was to enhance efficiency of Riker. Riker had a bug that whenever there is error compiling files, it treated the next successful build as brand new, i.e. redo every step, even though some files didn’t change. My task was to fix this bug, which is to let Riker skips unnecessary steps in the next successful build.

The current implementation doesn’t work as expected because the `deferredSteps` of orphans are not added properly. Before adding, orphans are filtered because they are simulated. We couldn’t just take away the filter step. We need to figure out how to add `deferredSteps` of orphans to `deferredStep` set without being filtered.

The other problem we need to consider is how riker should react when the orphan is not adopted in the third build. With current implementation, will riker be able to adopt orphans in the fourth build if the third build still contains bugs?

The third thing is testing. Since the implementation hasn’t finished yet, I’m not able to test edge cases or cover all the conditions. If the implementation can be completed in the future, testing is definitely expected. 
