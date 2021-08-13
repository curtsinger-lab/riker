# End of Summer Report for Clean Project

**1. What was the goal for this task?**

The goal was to create subcommands that mimic the functionality of `make clean`, which can do both of these two things -- removing all the files created during a build and ensuring the next build is a fresh one. Because riker keeps a cache of all the information of the build after the first build, riker is able to acheive these two functionalities in two separate commands, thus incorporating different needs. 

**2. Why is this useful?**

Even when using an incremental build software, developers often want to do a fresh build to clear out all the unusable caches. This is also useful when doing performance testing of riker because we only want to measure the time for the first build rather than subsequent build. Previously can only do it by running `rm -rf .rkr` which is not a standard cleaning command and could be unintuitive for new users. Additionally, when developers want to push to github repository, they usually want to clean up all the files created during a build because these files might not work in another environment. 

**3. How does your implementation work?**

To achieve the first functionality -- ensuring the next build is a fresh one -- I simply mask the current command `rm -rf .rkr` under a new command `../riker/rkr clean` so that it fully mimics `make clean`. 

To achieve the second functionality -- removing all the files created during a build -- I focus on the `DirArtifact`. Each `DirArtifact` has multiple `DirEntry` and each of these entries represent a file in the directory. I add a new field called `created_during_build` to the `DirEntry` object which stores a boolean value that tracks whether this entry is created during build or not. I update this field in function `DirArtifact::updateEntry` based on the paramter passed in. The function that calls `DirArtifact::updateEntry` is `DirArtifact::addEntry`. There are multiple places that call `DirArtifact::addEntry`, but only one place indicates that the file is newly created. So I add a new `DirArtifact::addEntry` where an additional boolean is passed in as parameter to indicate that the file is newly created. 

One issue I encountered is that function `DirArtifact::resolve` recurse on itself if there is more path left to resolve to make sure that the path is not a symlink but it causes the function to miss updating the entry sometimes. This issue was solved by checking directly before recursing that the current artifact is a file and updating the entry if it is. 

**4. Describe one or two examples that demonstrate the new functionality you added.** 

When a person builds the program with riker for the first time, it will take relatively longer time because riker needs to track all the commands and store all the information. Subsequent builds take shorter time because riker can just check `.rkr` to see if a file is modified or not without actually building it. Now if we run `../riker/rkr clean`, the folder `.rkr` will be deleted. So when we build the program again, it would be another fresh build that takes relatively longer time. 

If we only want to remove all the files created during a build (except `.rkr`), we can run `../riker/rkr clean -a`. Now all the object files and executables will be removed but subsequent build will still be relatively quick because we did not remove the cached information. 

**5. Describe next steps for this task, if there are any.**

There is no foreseeable next step for this task. 