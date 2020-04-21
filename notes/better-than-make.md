# Things we (can) do better than make

* Any file that is created by a build, and then later modified by a user should get a warning that it will be overwritten on a subsequent rebuild.
    - there is no way to do this in make; it will just do whatever writes is asked of it
    - so this is a safety property that we can maintain that make cannot
* We can skip rerunning children of commands marked for rerun
    - make only knows about file mtimes
    - we won't run it if file contents do not change (depends on content fingerprinting)