# Incremental Build Experiment: `redis`

## Setting up
Clone git repository
```
$ git clone git@github.com:redis/redis
```

Check out the starting commit (30 commits before 6.2.2)
```
$ git checkout d96f47cf06b1cc24b82109e0e87ac5428517525a
```

Use the Rikerfile-redis-serial from the case-studies directory

Run a full build
```
$ rkr
```

Now hop through these revisions
- cd03e293c3f5a0351cc3203f50cc3d578eebc23f
- a0e19e3cf153ffbd8b1c7e72249d22b92f71532f
- 5e3a15ae1b58630a10639b34cd016ba9c0ff6b15
- 175a9e3199f79305827e826b5d2da3ed5d8a3502
- 733daef127de95d9c8042997913546fe82b7df23
- 38da8d07d0f6f90f7983fe215c98ed1266357291
- c07e16fadd2de67be99b627b07310f7015c1085a
- b278e443761df27a6e425ec4652f33bd494ceb94
- 4938052f6b65a44b8832c5f7e530d84ed4277a96
- c6cd1e59b1e1dff4f6b47e5d1946ab752738c1b6
- d7920ff9b16b1b3cec5838581d8499d0d50dc935
- 645c664fbbaad462f482328818c42cf7460805be
- 07601b7e0737446d9a678623d1afa24ac8a1d20e
- 0a2621c673df184ef6486119ea43736f9703987d
- d63d02601f5b6ba1b149fdd686c99c20649b9916
- 7d749d810b327cc0ac02df8821ccf11ab54a0a9b
- 374401d7866685679acaee95ed8357287136039e
- a60016e0619d489f1c282aad6acd36f9d44e3459
- f4b5a4d8698cfee563ee0c054bd97ef5b46c5d26
- a9897b0084919d85c64e6a41a0fea0f882550760
- c0f5c678c2e848899427160cf468e3727f0b752d
- 0413fbc7d015d2ad9a4f57cc50a68e581f0e7744
- 3a955d9ad443adcb1d0495f4b702612f6b250f6e
- 7a3d1487e443c3cb5fde3605aebea20698a940b4
- f40ca9cb58049683b563245006892001a5ebfacd
- 53a4d6c3b152a9aa6386c2362400b258cec09ed3
- 61d3fdb474bdb292f8ba40d8aafa175431f26a00
- f5ca1f9ee9c92b004d8a7b641da0a4e502cc4a7d
- aa730ef1eadf7e7e603315f8a204927d33eb2be1


TODO: mkreleasehdr.sh is exposing some weird broken behavior with pipes. Just track all the commands that start with a reference to a pipe and put them all in each others' "needs output from" sets. That way everything runs together. That's probably the right approach anyway.
