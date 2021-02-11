#!/bin/sh

wget --no-clobber https://cdn.kernel.org/pub/linux/kernel/v5.x/linux-5.9.1.tar.xz
tar xf linux-5.9.1.tar.xz
cp dodo/benchmarks/linux-kernel/.config linux-5.9.1/
cp dodo/benchmarks/linux-kernel/Rikerfile linux-5.9.1/
