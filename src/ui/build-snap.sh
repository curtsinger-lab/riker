#!/bin/bash

# Construct snap structure
# riker
# └── squashfs-root/
#    └── meta/
#        └── snapcraft.yaml
#    └── snap/
#        └── command-chain/
#            └── snapcraft-runner
rm -rf squashfs-root
mkdir squashfs-root
mkdir squashfs-root/meta
touch squashfs-root/meta/snapcraft.yaml
mkdir squashfs-root/snap
mkdir squashfs-root/snap/command-chain

# Install all executable files
# └── squashfs-root/
#    └── rkr
#    └── rkr-launch
#    └── rkr-inject.so
for file in `find $ROOT_DIR -maxdepth 1 -executable -type f`
do
    install -m 755 $file squashfs-root
done

#  Install all packges/file needed for runtime
# └── squashfs-root/
#    └── etc/
#        └── ld.so.cache
#    └── usr/
#        └── lib/
#            └── locale
#               └── locale-archive
install -d -m 755 squashfs-root/usr/lib/locale
install -m 755 /usr/lib/locale/locale-archive squashfs-root/usr/lib/locale
install -d -m 755 squashfs-root/etc
install -m 755 /etc/ld.so.cache squashfs-root/etc

# format snap-craft runner script
echo "#!/bin/sh
export PATH=\"\$SNAP/usr/sbin:\$SNAP/usr/bin:\$SNAP/sbin:\$SNAP/bin\${PATH:+:\$PATH}\"
export LD_LIBRARY_PATH=\"\$SNAP_LIBRARY_PATH\${LD_LIBRARY_PATH:+:\$LD_LIBRARY_PATH}\"
exec \"\$@\"" > squashfs-root/snap/command-chain/snapcraft-runner