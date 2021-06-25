#!/bin/sh

# LSB Test Suite installation script
# This script handles the installation of the TET3/VSXgen test
# harness and desired test suites.
#
# This is $Revision: 1.9 $
#
# $Log: install.sh,v $
# Revision 1.9  2001/08/09 02:19:48  cyeoh
# Improve error message regarding byacc installation and make suggestion
# as to how to resolve the problem (as suggested by Matt Taggart).
#
# Revision 1.8  2001/08/01 18:50:11  cyeoh
# Fix autodetect code. Adds better error detection
#
# Revision 1.7  2001/08/01 14:57:28  cyeoh
# Add autodetection of latest version of tet/vsxgen harness
# so we don't have to keep editing this script
#
# Revision 1.6  2001/05/29 05:43:21  cyeoh
# Update for new release
#
# Revision 1.5  2001/05/04 04:08:08  cyeoh
# update for version numbers
#
# Revision 1.4  2001/01/19 00:31:02  cyeoh
# Fixes workaround for groupadd to work properly.
#
# Revision 1.3  2001/01/12 07:37:30  cyeoh
# adds workaround for different groupadd and usermod programs
#
# Revision 1.2  2000/11/09 04:36:36  cyeoh
# Adds check for correct version of yacc
# Adds warning notice about the use of test suite results to
# claim compliance.
#
# Revision 1.1  2000/08/29 05:43:42  cyeoh
# Script for automating install of test suite harness and
# test sets for LSB
#

######################################################################
# Useful functions

######################################################################
# Add a group. Try to be tolerant of different groupadd arguments
add_single_group()
{
  if [ -n "$GROUP_ID" ]; then
    GROUP_ID=$(($GROUP_ID+1))
    GROUP_FLAGS="-g $GROUP_ID"
  fi
  set -x
  groupadd $GROUP_FLAGS $1
  GROUP_ADD=$?
  set +x
  if [ $GROUP_ADD -ne 0 -a -z "$GROUP_ID" ]; then
    grep $1 /etc/group
    if [ $? -ne 0 ]; then
      # work out a group id we can use
      # Serious dodgy algorithm but avoids the problem of 
      # nogroup often being set to the last valid gid
      echo
      echo Possibly detected different group add implementation
      echo Trying to workaround by supplying -G flag
      echo
      GROUP_ID=`cut -f3 -d: /etc/group | sort -n | tail -2 | head -1`;
      GROUP_ID=$(($GROUP_ID+10))
      GROUP_FLAGS="-g $GROUP_ID"
      set -x
      groupadd $GROUP_FLAGS $1
      set +x
    fi
  fi
}

uidsetup() 
{
  # add the maximum number of groups for a user
  echo "Adding the groups"
  add_single_group  vsxg0
  add_single_group  vsxg1
  add_single_group  vsxg2
  # note that this is to NGROUP_MAX , vsxg0 + NGROUP_MAX supp groups
  add_single_group  supp1
  add_single_group  supp2
  add_single_group  supp3
  add_single_group  supp4
  add_single_group  supp5
  add_single_group  supp6
  add_single_group  supp7
  add_single_group  supp8
  add_single_group  supp9
  add_single_group  supp10
  add_single_group  supp11
  add_single_group  supp12
  add_single_group  supp13
  add_single_group  supp14
  add_single_group  supp15
  add_single_group  supp16
  add_single_group  supp17
  add_single_group  supp18
  add_single_group  supp19
  add_single_group  supp20
  add_single_group  supp21
  add_single_group  supp22
  add_single_group  supp23
  add_single_group  supp24
  add_single_group  supp25
  add_single_group  supp26
  add_single_group  supp27
  add_single_group  supp28
  add_single_group  supp29
  add_single_group  supp30
  add_single_group  supp31
  
  echo "Adding the users"
  
  set -x
  
  # you may prefer the ksh to sh
  useradd -g vsxg0 -d $TET_ROOT/test_sets -s /bin/sh -c "VSX0 test login" vsx0
  # We add the supplementary groups separately just in case the vsx0
  # user already exists but without the required groups

  # If we have a GROUP_ID set then we're probably on a system that has a
  # modified usermod command as well.
  if [ -z "$GROUP_ID" ]; then
    USER_MOD_FLAG="-G"
  else
    USER_MOD_FLAG="-g"
  fi

  usermod $USER_MOD_FLAG supp1,supp2,supp3,supp4,supp5,supp6,supp7,supp8,supp9,supp10,supp11,supp12,supp13,supp14,supp15,supp16,supp17,supp18,supp19,supp20,supp21,supp22,supp23,supp24,supp25,supp26,supp27,supp28,supp29,supp30,supp31 vsx0
  useradd -g vsxg1 -d $TET_ROOT -s /bin/sh -c "VSX1 test login" vsx1
  useradd -g vsxg2 -d $TET_ROOT -s /bin/sh -c "VSX2 test login" vsx2
  set +x
  echo "Enter the password for the vsx0 user"
  passwd vsx0
}

# Sanity configuration check
SanityChecks()
{
  # Checks for correct version of yacc
  type yacc > /dev/null 2>&1
  
  if [ $? -ne 0 ]; then
    echo "*ERROR*: yacc (byacc) must be installed on the system."
    exit 1
  fi

  yacc --version > /dev/null 2>&1
  if [ $? -eq 0 ]; then
    echo "*ERROR*: Must have byacc and not bison installed as yacc on the system."
    echo 
    echo "A common way of working around this problem is to install the byacc"
    echo "package and putting a symlink from yacc to byacc in your path"
    echo "before the system installed yacc. There may be a better distribution"
    echo "specific way of doing this and you may want to consult your"
    echo "system administrator."
    exit 1
  fi

}

######################################################################

# Check we're running as root
if  [ $EUID -ne 0 ]; then
  echo "This installation script must be run as root"
  exit 1
fi

PATH=$PATH:/usr/sbin
export PATH

echo "LSB Test suite installation"
echo "---------------------------"
echo 
echo "IMPORTANT NOTE: These test suites are a work in progress,"
echo "no claims of LSB compliance or passing these test suites should be"
echo "made. These are unapproved test suites and issues remain both with"
echo "the test suites and the specifications under test. "
echo
echo "If you are installing a new version of the test harness or the last"
echo "installation failed for any reason it is recommended that you first"
echo "remove any the old installation."
echo

# Do sanity checks on installation to save problems later
SanityChecks

# Find out where to install the test suite
printf "Enter root directory of test suite installation: [/home/tet] "
read cmd
if [ "$cmd" = "" ]; then
  cmd=/home/tet
fi
if [ ! -d $cmd ]; then
  mkdir $cmd
  if [ $? -ne 0 ]; then
    echo "Unable to create installation directory: $cmd"
    exit 1
  fi
fi
TET_ROOT=$cmd
export TET_ROOT

# Check that user id's are setup
# We check that vsx1 user exists instead of vsx0 as some
# test suites have more stringent user and group requirments
# than others
grep vsx1 /etc/passwd 2>&1 >/dev/null
if [ $? -ne 0 ]; then
  printf "The required users and groups are not setup correctly\n"
  printf "Do you wish to install the user and groups ..?[y]"
  read cmd
  if [ "$cmd" = "Y" -o "$cmd" = "y" -o "$cmd" = "" ] ; then
      uidsetup
  else
    echo "Please setup required users and groups manually and rerun"
    echo "the installation script"
  fi
else
  echo "Setting vsx0, vsx1 and vsx2 users' home directories"
  echo "You may need to re-login any existing shells for these users"
  usermod -d $TET_ROOT/test_sets vsx0
  usermod -d $TET_ROOT vsx1
  usermod -d $TET_ROOT vsx2
fi

# Install the tet and vsxgen package
# Default to latest version of tarball available
tet_package=`ls -1 tet_vsxgen_* 2> /dev/null | tail -1`
tet_package=${tet_package:-""}
if [ $tet_package ]; then
  tet_package="$PWD/$tet_package"
fi
echo "Installing the TET and VSXgen test harness"
printf "TET and VSxgen package to install [$tet_package]: "
read cmd
if [ "$cmd" != "" ]; then
  tet_package=$cmd
fi
if [ ! -f "$tet_package" ]; then
  echo "Could not find file \"$tet_package\""
  echo Aborting
  exit 1
fi


(cd $TET_ROOT && tar xfz $tet_package)
if [ $? -ne 0 ]; then
  echo "Unable to unpack TET/VSXgen"
  exit 1
fi

# Install the test suites
test_suites=`echo lts_*.tgz | sed -e s/\ +//`
echo installing test_suites: $test_suites
printf "Test suites to be installed [$test_suites]: "
read cmd
if [ "$cmd" != "" ]; then
  test_suites=$cmd
fi

current_dir=$PWD;
for test_suite in $test_suites; do
  echo "Installing $test_suite"
  (cd $TET_ROOT/test_sets && tar xfz $current_dir/$test_suite)
  if [ $? -ne 0 ]; then
    echo "Unable to install test suite: $test_suite"
  fi
done;

# Configure setup script and profiles
sed -e "s@^echo Unconfigured@TET_ROOT=$TET_ROOT@" $TET_ROOT/setup.sh.skeleton \
  > $TET_ROOT/setup.sh && rm $TET_ROOT/setup.sh.skeleton
sed -e "s@^echo Unconfigured@TET_ROOT=$TET_ROOT@" $TET_ROOT/profile.skeleton \
  > $TET_ROOT/profile && rm $TET_ROOT/profile.skeleton
sed -e "s@^echo Unconfigured@TET_ROOT=$TET_ROOT@" $TET_ROOT/test_sets/profile.skeleton  > $TET_ROOT/test_sets/profile && rm $TET_ROOT/test_sets/profile.skeleton

# Set ownership correctly
echo Setting up ownership and permissions correctly
chown -R vsx0.vsxg0 $TET_ROOT
chmod u+x $TET_ROOT/setup.sh

# Display licence information
echo ----------------------------------------------------------------------
echo Licencing Conditions for test harness
echo 

sh $TET_ROOT/Licence.brief
printf "Press Enter to continue. "
read cmd

echo
echo ----------------------------------------------------------------------
echo Licencing Conditions for test suites
echo 

for i in $TET_ROOT/test_sets/scripts/*; do
  if [ -x $i/display_licence_brief.sh ]; then
    $i/display_licence_brief.sh
    printf "Press Enter to continue. "
    read cmd
    echo -------------------------------------------------------------------
  fi
done;

printf "\n\nYou should now login as the vsx0 user and run $TET_ROOT/setup.sh\n"
echo "Note that additional test suites can be installed by unpacking the"
echo "test suite tarball in $TET_ROOT/test_sets as the vsx0 user and re-running"
echo "$TET_ROOT/setup.sh (this will be ../setup.sh from the home directory)."


