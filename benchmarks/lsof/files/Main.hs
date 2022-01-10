module Main where

import Development.Rattle

main :: IO ()
main = rattleRun rattleOptions $ do
  cmd Shell "cd  lib"
  cmd Shell "rm  -f  *.o"
  cmd Shell "cc  -DLINUXV=54114 -DGLIBCV=231 -DHASIPv6 -DNEEDS_NETINET_TCPH -DHASUXSOCKEPT -DHASPTYEPT -DHASSOSTATE -DHASSOOPT -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -DHAS_STRFTIME -DLSOF_VSTR=\"5.4.114\" -O -Wno-format-truncation  -c  *.c"
  cmd Shell "ar  cr  liblsof.a  *.o"
  cmd Shell "ranlib  liblsof.a"
  cmd Shell "rm  -f  *.o"
  cmd Shell "cd  .."
  cmd Shell "rm  -f  version.h"
  cmd Shell "( echo  #define LSOF_BLDCMT \"\"  > version.h    ;  )"
  cmd Shell "echo  #define LSOF_CC \"cc\"  >> version.h"
  cmd Shell "echo  #define LSOF_CCV \"'`cc -v 2>&1 | sed -n 's/.*version \\(.*\\)/\\1/p'`'\"  >> version.h"
  cmd Shell "echo  #define	LSOF_CCDATE	\"'`date`'\"  >> version.h"
  cmd Shell "echo  #define LSOF_CCFLAGS \"'`echo $CFLAGS | sed 's/\\\\\\\\(/\\\\(/g' | sed 's/\\\\\\\\)/\\\\)/g' | sed 's/\"/\\\\\\\\\"/g'`'\"  >> version.h"
  cmd Shell "echo  #define LSOF_CINFO \"\"  >> version.h"
  cmd Shell "echo  #define LSOF_HOST \"'`uname -n`'\"  >> version.h"
  cmd Shell "echo  #define LSOF_LDFLAGS \"-L./lib -llsof \"  >> version.h"
  cmd Shell "echo  #define LSOF_LOGNAME \"'`whoami`'\"  >> version.h"
  cmd Shell "echo  #define LSOF_SYSINFO \"'`uname -a`'\"  >> version.h"
  cmd Shell "echo  #define LSOF_USER \"'`whoami`'\"  >> version.h"
  cmd Shell "sed  /VN/s/.ds VN \\(.*\\)/#define LSOF_VERSION \"\\1\"/  < version   >> version.h"
  cmd Shell "cc  -DLINUXV=54114 -DGLIBCV=231 -DHASIPv6 -DNEEDS_NETINET_TCPH -DHASUXSOCKEPT -DHASPTYEPT -DHASSOSTATE -DHASSOOPT -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -DHAS_STRFTIME -DLSOF_VSTR=\"5.4.114\" -O -Wno-format-truncation  -o  lsof  *.c  -L./lib  -llsof"

