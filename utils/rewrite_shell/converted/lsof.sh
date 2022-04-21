#!/bin/sh

 

# Build lib/liblsof.a
cd  lib  

rm  -f  *.o  
cc  -DLINUXV=54114 -DGLIBCV=231 -DHASIPv6 -DNEEDS_NETINET_TCPH -DHASUXSOCKEPT -DHASPTYEPT -DHASSOSTATE -DHASSOOPT -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -DHAS_STRFTIME -DLSOF_VSTR="5.4.114" -O -Wno-format-truncation  -c  *.c  
ar  cr  liblsof.a  *.o  
ranlib  liblsof.a  
rm  -f  *.o  

cd  ..  

# Generate version.h
rm  -f  version.h  
( echo  #define LSOF_BLDCMT ""  > version.h    ;  ) 
echo  #define LSOF_CC "cc"  >> version.h   
echo  #define LSOF_CCV "'`cc -v 2>&1 | sed -n 's/.*version \(.*\)/\1/p'`'"  >> version.h   
echo  #define	LSOF_CCDATE	"'`date`'"  >> version.h   
echo  #define LSOF_CCFLAGS "'`echo $CFLAGS | sed 's/\\\\(/\\(/g' | sed 's/\\\\)/\\)/g' | sed 's/"/\\\\"/g'`'"  >> version.h   
echo  #define LSOF_CINFO ""  >> version.h   
echo  #define LSOF_HOST "'`uname -n`'"  >> version.h   
echo  #define LSOF_LDFLAGS "-L./lib -llsof "  >> version.h   
echo  #define LSOF_LOGNAME "'`whoami`'"  >> version.h   
echo  #define LSOF_SYSINFO "'`uname -a`'"  >> version.h   
echo  #define LSOF_USER "'`whoami`'"  >> version.h   
sed  /VN/s/.ds VN \(.*\)/#define LSOF_VERSION "\1"/  < version   >> version.h   

# Now build the lsof executable
cc  -DLINUXV=54114 -DGLIBCV=231 -DHASIPv6 -DNEEDS_NETINET_TCPH -DHASUXSOCKEPT -DHASPTYEPT -DHASSOSTATE -DHASSOOPT -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -DHAS_STRFTIME -DLSOF_VSTR="5.4.114" -O -Wno-format-truncation  -o  lsof  *.c  -L./lib  -llsof  
