module Main where

import Development.Rattle

main :: IO ()
main = rattleRun rattleOptions $ do
  cmd "cd  src"
  cmd "gcc  -std=gnu99 -O2 -Wall -Wextra -DLUA_COMPAT_5_3 -DLUA_USE_LINUX  -c"
  cmd "ar  rcu  liblua.a  lapi.o  lcode.o  lctype.o  ldebug.o  ldo.o  ldump.o  lfunc.o  lgc.o  llex.o  lmem.o  lobject.o  lopcodes.o  lparser.o  lstate.o  lstring.o  ltable.o  ltm.o  lundump.o  lvm.o  lzio.o  lauxlib.o  lbaselib.o  lcorolib.o  ldblib.o  liolib.o  lmathlib.o  loadlib.o  loslib.o  lstrlib.o  ltablib.o  lutf8lib.o  linit.o"
  cmd "ranlib  liblua.a"
  cmd "gcc  -std=gnu99 -O2 -Wall -Wextra -DLUA_COMPAT_5_3 -DLUA_USE_LINUX  -c  -o  lua.o  lua.c"
  cmd "gcc  -std=gnu99  -o  lua  lua.o  liblua.a  -lm  -Wl,-E  -ldl"
  cmd "gcc  -std=gnu99 -O2 -Wall -Wextra -DLUA_COMPAT_5_3 -DLUA_USE_LINUX  -c  -o  luac.o  luac.c"
  cmd "gcc  -std=gnu99  -o  luac  luac.o  liblua.a  -lm  -Wl,-E  -ldl"

