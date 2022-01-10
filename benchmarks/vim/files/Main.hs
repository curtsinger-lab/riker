module Main where

import Development.Rattle

main :: IO ()
main = rattleRun rattleOptions $ do
  cmd Shell "cd  src"
  cmd Shell "echo  /* pathdef.c */  > auto/pathdef.c"
  cmd Shell "echo  /* This file is automatically created by Makefile  >> auto/pathdef.c"
  cmd Shell "echo   * DO NOT EDIT!  Change Makefile only. */  >> auto/pathdef.c"
  cmd Shell "echo  #include \"vim.h\"  >> auto/pathdef.c"
  cmd Shell "echo  char_u *default_vim_dir = (char_u *)\"/usr/local/share/vim\";   |  sed  -e  s/[\\\\\"]/\\\\&/g  -e  s/\\\\\"/\"/  -e  s/\\\\\";$/\";/  -e  s/  */ /g  >> auto/pathdef.c"
  cmd Shell "echo  char_u *default_vimruntime_dir = (char_u *)\"\";   |  sed  -e  s/[\\\\\"]/\\\\&/g  -e  s/\\\\\"/\"/  -e  s/\\\\\";$/\";/  -e  s/  */ /g  >> auto/pathdef.c"
  cmd Shell "echo  char_u *all_cflags = (char_u *)\"'$CFLAGS'\";   |  sed  -e  s/[\\\\\"]/\\\\&/g  -e  s/\\\\\"/\"/  -e  s/\\\\\";$/\";/  -e  s/  */ /g  >> auto/pathdef.c"
  cmd Shell "echo  char_u *all_lflags = (char_u *)\"gcc '$LFLAGS' -o vim '$LIBS'\";   |  sed  -e  s/[\\\\\"]/\\\\&/g  -e  s/\\\\\"/\"/  -e  s/\\\\\";$/\";/  -e  s/  */ /g  >> auto/pathdef.c"
  cmd Shell "echo  char_u *compiled_user = (char_u *)\"   |  tr  -d  \\012  >> auto/pathdef.c"
  cmd Shell "(  ( (  logname   )   ||  whoami   )  )   |  tr  -d  \\012  >> auto/pathdef.c"
  cmd Shell "echo  \";  >> auto/pathdef.c"
  cmd Shell "echo  char_u *compiled_sys = (char_u *)\"   |  tr  -d  \\012  >> auto/pathdef.c"
  cmd Shell "hostname   |  tr  -d  \\012  >> auto/pathdef.c"
  cmd Shell "echo  \";  >> auto/pathdef.c"
  cmd Shell "sh  ./pathdef.sh"
  cmd Shell "sh  ./osdef.sh"
  cmd Shell "gcc  -L/usr/local/lib -Wl,--as-needed  -I. -Ilibvterm/include -Iproto -DHAVE_CONFIG_H -g -O2 -U_FORTIFY_SOURCE -D_FORTIFY_SOURCE=1 -DINLINE= -DVSNPRINTF=vim_vsnprintf -DSNPRINTF=vim_snprintf -DIS_COMBINING_FUNCTION=utf_iscomposing_uint -DWCWIDTH_FUNCTION=utf_uint2cells  -o  vim  arabic.c arglist.c autocmd.c beval.c buffer.c change.c blob.c blowfish.c cindent.c clientserver.c clipboard.c cmdexpand.c cmdhist.c crypt.c crypt_zip.c debugger.c dict.c diff.c digraph.c drawline.c drawscreen.c edit.c eval.c evalbuffer.c evalfunc.c evalvars.c evalwindow.c ex_cmds.c ex_cmds2.c ex_docmd.c ex_eval.c ex_getln.c fileio.c filepath.c findfile.c float.c fold.c getchar.c gui_xim.c hardcopy.c hashtab.c help.c highlight.c if_cscope.c if_xcmdsrv.c indent.c insexpand.c list.c locale.c map.c mark.c match.c mbyte.c memline.c menu.c misc1.c misc2.c mouse.c move.c normal.c ops.c option.c optionstr.c os_unix.c auto/pathdef.c popupmenu.c popupwin.c profiler.c pty.c quickfix.c regexp.c register.c screen.c scriptfile.c search.c session.c sha256.c sign.c sound.c spell.c spellfile.c spellsuggest.c syntax.c tag.c term.c terminal.c testing.c textformat.c textobject.c textprop.c time.c typval.c ui.c undo.c usercmd.c userfunc.c vim9compile.c vim9execute.c vim9script.c vim9type.c viminfo.c window.c bufwrite.c netbeans.c job.c channel.c xdiff/xdiffi.c xdiff/xemit.c xdiff/xprepare.c xdiff/xutils.c xdiff/xhistogram.c xdiff/xpatience.c charset.c json.c main.c memfile.c message.c version.c libvterm/src/encoding.c libvterm/src/keyboard.c libvterm/src/mouse.c libvterm/src/parser.c libvterm/src/pen.c libvterm/src/screen.c libvterm/src/state.c libvterm/src/unicode.c libvterm/src/vterm.c  -lSM -lICE -lXt -lX11 -lXdmcp -lSM -lICE -lm -ltinfo -lselinux -ldl"

