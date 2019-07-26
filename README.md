```
    .--.
.--/ *  \
(--._   |     
 `   /  /_.----._  _
    /      __    \| )
   (       \-      (
    \_     /='     |
      \___    ____.'
          \ \/
          | |
         '" "`
```

To build, run
```
make
./dodo "make -B"
```

To rebuild, run
```
./dodo-build --unchanged src .
```

This will also output `out.dot`, which can be rendered using, e.g.
```
dot -Tpdf out.dot >out.pdf
```
