#!/usr/bin/env python3

import os

os.mkdir('src')

FILES = 100

f = open('src/main.c', 'w')
for i in range(0, FILES):
  f.write('extern void fun'+str(i)+'();\n')

f.write('\n')
f.write('int main() {\n')
for i in range(0, FILES):
  f.write('  fun'+str(i)+'();\n')
f.write('}\n')
f.close()

for i in range(0, FILES):
  f = open('src/file'+str(i)+'.c', 'w')
  f.write('#include <stdio.h>\n')
  f.write('\n')
  f.write('void fun'+str(i)+'() {\n')
  f.write('  printf("Hello from fun'+str(i)+'!\\n");\n')
  f.write('}\n')
  f.close()

