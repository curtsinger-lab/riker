#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>

int main (void) {
	//FILE *fp = fopen("test.txt", "ab+");
	char * buf = "test";	
	int fd = open("test.txt", O_RDWR|O_CREAT);
	char inbuf[8];
	read(fd, inbuf, 4);
	write(fd, buf, 4);
	//fwrite(buf, 8, 4, fp);
	char * args[2];
	char * cmd = "./child";
	args[0] = "./child";
	args[1] = NULL;
	if (execvp(cmd, args) == -1) {
		perror("YIKES: \n");
	}
}

