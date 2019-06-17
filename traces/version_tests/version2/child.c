#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>

int main (void) {
	int fd = open("test.txt", O_RDWR|O_CREAT);
	char buf[8];	
	write(fd, "test", 4);
	buf[5] = '\0';
	//printf("%s\n", buf);
}

