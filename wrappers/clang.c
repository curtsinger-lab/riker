#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>

typedef struct linked_list{
    char * string;
    struct linked_list * next;
}linked_list;

void clang_wrapper(int argc, char* args[]) {
    char * supported_flags[13] = {"-D", "-f", "-I", "-o", "-W", "-pthread", "-g", "-M", "-O", "-std", "--std", "-pedantic", "-m"};
    char * supported_compile_flags[1] = {};
    char * supported_linker_flags[6] = {"-L", "-shared", "-Wl", "-l", "-r", "../deps/"};

    // Determine compiler and whether we are using C++
    char * compiler = args[0];
    char * last = strrchr(compiler, '/');
    if (last != NULL){
        compiler = last+1;
    }
    bool is_cpp = false;
    if (!strcmp(compiler, "clang") || !strcmp(compiler, "gcc") || !strcmp(compiler, "cc")) {
        is_cpp = false;
    } else if (!strcmp(compiler, "clang++") || !strcmp(compiler, "g++") || !strcmp(compiler, "c++")) {
        is_cpp = true;
    } else {
        printf("Error - unrecognized compiler: %s\n", compiler);
        return;
    }
    
    // Initialize arrays
    
    
    char * output_name = NULL;

    for (int i = 1; i < argc; i++) {
        char* arg = args[i];
        if (strcmp(arg, "-nowrapper") == 0){
            // todo
            char * path = getenv("PATH");
            
        } else if (strcmp(arg, "-o") == 0) {
            
        } else if (!is_cpp && (strcmp(strrchr(arg, '.'), ".c") == 0)) {
            
        } else if (is_cpp && ((strcmp(strrchr(arg, '.'), ".cc") == 0) || 
                              (strcmp(strrchr(arg, '.'), ".cpp") == 0) || 
                              (strcmp(strrchr(arg, '.'), ".c++") == 0))) {
            
        } else if ((output_name != NULL) && (arg == output_name)) {

        } else if (strcmp(strrchr(arg, '.'), ".0") == 0) {
            
        }

    }
}

int main(int argc, char* argv[]){
    printf("Using our C clang wrapper.\n");
    
    
    clang_wrapper(argc, argv);
    return 0;
}

