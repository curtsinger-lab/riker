#!/usr/bin/env python3

import copy
import graphviz
import os
import sys
from typing import List, Dict, Set, Optional
import parse_strace as parser

class DuplicateProcessError(Exception):
    def __init__(self, e: parser.Event) -> None:
        self.e = e
    
    def __str__(self) -> str:
        return 'Process {} already exists.\n  {}'.format(self.e.pid, e)

class ProcessNotFoundError(Exception):
    def __init__(self, e: parser.Event) -> None:
        self.e = e
    
    def __str__(self) -> str:
        return 'Process {} was not found.\n  {}'.format(self.e.pid, e)

class Context:
    def __init__(self, starting_dir: str) -> None:
        self.starting_dir = starting_dir
        self.commands: List[Command] = []
        self.processes: Dict[int, Process] = {}
        self.files: Set[str] = set()
    
    def handle_event(self, e: parser.Event) -> None:
        # Handle the entry point by creating a process and command
        if len(self.processes) == 0 and e.name == 'execve' and e.retval == 0:
            cmd = Command(self, e.args)
            self.commands.append(cmd)
            self.processes[e.pid] = Process(self, self.starting_dir, cmd)
        elif e.pid not in self.processes:
            print(self.processes.keys())
            raise ProcessNotFoundError(e)
        else:
            self.processes[e.pid].handle_event(e)
    
    def to_graph(self) -> graphviz.Digraph:
        g = graphviz.Digraph()
        
        # Generate nodes for files
        for f in self.files:
            g.node(f, os.path.basename(f), shape='rectangle')
        
        # Generate nodes for all base commands
        for c in self.commands:
            c.to_graph(g)
        
        return g

class Command:
    def __init__(self, context: Context, args) -> None:
        self.context = context
        self.args = args
        self.children: List[Command] = []
        self.inputs: Set[str] = set()
        self.outputs: Set[str] = set()
    
    def make_child(self, e: parser.Event):
        cmd = Command(self.context, e.args)
        self.children.append(cmd)
        return cmd
    
    def add_input(self, filename: str):
        self.context.files.add(filename)
        self.inputs.add(filename)
    
    def add_output(self, filename: str):
        self.context.files.add(filename)
        self.outputs.add(filename)
    
    def to_graph(self, g: graphviz.Digraph) -> str:
        id = ' '.join(self.args[1])
        g.node(id, os.path.basename(self.args[0]), shape='oval')
        
        for c in self.children:
            child_id = c.to_graph(g)
            g.edge(id, child_id)
        
        for i in self.inputs:
            g.edge(i, id)
        
        for o in self.outputs:
            g.edge(id, o)
            
        return id

class Process:
    def __init__(self, context: Context, cwd: str, command: Command, fd={}) -> None:
        self.context = context
        self.cwd = cwd
        self.command = command
        self.fd = copy.deepcopy(fd)
    
    def normpath(self, path: str) -> str:
        if not os.path.isabs(path):
            path = os.path.join(self.cwd, path)
        return os.path.normpath(path)
    
    def handle_event(self, e: parser.Event) -> None:
        if e.name in ['fork', 'vfork'] and e.retval > 0:
            self.context.processes[e.retval] = Process(self.context, self.cwd, self.command)
        
        elif e.name == 'clone' and e.retval > 0:
            # Is this a fork?
            if 'SIGCHLD' in e.args[1] and e.retval and e.retval != -1:
                # Yes, create a new process
                self.context.processes[e.retval] = Process(self.context, self.cwd, self.command)
            else:
                print('Trace contains thread creation, which is not yet handled.')
                exit(2)
        
        elif e.name == 'execve' and e.retval == 0:
            if e.retval == -1:
                # TODO: Handle failed execve calls
                pass
            else:
                print('{} in process {}'.format(e.args[0], e.pid))
                self.command = self.command.make_child(e)
        
        elif e.name in ['access', 'stat', 'lstat', 'readlink'] and e.retval == 0:
            filename = self.normpath(e.args[0])
            #self.command.add_input(filename)
        
        elif e.name in ['unlink', 'chmod'] and e.retval == 0:
            # unlink is a bit complicated. If you rm a file without statting or
            # reading first, we'll say it's just an output
            filename = self.normpath(e.args[0])
            #self.command.add_output(self.normpath(filename))
        
        elif e.name == 'openat' and e.retval > 0:
            filename = self.normpath(e.args[1])
            self.fd[e.retval] = filename
        
        elif e.name == 'dup' and e.retval > 0:
            fdnum = int(e.args[0])
            self.fd[e.retval] = self.fd[fdnum]
        
        elif e.name == 'read' and e.retval >= 0:
            fdnum = int(e.args[0])
            self.command.add_input(self.fd[fdnum])
        
        elif e.name == 'write' and e.retval >= 0:
            fdnum = int(e.args[0])
            self.command.add_output(self.fd[fdnum])
            
        else:
            pass
            
        

def usage():
    print('Usage: {} <path to trace> <working directory>'.format(sys.argv[0]))
    sys.exit(1)

if __name__ == '__main__':
    if len(sys.argv) != 3:
        usage()
    
    # Parse the event trace
    events = parser.parse_file(sys.argv[1])
    
    # Check for events
    if len(events) == 0:
        print('There were no events in the trace file provided.')
        exit(1)
    
    # Create a context to track processes
    cwd = sys.argv[2]
    c = Context(cwd)
    
    for e in events:
        try:
            c.handle_event(e)
        except Exception as ex:
            print('Error while processing {}'.format(e))
            raise(ex)
    
    c.to_graph().render('graph.gv')
