#!/usr/bin/env python3

import copy
import graphviz
import os
import sys
from typing import List, Dict, Set, Optional
import parse_strace as parser

TEMP_ID = 0

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

class Command: pass

class File:
    num_files = 0

    def __init__(self, filename: str, writer: Command) -> None:
        self.filename = filename
        self.users: Set[Command] = set()
        self.producers: Set[Command] = set()
        self.id = self.num_files
        self.num_files+= 1
        self.version = 0
        self.trunc = False
        self.interactions: List[Commands] = []
        self.has_race = False
        self.writer = writer
        self.conflicts: Set[Commands] = set()

    def is_local(self) -> bool:
        if len(sys.argv) == 4 and sys.argv[3] == "--show-sysfiles":
            return True
        if os.path.dirname(self.filename) == "":
            return True
        else:
            return False

    def is_intermediate(self) -> bool:
        if len(self.users)!=0 and len(self.producers)!=0 and "tmp" in os.path.dirname(self.filename):
            return True
        else:
            return False

    def collapse(self) -> None:
        self.has_race = True
        for i in self.interactions:
            i.has_race = True
            self.conflicts.add(i)

    def print_file(self) -> None:
        print(self.filename + ": " + str(self.version))
        for i in self.interactions:
            print("\t" + i.args[0])

    def can_depend(self, cmd: Command) -> bool:
        if self.writer == cmd or self.writer == None and self.trunc:
                return False
        else:
            return True

class Context:
    def __init__(self, starting_dir: str) -> None:
        self.starting_dir = starting_dir
        self.commands: List[Command] = []
        self.processes: Dict[int, Process] = {}
        self.files: Set[Files] = set()
    
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
        #for f in self.files: 
        #    f.print_file()
        g = graphviz.Digraph(engine='dot')
        g.attr('graph', [('rankdir', 'LR')])
        g.attr('node', [('fontname', 'Courier')])
        
        # Generate nodes for files
        #for f in self.files:
         #   if f.is_local():        
          #      g.node(f.filename, os.path.basename(f.filename), shape='rectangle')
        # Generate nodes for all base commands
        for c in self.commands:
            c.to_graph(g)
        return g

    # return latest version or create and put in files
    def find_file(self, filename: str) -> File:
        ret = None
        for f in self.files:
            if f.filename == filename:
                if ret is None:
                    ret = f
                elif f.version > ret.version:
                    ret = f
        if ret is None:
            f = File(filename, None)
            self.files.add(f)
            return f
        else:
            return ret

class Command:
    def __init__(self, context: Context, args) -> None:
        self.context = context
        self.args = args
        self.children: List[Command] = []
        self.inputs: Set[File] = set()     
        self.outputs: Set[File] = set()
        self.wr_interactions: Set[File] = set()
        self.rd_interactions: Set[File] = set()
        self.has_race = False

    def make_child(self, e: parser.Event):
        cmd = Command(self.context, e.args)
        self.children.append(cmd)
        return cmd
    
    def add_input(self, filename: str):
        f = self.context.find_file(filename)
        f.interactions.append(self)
        if f.can_depend(self):
            f.users.add(self)
            self.inputs.add(f)
        # if we've read from the file previously, check for a race
        for rds in self.rd_interactions: 
            if filename == rds.filename and f.writer != self:
                if rds.version == f.version:
                    #we're all good
                    return
                else: 
                    #we've found a race
                    rds.collapse();
                    self.has_race = True
                    return
        # otherwise, note that we've now read from this file version
        self.rd_interactions.add(f)


    def add_output(self, filename: str):
        f = self.context.find_file(filename) 
        # if we've written to the file before, check for a race 
        for wrs in self.wr_interactions:
            if filename == wrs.filename:
                f.producers.add(self)
                self.outputs.add(f)
                if f.version == wrs.version:
                    #we're continuing to write the same version
                    return
                else:
                    #the version has changed, someone has written in the meantime
                    #collapse all intermediate versions
                    wrs.collapse()
                    for w in self.context.files:
                        if w.filename == filename and w.version > wrs.version:
                            w.collapse()
                    # TODO collapse all itermittent versions not just the one last written to
                    wrs.has_race = True
                    self.has_race = True
                    return
        # if we haven't written to this file before, create a new version
        f.interactions.append(self)
        fnew = File(filename, self)
        fnew.version = f.version + 1 
        fnew.producers.add(self)
        fnew.trunc = f.trunc
        self.outputs.add(fnew)
        self.wr_interactions.add(fnew)
        self.context.files.add(fnew)

    def to_graph(self, g: graphviz.Digraph) -> str:
        #print("FILES")
        #for f in self.context.files:
        #    f.print_file()

        id = ' '.join(self.args[1])
        if self.has_race:
            g.node(id, os.path.basename(self.args[0]), shape='oval', style='filled', fillcolor='red', fontcolor='white')
        else:
            g.node(id, os.path.basename(self.args[0]), shape='oval', style='filled', fillcolor='gray35', fontcolor='white')
        
        for c in self.children:
            child_id = c.to_graph(g)
            g.edge(id, child_id, style="dashed")
        
        for i in self.inputs:

            if i.is_local() and not i.is_intermediate():
                g.node(i.filename + str(i.version), os.path.basename(i.filename), shape='rectangle')
                g.edge(i.filename + str(i.version), id, arrowhead='empty')

        for o in self.outputs: 

            global TEMP_ID
            if not o.is_intermediate():
                g.node(o.filename + str(o.version), os.path.basename(o.filename), shape='rectangle')
                g.edge(id, o.filename + str(o.version), arrowhead='empty')
            else:
                # For intermediate files, create a node in the graph but do not show a name
                node_id = 'temp_'+str(TEMP_ID)
                TEMP_ID += 1
                n = g.node(node_id, label='\\<temp\\>', shape='rectangle') 
                #n = g.node(node_id, label=o.filename, shape='rectangle') 
                g.edge(id, node_id, arrowhead='empty')
                
                # Create edges from the intermediate file to its dependents, since those commands will not create edges from non-local files by default
                for u in o.users:
                        g.edge(node_id, ' '.join(u.args[1]), arrowhead='empty')
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
        
        elif e.name == 'unlink' and e.retval == 0:
            filename = self.normpath(e.args[0])
            f = self.context.find_file(filename)
            version = f.version
            f = File(filename, None)
            f.trunc = True
            f.version = version +1
            self.context.files.add(f)

        #elif e.name == 'chmod' and e.retval == 0:
            #TODO

        elif e.name == 'openat' and e.retval > 0:
            filename = self.normpath(e.args[1])
            self.fd[e.retval] = filename
            f = self.context.find_file(filename)
            f.trunc = False
            #if f.closed:
            if "O_TRUNC" in e.args[2] or "O_EXCL" in e.args[2]:
                version = f.version
                f = File(filename, None)
                f.trunc = True
                f.version = version+1
                self.context.files.add(f)

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
    print('    or {} <path to trace> <working directory> --show-sysfiles'.format(sys.argv[0]))
    sys.exit(1)

if __name__ == '__main__':
    if len(sys.argv) != 3 and len(sys.argv) != 4:
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
