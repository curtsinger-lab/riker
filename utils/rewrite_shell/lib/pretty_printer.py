from typing import TYPE_CHECKING
from bashlex import ast

class prettyprinter(ast.nodevisitor):

    def __init__(self):
        self.indent = 0

    def visitoperator(self, n, op):
        print(" " * self.indent + str(n))

    def visitlist(self, n, parts):
        print(" " * self.indent + str(n))

    def visitpipe(self, n, pipe):
        print(" " * self.indent + str(n))

    def visitpipeline(self, n, parts):
        print(" " * self.indent + str(n))

    def visitcompound(self, n, list, redirects):
        print(" " * self.indent + str(n))

    def visitif(self, n, parts):
        print(" " * self.indent + str(n))

    def visitfor(self, n, parts):
        print(" " * self.indent + str(n))

    def visitwhile(self, n, parts):
        print(" " * self.indent + str(n))

    def visituntil(self, n, parts):
        print(" " * self.indent + str(n))

    def visitcommand(self, n, parts):
        print(" " * self.indent + "CommandNode")

    def visitfunction(self, n, name, body, parts):
        print(" " * self.indent + str(n))

    def visitword(self, n, word):
        if len(n.parts) == 0:
            print(" " * self.indent + "WordNode(word = '" + word + "')")
        else:
            print(" " * self.indent + "WordNode")

    def visitassignment(self, n, word):
        [lhs, rhs] = word.split("=")
        print(" " * self.indent + "AssignmentNode(lhs = '" + lhs + "', rhs = '" + rhs + "')")

    def visitreservedword(self, n, word):
        print(" " * self.indent + str(n))

    def visitparameter(self, n, value):
        print(" " * self.indent + "ParameterNode(value = '" + value + "')")

    def visittilde(self, n, value):
        print(" " * self.indent + str(n))

    def visitredirect(self, n, input, type, output, heredoc):
        if input != None and output == None:
            print(" " * self.indent + "RedirectInputNode")
        elif input == None and output != None:
            print(" " * self.indent + "RedirectOutputNode")
        elif input != None and output != None:
            print(" " * self.indent + "RedirectInputAndOutputNode")
        else:
            print(" " * self.indent + "RedirectNothing???")

    def visitheredoc(self, n, value):
        print(" " * self.indent + str(n))

    def visitprocesssubstitution(self, n, command):
        print(" " * self.indent + str(n))

    def visitcommandsubstitution(self, n, command):
        print(" " * self.indent + str(n))

    def visit(self, n):
        k = n.kind
        if k == "operator":
            self.indent += 2
            self._visitnode(n, n.op)
            self.indent -= 2
        elif k == "list":
            self.indent += 2
            dochild = self._visitnode(n, n.parts)
            if dochild is None or dochild:
                self.indent += 2
                for child in n.parts:
                    self.visit(child)
                self.indent -= 2
            self.indent -= 2
        elif k == "reservedword":
            self.indent += 2
            self._visitnode(n, n.word)
            self.indent -= 2
        elif k == "pipe":
            self.indent += 2
            self._visitnode(n, n.pipe)
            self.indent -= 2
        elif k == "pipeline":
            self.indent += 2
            dochild = self._visitnode(n, n.parts)
            self.indent -= 2
            if dochild is None or dochild:
                self.indent += 2
                for child in n.parts:
                    self.visit(child)
                self.indent -= 2
        elif k == "compound":
            self.indent += 2
            dochild = self._visitnode(n, n.list, n.redirects)
            if dochild is None or dochild:
                self.indent += 2
                for child in n.list:
                    self.visit(child)
                for child in n.redirects:
                    self.visit(child)
                self.indent -= 2
            self.indent -= 2
        elif k in ("if", "for", "while", "until"):
            self.indent += 2
            dochild = self._visitnode(n, n.parts)
            if dochild is None or dochild:
                self.indent += 2
                for child in n.parts:
                    self.visit(child)
                self.indent -= 2
            self.indent -= 2
        elif k == "command":
            self.indent += 2
            dochild = self._visitnode(n, n.parts)
            if dochild is None or dochild:
                self.indent += 2
                for child in n.parts:
                    self.visit(child)
                self.indent -= 2
            self.indent -= 2
        elif k == "function":
            self.indent += 2
            dochild = self._visitnode(n, n.name, n.body, n.parts)
            if dochild is None or dochild:
                self.indent += 2
                for child in n.parts:
                    self.visit(child)
                self.indent -= 2
            self.indent -= 2
        elif k == "redirect":
            self.indent += 2
            dochild = self._visitnode(n, n.input, n.type, n.output, n.heredoc)
            if dochild is None or dochild:
                self.indent += 2
                if isinstance(n.output, ast.node):
                    self.visit(n.output)
                if n.heredoc:
                    self.visit(n.heredoc)
                self.indent -= 2
            self.indent -= 2
        elif k == "word":
            self.indent += 2
            dochild = self._visitnode(n, n.word)
            if dochild is None or dochild:
                self.indent += 2
                for child in n.parts:
                    self.visit(child)
                self.indent -= 2
            self.indent -= 2
        elif k == "assignment":
            self.indent += 2
            dochild = self._visitnode(n, n.word)
            if dochild is None or dochild:
                self.indent += 2
                for child in n.parts:
                    self.visit(child)
                self.indent -= 2
            self.indent -= 2
        elif k in ("parameter", "tilde", "heredoc"):
            self.indent += 2
            self._visitnode(n, n.value)
            self.indent -= 2
        elif k in ("commandsubstitution", "processsubstitution"):
            self.indent += 2
            dochild = self._visitnode(n, n.command)
            if dochild is None or dochild:
                self.indent += 2
                self.visit(n.command)
                self.indent -= 2
            self.indent -= 2
        else:
            raise ValueError("unknown node kind %r" % k)
        self.indent += 2
        self.visitnodeend(n)
        self.indent -= 2