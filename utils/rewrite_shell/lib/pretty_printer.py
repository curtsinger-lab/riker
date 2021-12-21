from typing import TYPE_CHECKING
from bashlex import ast


class prettyprinter(ast.nodevisitor):
    def __init__(self):
        self.indent = 0

    def visitoperator(self, n, op):
        return op + " "

    def visitlist(self, n, parts):
        return "( " + " ".join(map(lambda part: self.visit(part), parts)) + " ) "

    def visitpipe(self, n, pipe):
        return pipe + " "

    def visitpipeline(self, n, parts):
        return "[TODO pipeline] "

    def visitcompound(self, n, list, redirects):
        return "[TODO compound] "

    def visitif(self, n, parts):
        return "[TODO if] "

    def visitfor(self, n, parts):
        return "[TODO for] "

    def visitwhile(self, n, parts):
        return "[TODO while] "

    def visituntil(self, n, parts):
        return "[TODO until] "

    def visitcommand(self, n, parts):
        return " ".join(map(lambda part: self.visit(part), parts)) + " "

    def visitfunction(self, n, name, body, parts):
        return "[TODO function] "

    def visitword(self, n, word):
        return word + " "

    def visitassignment(self, n, word):
        return word + " "

    def visitreservedword(self, n, word):
        return word + " "

    def visitparameter(self, n, value):
        return value + " "

    def visittilde(self, n, value):
        return value + " "

    def visitredirect(self, n, input, type, output, heredoc):
        if heredoc != None:
            return self.visit(heredoc)
        if input != None and output == None:
            return type + " " + self.visit(input) + " "
        elif input == None and output != None:
            return type + " " + self.visit(output) + " "
        else:
            return "[TODO some other kind of redirect] "

    def visitheredoc(self, n, value):
        return value + " "

    def visitprocesssubstitution(self, n, command):
        return self.visit(command) + " "

    def visitcommandsubstitution(self, n, command):
        return self.visit(command) + " "

    # This redefinition exists to push all the work
    # inside each visitor.
    def visit(self, n):
        k = n.kind
        if k == "operator":
            return self._visitnode(n, n.op)
        elif k == "pipe":
            return self._visitnode(n, n.pipe)
        elif k == "compound":
            return self._visitnode(n, n.list, n.redirects)
        elif k in ("if", "for", "while", "until", "command", "pipeline", "list"):
            return self._visitnode(n, n.parts)
        elif k == "function":
            return self._visitnode(n, n.name, n.body, n.parts)
        elif k == "redirect":
            return self._visitnode(n, n.input, n.type, n.output, n.heredoc)
        elif k in ("word", "assignment", "reservedword"):
            return self._visitnode(n, n.word)
        elif k in ("parameter", "tilde", "heredoc"):
            return self._visitnode(n, n.value)
        elif k in ("commandsubstitution", "processsubstitution"):
            return self._visitnode(n, n.command)
        else:
            raise ValueError("unknown node kind %r" % k)

    @staticmethod
    def print(tree):
        pp = prettyprinter()
        print(pp.visit(tree))
