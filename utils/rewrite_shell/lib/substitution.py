from typing import TYPE_CHECKING
from bashlex import ast
import sys

# see https://github.com/idank/bashlex/blob/815653c7208a578735e18558069443cb5f67a9a2/bashlex/ast.py
# for original visitor implementation
# also see https://github.com/idank/bashlex/blob/815653c7208a578735e18558069443cb5f67a9a2/examples/commandsubstitution-remover.py
# for additional examples


class substitutionvisitor(ast.nodevisitor):
    def __init__(self, env):
        self.env = env

    def genericpartsvisitor(self, kind, n, parts):
        newparts = []
        for part in parts:
            newparts.append(self.visit(part))
        return ast.node(kind=kind, parts=newparts, pos=n.pos)

    def visitoperator(self, n, op):
        return n

    def visitlist(self, n, parts):
        return self.genericpartsvisitor("list", n, parts)

    def visitpipe(self, n, pipe):
        return n

    def visitpipeline(self, n, parts):
        return self.genericpartsvisitor("pipeline", n, parts)

    def visitcompound(self, n, list, redirects):
        newlist = []
        newredirects = []
        for ls in list:
            newlist.append(self.visit(ls))
        for rd in redirects:
            newredirects.append(self.visit(rd))
        return ast.node(
            kind="compound", list=newlist, redirects=newredirects, pos=n.pos
        )

    def visitif(self, n, parts):
        return self.genericpartsvisitor("if", n, parts)

    def visitfor(self, n, parts):
        return self.genericpartsvisitor("for", n, parts)

    def visitwhile(self, n, parts):
        return self.genericpartsvisitor("while", n, parts)

    def visituntil(self, n, parts):
        return self.genericpartsvisitor("until", n, parts)

    def visitcommand(self, n, parts):
        return self.genericpartsvisitor("command", n, parts)

    def visitfunction(self, n, name, body, parts):
        newparts = []
        for part in parts:
            newparts.append(self.visit(part))
        return ast.node(
            kind="function", name=name, body=body, parts=newparts, pos=n.pos
        )

    def visitword(self, n, word):
        # substitute the parameter from env
        if len(n.parts) == 1 and n.parts[0].kind == "parameter":
            word = self.env[n.parts[0].value]
            return ast.node(kind="word", parts=[], pos=n.pos, word=word)
        if len(n.parts) > 1:
            raise sys.Error("WordNode has more than 1 part.")
        return n

    def visitassignment(self, n, word):
        # add entry to env
        [var, val] = word.split("=")
        self.env[var] = val
        return n

    def visitreservedword(self, n, word):
        return n

    def visitparameter(self, n, value):
        return ast.node(kind="parameter", value=self.visit(value))

    def visittilde(self, n, value):
        return n

    def visitredirect(self, n, input, type, output, heredoc):
        newoutput = output
        newheredoc = heredoc
        if isinstance(n.output, ast.node):
            newoutput = self.visit(n.output)
        if n.heredoc:
            newheredoc = self.visit(n.heredoc)
        return ast.node(
            kind="redirect",
            input=input,
            type=type,
            output=newoutput,
            heredoc=newheredoc,
        )

    def visitheredoc(self, n, value):
        return n

    def visitprocesssubstitution(self, n, command):
        newcommand = self.visit(command)
        return ast.node(kind="processsubstitution", command=newcommand)

    def visitcommandsubstitution(self, n, command):
        newcommand = self.visit(command)
        return ast.node(kind="commandsubstitution", command=newcommand)

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
