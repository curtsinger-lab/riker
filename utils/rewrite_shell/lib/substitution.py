from typing import TYPE_CHECKING
from bashlex import ast # type: ignore
from subprocess import check_output
from lib.pretty_printer import PrettyPrinter
import sys
import re

# see https://github.com/idank/bashlex/blob/815653c7208a578735e18558069443cb5f67a9a2/bashlex/ast.py
# for original visitor implementation
# also see https://github.com/idank/bashlex/blob/815653c7208a578735e18558069443cb5f67a9a2/examples/commandsubstitution-remover.py
# for additional examples


class SubstitutionVistor(ast.nodevisitor):
    def __init__(self, env):
        self.env = env

    def genericpartsvisitor(self, kind: str, n: ast.node, parts: list[ast.node]) -> ast.node:
        newparts = []
        for part in parts:
            newparts.append(self.visit(part))
        return ast.node(kind=kind, parts=newparts, pos=n.pos)

    def visitoperator(self, n: ast.node, op: str) -> ast.node:
        return n

    def visitlist(self, n: ast.node, parts: list[ast.node]) -> ast.node:
        return self.genericpartsvisitor("list", n, parts)

    def visitpipe(self, n: ast.node, pipe: str):
        return n

    def visitpipeline(self, n: ast.node, parts: list[ast.node]) -> ast.node:
        return self.genericpartsvisitor("pipeline", n, parts)

    def visitcompound(self, n: ast.node, list: list[ast.node], redirects: list[ast.node]) -> ast.node:
        newlist = []
        newredirects = []
        for ls in list:
            newlist.append(self.visit(ls))
        for rd in redirects:
            newredirects.append(self.visit(rd))
        return ast.node(
            kind="compound", list=newlist, redirects=newredirects, pos=n.pos
        )

    def visitif(self, n: ast.node, parts: list[ast.node]) -> ast.node:
        return self.genericpartsvisitor("if", n, parts)

    def visitfor(self, n: ast.node, parts: list[ast.node]) -> ast.node:
        return self.genericpartsvisitor("for", n, parts)

    def visitwhile(self, n: ast.node, parts: list[ast.node]) -> ast.node:
        return self.genericpartsvisitor("while", n, parts)

    def visituntil(self, n: ast.node, parts: list[ast.node]) -> ast.node:
        return self.genericpartsvisitor("until", n, parts)

    def visitcommand(self, n: ast.node, parts: list[ast.node]) -> ast.node:
        return self.genericpartsvisitor("command", n, parts)

    def visitfunction(self, n: ast.node, name: str, body, parts: list[ast.node]) -> ast.node:
        newparts = []
        for part in parts:
            newparts.append(self.visit(part))
        return ast.node(
            kind="function", name=name, body=body, parts=newparts, pos=n.pos
        )

    def visitword(self, n: ast.node, word) -> ast.node:
        # substitute the parameter from env
        if len(n.parts) == 1 and n.parts[0].kind == "parameter":
            # eval the parameter first
            param = self.env[n.parts[0].value]
            
            # substitute the parameter in for the variable
            word2 = word.replace("$" + n.parts[0].value, param)
            return ast.node(kind="word", parts=[], pos=n.pos, word=word2)
        # not a substitution; evaluate subexpression
        elif len(n.parts) == 1:
            return ast.node(kind='word', parts=[self.visit(n.parts[0])], pos=n.pos, word="")
        elif len(n.parts) > 1:
            raise Exception("WordNode has more than 1 part.")
        return n

    def visitassignment(self, n: ast.node, word: str) -> ast.node:
        # get lhs and rhs
        [lhs, rhs] = word.split("=", 1)
        
        # init pretty printer
        pp = PrettyPrinter()
        
        # print(lhs + "=" + str(n.parts))
        # print(rhs)
        # input("PAUSED")
        
        # if any part of the rhs is a variable, substitute it now
        for part in n.parts:
            if part.kind == "parameter":
                rhs = rhs.replace("$" + part.value, self.env[part.value])
                
        # print(rhs)
        # input("PAUSED")
        
        # if any part of the rhs is a command substitution, run the command now
        # and put the output into env for later use
        subeval = []
        for part in n.parts:
            if part.kind == "commandsubstitution":
                # pretty print the command
                cmd = pp.visit(part).strip()
                out = check_output(cmd, shell=True).decode("utf-8").strip()
                subeval.append(out)

        # do replacements for any command substitution
        for i in range(len(subeval)):
            # replace the subcommand with the evaluated output
            rhs = re.sub(r"`.+?`", subeval[i], rhs, 1)
            
        # print(rhs)
        # input("PAUSED")
            
        # update in env
        self.env[lhs] = rhs
        return None

    def visitreservedword(self, n: ast.node, word: str) -> ast.node:
        return n

    def visitparameter(self, n: ast.node, value: ast.node) -> ast.node:
        return ast.node(kind="parameter", value=self.visit(value))

    def visittilde(self, n: ast.node, value) -> ast.node:
        return n

    def visitredirect(self, n: ast.node, input, type, output, heredoc) -> ast.node:
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

    def visitheredoc(self, n: ast.node, value: str) -> ast.node:
        return n

    def visitprocesssubstitution(self, n: ast.node, command: str) -> ast.node:
        newcommand = self.visit(command)
        return ast.node(kind="processsubstitution", command=newcommand)

    def visitcommandsubstitution(self, n: ast.node, command: str) -> ast.node:
        newcommand = self.visit(command)
        return ast.node(kind="commandsubstitution", command=newcommand)

    # This redefinition exists to push all the work
    # inside each visitor.
    def visit(self, n: ast.node) -> ast.node:
        # print(str(n))
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
