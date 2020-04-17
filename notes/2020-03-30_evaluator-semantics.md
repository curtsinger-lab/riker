(*
 * Evaluating these expressions, produces a set commands to run.
 *)

(* example
 * X(GET("cc1", GET("8", GET("x86_64-linux-gnu", GET("gcc", GET("lib", GET("usr", /))))))) = OK
 * eq (X (...), saved)
 * ...
 * W(GET("a.s", TRUNC(LINK_IF_NEEDED(NEWFILE, "a.s", LINK("a.s", NEWFILE, GET("tmp", /))))))
 * --> (spurious LINK_IF_NEEDED)
 * W(GET("a.s", TRUNC(LINK("a.s", NEWFILE, GET("tmp", /)))))
 * --> (truncating a linked empty file is the same as linking an empty file)
 * W(GET("a.s", LINK("a.s", NEWFILE, GET("tmp", /))))
 *)

type INode of int

type Expression
| Command

type File
| Existing
| NewFile
| W of File
| FTRUNC of File

type Artifact
| File of File
| Directory of INode // a directory is a map from path to INode
| Error

type Outcome
| OK
| ENOENT
| EACCES
| ...

type FileName = string

(* gets an Artifact from a directory *)
let GET(f: FileName)(d: Directory) : Artifact = ???

(* Link an artifact into a directory with a given filename.
 * Returns the new version of the directory containing the linked file.
 *)
let LINK(f: FileName)(a: Artifact)(d: Directory) : Directory = ???

(* the given artifact is executable *)
let X(a: Artifact) : bool = ???

let W

let TRUNC

let LINK_IF_NEEDED



let eq a b : bool

(* GET("foo", LINK("bar", NEWFILE, /)) *)
