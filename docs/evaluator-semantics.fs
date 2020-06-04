(*
 * Expression denote dependence semantics of system calls.
 * In order to determine whether we need to rerun a command,
 * we just check whether running a command WOULD produce
 * literally the same expression.
 *)

(* example
 * X(GET("cc1", GET("8", GET("x86_64-linux-gnu", GET("gcc", GET("lib", GET("usr", /))))))) = OK
 * eq (X (...), saved)
 * ...
 * For example, cc1 issues this syscall: open("/tmp/a.s", PROT_READ | PROT_WRITE, O_CREAT | O_TRUNC)
 * W(GET("a.s", TRUNC(LINK_IF_NEEDED(NEWFILE, "a.s", LINK("a.s", NEWFILE, GET("tmp", /))))))
 * --> (spurious LINK_IF_NEEDED)
 * W(GET("a.s", TRUNC(LINK("a.s", NEWFILE, GET("tmp", /)))))
 * --> (truncating a linked empty file is the same as linking an empty file)
 * W(GET("a.s", LINK("a.s", NEWFILE, GET("tmp", /))))
 *)

(* another example, for cc1 command
 * R(GET("a.c", .)) = FINGERPRINT
 *
 * and for the as command
 * GET("a.s", GET("tmp", /)) = cc1 { W(GET("a.s", TRUNC("a.s", LINK_IF_NEEDED("a.s", NEWFILE, GET("tmp", /))))) }
 *
 *
 * a.s:0 = GET("a.s", GET("tmp", /)) = W(GET("a.s", ))
 *)

type INode = int

(* this is not a path; it is the path head minus the basename *)
type FileName = string

type Operation =
| Open of Command * Path * Perms * Flags * Outcome
|

type File =
| ExistingFile
| NewFile
| Write of Command * File

type Directory =
| ExistingDirectory
| NewDir
| Link of Command * FileName * Artifact * Directory * Outcome
| Unlink of FileName * Directory

type Outcome =
| OK
| ENOENT
| EACCES
| ...

type Artifact =
| File of File
| Directory of Directory // a directory is a map from path to INode
| Error of Error

type State = ???

(* gets an Artifact from a directory *)
let GET(f: FileName)(d: Directory)(s: State) : Artifact = ???

(* the given artifact is executable *)
let X(a: Artifact) : bool = ???

let R(a: Artifact) :

let eq a b : bool

(* GET("foo", LINK("bar", NEWFILE, /)) *)
