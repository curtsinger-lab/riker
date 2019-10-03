// Learn more about F# at http://fsharp.org

open System
open System.Collections.Generic

let mutable global_time = 0

type Command =
    | Program of name: String * time: int

type Artifact =
    | File of path: String * version: int

type Edge =
    | Input of artifact: Artifact * command: Command
    | Output of command: Command * artifact: Artifact
    | Action of parent: Command * child: Command

type Graph = Edge list

let GetInputsFor(c: Command)(g: Graph) : Artifact list =
    g |> List.filter (fun e ->
                        match e with
                        | Input(artifact, cmd) -> cmd = c
                        | _ -> false
                     )
      |> List.map (fun e ->
                      match e with
                      | Input(artifact, cmd) -> artifact
                      | _ -> failwith "impossible"
                  )

let CommandString(c: Command) : string =
    match c with
    | Program(name, t) -> sprintf "\"%s\"" name

let ArtifactString(a: Artifact) : string =
    match a with
    | File(path, version) -> sprintf "\"%s:%d\"" path version

let PrintEdge(e: Edge) : string =
    match e with
    | Input(a, c) -> sprintf "%s -> %s\n" (ArtifactString a) (CommandString c)
    | Output(c, a) -> sprintf "%s -> %s\n" (CommandString c) (ArtifactString a)
    | Action(p, c) -> sprintf "%s -> %s [style=dashed]\n" (CommandString p) (CommandString c)

let DemoGraph =
    (* nodes *)
    let dodofile = File("Dodofile", 0)
    let dodocmd = Program("Dodofile", 0)
    let gccfile = File("gcc", 0)
    let gcccmd = Program("gcc", 1)
    let cc1file = File("cc1", 0)
    let cc1cmd = Program("cc1", 2)
    let hello_c = File("hello.c", 0)
    let tmp_o_0 = File("tmp.o", 0)
    let tmp_s_0 = File("tmp.s", 0)
    let tmp_s_1 = File("tmp.s", 1)
    let ascmd = Program("as", 3)
    let collect2cmd = Program("collect2", 4)
    let tmp_o_1 = File("tmp.o", 1)
    let ldcmd = Program("ld", 5)
    let log_0 = File("log", 0)
    let log_1 = File("log", 1)
    let log_2 = File("log", 2)
    let hello = File("hello", 0)

    (* edges *)
    let e1  = Input(dodofile,dodocmd)
    let e2  = Action(dodocmd, gcccmd)
    let e3  = Action(gcccmd, cc1cmd)
    let e4  = Action(gcccmd, ascmd)
    let e5  = Action(gcccmd, collect2cmd)
    let e6  = Input(hello_c, cc1cmd)
    let e7  = Output(gcccmd, tmp_o_0)
    let e8  = Output(gcccmd, tmp_s_0)
    let e9  = Input(tmp_s_0, cc1cmd)
    let e10 = Input(tmp_o_0, ascmd)
    let e11 = Output(cc1cmd, tmp_s_1)
    let e12 = Input(tmp_s_1, ascmd)
    let e13 = Output(ascmd, tmp_o_1)
    let e14 = Output(collect2cmd, log_0)
    let e15 = Input(log_0, ldcmd)
    let e16 = Output(ldcmd, hello)
    let e17 = Action(collect2cmd, ldcmd)
    let e18 = Output(ldcmd, log_1)
    let e19 = Input(log_1, collect2cmd)
    let e20 = Output(collect2cmd, log_2)
    let e21 = Input(tmp_o_1, ldcmd)

    (* graph *)
    [ e1; e2; e3; e4; e5; e6; e7; e8; e9; e10; e11; e12; e13; e14; e15; e16; e17; e18; e19; e20; e21 ]

let ToDOT(g: Graph) : unit =
    let graph = List.map PrintEdge g
    let graph2 = "digraph {\n" :: "graph [rankdir=LR]\n" :: "node [fontname=Courier]\n" :: graph @ ["}\n"]
    IO.File.WriteAllLines(@"graph.dot", graph2)

let run(c: Command) =
    failwith "hmm"

let Dodo(g: Graph) : unit =
    let start = Program("Dodofile", 0)
    let cmdQ = new Queue<Command>()
    let iStk = new Stack<Artifact>()
    cmdQ.Enqueue start

    let mutable finished = false

    while not finished do
        // get a command
        let cmd = cmdQ.Dequeue

        // are its inputs up to date?
        let inputs = GetInputsFor cur g
        
        failwith "tired"
    
    failwith "not yet"
    
[<EntryPoint>]
let main argv =
    let d = DemoGraph
    Dodo d
    ToDOT d
    0 // return an integer exit code
