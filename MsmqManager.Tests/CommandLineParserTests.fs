module CommandLineParserTests

open FsUnit
open NUnit.Framework
open CommandLineParser

let defaultCommand = 
    { command = NotAvailable
      queuePath = ""
      filePath = "dump.bin" }

[<Test>]
let ``fail when bad argument``() = 
    (fun () -> (parseArguments [ "/toto" ] defaultCommand) |> ignore) |> should throw typeof<CommandLineException>

[<Test>]
let ``fail when command not exists``() = 
    (fun () -> (parseArguments [ "/c"; "toto" ] defaultCommand) |> ignore) |> should throw typeof<CommandLineException>

[<Test>]
let ``fail when missing command``() = 
    (fun () -> (parseArguments [ "/c" ] defaultCommand) |> ignore) |> should throw typeof<CommandLineException>

[<Test>]
let ``Parse command Export``()= (parseArguments ["/c";"export"] defaultCommand).command |> should equal Export

[<Test>]
let ``Parse command export from journal``()= (parseArguments ["/c";"exportfromjournal"] defaultCommand).command |> should equal ExportFromJournal

[<Test>]
let ``Parse command export from journal ignore case``()= (parseArguments ["/c";"exportFromJournal"] defaultCommand).command |> should equal ExportFromJournal

[<Test>]
let ``Parse target parameter``()= (parseArguments ["/q";"toto"] defaultCommand).queuePath |> should equal "toto"

[<Test>]
let ``Parse command log``()= (parseArguments ["/c";"log"] defaultCommand).command |> should equal EnableJournal

[<Test>]
let ``Parse command unlog``()= (parseArguments ["/c";"unlog"] defaultCommand).command |> should equal DisableJournal

[<Test>]
let ``Parse command Import``() = (parseArguments [ "/c"; "import" ] defaultCommand).command |> should equal Import

[<Test>]
let ``Parse file path``() = (parseArguments [ "/f"; "file.txt" ] defaultCommand).filePath |> should equal "file.txt"

[<Test>]
let ``Parse list command``() = (parseArguments [ "/c"; "list" ] defaultCommand).command |> should equal List