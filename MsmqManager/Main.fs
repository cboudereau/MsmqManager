module Main

open CommandLineParser
open MsmqTools
open System.IO
open System

[<EntryPoint>]
let main args = 
    let defaultCommand = 
        { command = NotAvailable
          queuePath = ""
          filePath = "dump.bin" }
    
    let bootstrap f = 
        try 
            f() |> printfn "%s"
            0
        with
        | CommandLineException msg -> 
            printfn "%s" msg
            -1
        | QueueNotFound msg -> 
            printfn "%s" msg
            -2
        | ex -> 
            printfn "%s" ex.Message
            -3
    
    let exportCommand filePath target = 
        use stream = File.OpenWrite filePath
        
        let target = 
            match target with
            | "" -> raise (CommandLineException "missing parameter target")
            | t -> t
        
        let exportedQueues = export stream target
        exportedQueues 
        |> Seq.map(fun exported -> sprintf "%i message(s) exported from queue %s to %s" (exported.messages |> Seq.length) exported.path filePath)
        |> String.concat System.Environment.NewLine
    
    let importCommand filePath queuePath = 
        use stream = File.OpenRead filePath
        let importedQueues = import stream queuePath
        
        importedQueues
        |> Seq.map(fun imported -> sprintf "%i message(s) imported to queue %s from %s" (imported.messages |> Seq.length) imported.path filePath)
        |> String.concat System.Environment.NewLine
    
    let boolToToggle toggle = 
        match toggle with
        | true -> "enabled"
        | false -> "disabled"
    
    let journalCommand queuePath toggle = 
        sprintf "journaling %s to queue %s" (queuePath
                                             |> journal toggle
                                             |> boolToToggle) queuePath
    
    let entryPoint = 
        fun () -> 
            (let usage = @"
error, see usage : 
==List available local queues==
/c list /q zanadu

==Enable queue journaling==
/c log /q dev-mdossantos\private$\siriona.connectivity.zanadu.input

==Export log==
/c exportFromJournal /q dev-mdossantos\private$\siriona.connectivity.zanadu.input /f d:\dump.bin
/c export /q dev-mdossantos\private$\siriona.connectivity.zanadu.input /f d:\dump.bin

==Import from dump==
/c import /q dev-mdossantos\private$\siriona.connectivity.zanadu.input /f d:\dump.bin

==Disable queue journaling==
/c unlog /q dev-mdossantos\private$\siriona.connectivity.zanadu.input
            "
             let commandLines = parseArguments (args |> Array.toList) defaultCommand
             match commandLines.command with
             | NotAvailable -> raise (CommandLineException usage)
             | List -> 
                 commandLines.queuePath
                 |> list
                 |> String.concat System.Environment.NewLine
             | Import -> commandLines.queuePath |> importCommand commandLines.filePath
             | Export -> commandLines.queuePath |> exportCommand commandLines.filePath
             | ExportFromJournal -> 
                 commandLines.queuePath
                 |> getJournalQueue
                 |> exportCommand commandLines.filePath
             | EnableJournal -> journalCommand commandLines.queuePath true
             | DisableJournal -> journalCommand commandLines.queuePath false)
    
    bootstrap <| entryPoint
