module CommandLineParser

open System

type Command = 
    | NotAvailable
    | List
    | Import
    | Export
    | ExportFromJournal
    | EnableJournal
    | DisableJournal

type CommandLine = 
    { command : Command
      queuePath : string
      filePath : string }

exception CommandLineException of string

let private (|InvariantEqual|_|) (str : string) arg = 
    if String.Compare(str, arg, StringComparison.OrdinalIgnoreCase) = 0 then Some()
    else None

let parseCommand command = 
    match command with
    | InvariantEqual "list" -> List
    | InvariantEqual "import" -> Import
    | InvariantEqual "export" -> Export
    | InvariantEqual "exportfromjournal" -> ExportFromJournal
    | InvariantEqual "log" -> EnableJournal
    | InvariantEqual "unlog" -> DisableJournal
    | _ -> raise (CommandLineException "command not found")

let rec parseArguments args commandLine = 
    match args with
    | [] -> commandLine
    | "/f" :: v :: q -> parseArguments q { commandLine with filePath = v }
    | "/q" :: v :: q -> parseArguments q { commandLine with queuePath = v }
    | "/c" :: v :: q -> parseArguments q { commandLine with command = parseCommand v }
    | _ -> raise (CommandLineException "missing required argument or value")
