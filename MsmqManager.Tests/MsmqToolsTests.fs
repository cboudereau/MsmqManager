module MsmqToolsTests

open FsUnit
open NUnit.Framework
open MsmqTools
open System.IO

let getRandomQueuePath = fun () -> (sprintf "test%s" (System.Guid.NewGuid().ToString()) |> getLocalPrivateQueue)
let getMessages q = q.messages

let runIntoQueue f = 
    let queuePath = getRandomQueuePath()
    try 
        queuePath
        |> create
        |> should equal true
        f (queuePath)
    finally
        queuePath
        |> delete
        |> should equal true

[<Test>]
let ``list available queue with filter``() = 
    runIntoQueue (fun queuePath -> 
        "Test"
        |> list
        |> Seq.length
        |> should be (greaterThan 0))

[<Test>]
let ``activate journaling send message export and import``() = 
    runIntoQueue (fun queuePath -> 
        queuePath
        |> journal true
        |> should equal [(queuePath,true)]

        let message = 
            queuePath |> sendMessage { body = "hello"
                                       label = "label" }
        
        let queue = queuePath |> receiveQueue
        queue.messages |> should equal [ message ]
        use stream = new MemoryStream()
        
        let exportedQueues = 
            queuePath
            |> getJournalQueue
            |> export stream
        
        let importedQueues = import stream ""
        importedQueues |> Seq.iter (fun q -> q.path |> should equal queuePath)
        importedQueues.[0].messages |> should equal exportedQueues.[0].messages)

[<Test>]
let ``activate journaling``() = 
    runIntoQueue (fun queuePath -> 
        queuePath
        |> journal true
        |> should equal [ (queuePath, true) ]
        let message = 
            queuePath |> sendMessage { body = "hello"
                                       label = "label" }
        
        let queue = queuePath |> receiveQueue
        queue.messages |> should equal [ message ]
        queuePath
        |> peekQueue
        |> getMessages
        |> should equal []
        queuePath
        |> getJournalQueue
        |> receiveQueue
        |> getMessages
        |> should equal queue.messages)

[<Test>]
let ``delete not existing queue return false``() = 
    getRandomQueuePath()
    |> delete
    |> should equal false

[<Test>]
let ``create existing queue return false``() = 
    runIntoQueue (fun queuePath -> 
        queuePath
        |> create
        |> should equal false)

[<Test>]
let ``purge not exists queue return false``() = 
    getRandomQueuePath()
    |> purge
    |> should equal false

[<Test>]
let ``export and import message from queue to stream``() = 
    runIntoQueue (fun queuePath -> 
        use stream = new MemoryStream()
        queuePath
        |> sendMessage { body = "hello"
                         label = "label" }
        |> ignore
        let expectedQueue = peekQueue queuePath
        export stream queuePath |> should equal [ expectedQueue ]
        let imported = 
            runIntoQueue (fun importedQueue -> 
                let imported = import stream importedQueue |> Seq.head
                imported.path |> should equal importedQueue
                imported)
        imported.messages |> should equal expectedQueue.messages)

[<Test>]
let ``export and import into same queue``() = 
    runIntoQueue (fun queuePath -> 
        use stream = new MemoryStream()
        queuePath
        |> sendMessage { body = "hello"
                         label = "label" }
        |> ignore
        let queue = queuePath |> peekQueue
        export stream queuePath |> should equal [ queue ]
        queuePath
        |> purge
        |> should equal true
        import stream "" |> should equal [ queue ])

[<Test>]
let ``fail when queue not exists``() = 
    (fun () -> 
    (getRandomQueuePath()
     |> peekQueue
     |> ignore))
    |> should throw typeof<QueueNotFound>

[<Test>]
let ``Send and receive message``() = 
    runIntoQueue (fun queuePath -> 
        let message = 
            { body = "hello"
              label = "label" }
        
        let sendedMessage = queuePath |> sendMessage message
        sendedMessage |> should equal message
        let queue = peekQueue queuePath
        queue.messages |> should equal [ message ])

[<Test>]
let ``purge queue``() = 
    runIntoQueue (fun queuePath -> 
        let message = 
            queuePath |> sendMessage { body = "hello"
                                       label = "label" }
        queuePath
        |> purge
        |> should equal true
        let queue = queuePath |> peekQueue
        queue.messages |> should not' (equal [ message ]))
