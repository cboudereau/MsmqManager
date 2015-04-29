module MsmqToolsTests

open FsUnit
open NUnit.Framework
open MsmqTools
open System.IO

let getRandomQueuePath = fun () -> (sprintf "test%s" (System.Guid.NewGuid().ToString()) |> getLocalPrivateQueue)
let allMessages queues = queues |> Seq.collect(fun q -> q.messages)

let runIntoQueues f = 
    let deleteWithAssert q = 
        q 
        |> delete 
        |> should equal [q]
    
    let deleteWithInfiniteConditionalStop queues = 
        queues 
        |> Seq.takeWhile (fun q -> q |> delete <> [])
        |> ignore

    let createWithAssert q = 
        q 
        |> create 
        |> should equal true
        q
    
    let queues = Seq.initInfinite <| fun _ -> getRandomQueuePath()
    try
        f (queues |> Seq.map createWithAssert)
    finally
        queues |> deleteWithInfiniteConditionalStop

let runIntoQueue f = 
    runIntoQueues
        (fun queuePaths -> 
            let queue = queuePaths |> Seq.take 1 |> Seq.exactlyOne
            f queue)

let ``delete all test queue``() =
    delete "test" |> Seq.length |> should be (greaterThan 0)

[<Test>]
let ``list available queue with filter``() = 
    runIntoQueue (fun queuePath ->
            let length = 
                "Test"
                |> list
                |> Seq.length
            length |> should be (greaterThan 0))

[<Test>]
let ``activate journaling send message export and import``() = 
    runIntoQueue (fun queuePath -> 
        queuePath
        |> journal true
        |> should equal [ (queuePath, true) ]
        let message = 
            queuePath |> sendMessage { body = "hello"
                                       label = "label" }
        
        let messages = queuePath |> receiveQueue |> allMessages
        messages |> should equal [ message ]
        use stream = new MemoryStream()
        
        let exportedQueues = 
            queuePath
            |> getJournalQueue
            |> export stream
        
        let importedQueues = import stream ""
        importedQueues |> Seq.iter (fun q -> q.path |> should equal queuePath)
        (importedQueues |> allMessages) |> should equal (exportedQueues |> allMessages))

[<Test>]
let ``activate journaling``() = 
    runIntoQueue (fun queuePath -> 
        queuePath
        |> journal true
        |> should equal [ (queuePath, true) ]
        let message = 
            queuePath |> sendMessage { body = "hello"
                                       label = "label" }
        
        let queues = queuePath |> receiveQueue  

        let messages = queues |> allMessages
        messages |> should equal [ message ]
        queuePath
        |> peekQueue
        |> allMessages
        |> should equal []
        queuePath
        |> getJournalQueue
        |> receiveQueue
        |> allMessages
        |> should equal messages)

[<Test>]
let ``delete not existing queue return false``() = 
    let randomQueuePath = getRandomQueuePath()
    
    randomQueuePath
    |> delete
    |> should equal []

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
        let expectedQueue = peekQueue queuePath |> Seq.head
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
        export stream queuePath |> should equal queue
        queuePath
        |> purge
        |> should equal true
        import stream "" |> should equal queue)

[<Test>]
let ``fail when queue not exists``() = 
    getRandomQueuePath()
    |> peekQueue
    |> should equal []

[<Test>]
let ``Send and receive message``() = 
    runIntoQueue (fun queuePath -> 
        let message = 
            { body = "hello"
              label = "label" }
        
        let sendedMessage = queuePath |> sendMessage message
        sendedMessage |> should equal message
        let queue = peekQueue queuePath |> Seq.head
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
        let queue = 
            queuePath
            |> peekQueue
            |> Seq.head
        queue.messages |> should not' (equal [ message ]))
