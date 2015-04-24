module MsmqTools

open System.Messaging
open System.IO
open System.Runtime.Serialization.Formatters.Binary
open System.Xml.Serialization

exception QueueNotFound of string

[<CLIMutable>]
type Message = 
    { body : string
      label : string }

[<CLIMutable>]
type Queue = 
    { path : string
      messages : Message[] }

let getPrivateQueue machineName name = sprintf @"%s\private$\%s" machineName name
let getLocalPrivateQueue name = getPrivateQueue (System.Environment.MachineName.ToLower()) name
let journalPrefix = ";journal"
let getJournalQueue queuePath = sprintf "%s%s" queuePath journalPrefix
let toQueuePath (journalQueuePath : string) = journalQueuePath.Replace(journalPrefix, "")

let list (filter:string) = 
    
    let hasJournal = (filter.EndsWith(journalPrefix))

    let patchSearch search = 
        if(hasJournal) then search |> toQueuePath
        else search
    
    let patchQueue queue = 
        if(hasJournal) then queue |> getJournalQueue
        else queue

    MessageQueue.GetPrivateQueuesByMachine(System.Environment.MachineName)
    |> Seq.filter (fun q -> q.Path.LastIndexOf(filter |> patchSearch, System.StringComparison.InvariantCultureIgnoreCase) <> -1)
    |> Seq.map (fun q -> sprintf @"%s\%s" q.MachineName q.QueueName |> patchQueue)
    |> Seq.toList

let private toMessage (message : System.Messaging.Message) = 
    let reader = new StreamReader(message.BodyStream)
    let body = reader.ReadToEnd()
    { body = body
      label = message.Label }

let private mandatoryQueue queuePath = 
    match MessageQueue.Exists queuePath with
    | false -> raise (QueueNotFound queuePath)
    | true -> new MessageQueue(queuePath)

let peekQueue queuePath = 
    let queues = queuePath |> list
    
    queues
    |> Seq.map(fun queue ->
        use messageQueue = queue |> mandatoryQueue
    
        let messages = 
            messageQueue.GetAllMessages()
            |> Seq.map toMessage
            |> Seq.toArray
        { path = messageQueue.Path
          messages = messages })
    |> Seq.toList

let private receiveAllMessages messages = 
    let rec internalReceiveAllMessages (enumerator : MessageEnumerator) messages  = 
        match enumerator.MoveNext(), enumerator.RemoveCurrent(MessageQueueTransactionType.Single) with
        | false, _ -> messages
        | true, current -> internalReceiveAllMessages enumerator ((current |> toMessage) :: messages) 
    internalReceiveAllMessages messages []

let receiveQueue queuePath = 
    let queues = queuePath |> list
    
    queues
    |> Seq.map(fun queue ->
            let messageQueue = queue |> mandatoryQueue
            { path = messageQueue.Path
              messages = 
                  messageQueue.GetMessageEnumerator2()
                  |> receiveAllMessages
                  |> Seq.toArray })
    |> Seq.toList

let purge queuePath = 
    match MessageQueue.Exists queuePath with
    | false -> false
    | true -> 
        use queue = new MessageQueue(queuePath)
        queue.Purge()
        true

let delete queuePath = 
    let queues = queuePath |> list
    
    queues
    |> Seq.map(
        fun queue ->
            MessageQueue.Delete(queue)
            queue)
    |> Seq.toList

let journal toggle queuePath = 
    let queues = list queuePath

    queues 
    |> Seq.map(
        fun q -> 
            use queue = new MessageQueue(q)
            queue.UseJournalQueue <- toggle
            q, queue.UseJournalQueue)
    |> Seq.toList
    
let create queuePath = 
    match MessageQueue.Exists queuePath with
    | false -> 
        use queue = MessageQueue.Create(queuePath, true)
        true
    | _ -> false

let sendMessage message queuePath = 
    create queuePath |> ignore
    use queue = new MessageQueue(queuePath)
    use msmqMessage = new System.Messaging.Message()
    use writer = new StreamWriter(msmqMessage.BodyStream)
    writer.Write message.body
    writer.Flush()
    msmqMessage.Label <- message.label
    queue.Send(msmqMessage, MessageQueueTransactionType.Single)
    message

let private toBegin (stream : Stream) = 
    match stream with
    | :? MemoryStream as memoryStream -> 
        memoryStream.Flush()
        memoryStream.Position <- 0L
        stream
    | _ -> stream

let private formatter = new XmlSerializer(typeof<Queue []>)

let export (stream : Stream) from = 
    let queues = from |> peekQueue |> Seq.toArray

    formatter.Serialize(stream, queues)
    queues

let import (stream : Stream) tO = 
    let queues = formatter.Deserialize(stream |> toBegin) :?> Queue []
    
    queues
    |> Array.map(
        fun queue ->
            let target = 
                match tO with
                | "" -> queue.path
                | _ -> tO
            queue.messages
            |> Seq.map (fun m -> sendMessage m (target |> toQueuePath))
            |> Seq.toList
            |> ignore
            let peekedQueue = target |> peekQueue |> Seq.head
            { peekedQueue with path = peekedQueue.path |> toQueuePath }
        )