module MsmqTools

open System.Messaging
open System.IO
open System.Runtime.Serialization.Formatters.Binary

exception QueueNotFound of string

type Message = 
    { body : string
      label : string }

type Queue = 
    { path : string
      messages : Message seq }

let getPrivateQueue machineName name = sprintf @"%s\private$\%s" machineName name
let getLocalPrivateQueue name = getPrivateQueue System.Environment.MachineName name
let getJournalQueue queuePath = sprintf "%s;journal" queuePath
let toQueuePath (journalQueuePath : string) = journalQueuePath.Replace(";journal", "")

let list filter = 
    MessageQueue.GetPrivateQueuesByMachine(System.Environment.MachineName)
    |> Seq.filter (fun q -> q.Path.LastIndexOf(filter, System.StringComparison.InvariantCultureIgnoreCase) <> -1)
    |> Seq.map (fun q -> q.Path)

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
    use queue = queuePath |> mandatoryQueue
    
    let messages = 
        queue.GetAllMessages()
        |> Seq.map toMessage
        |> Seq.toList
    { path = queue.Path
      messages = messages }

let rec private receiveAllMessages messages (enumerator : MessageEnumerator) = 
    match enumerator.MoveNext(), enumerator.RemoveCurrent(MessageQueueTransactionType.Single) with
    | false, _ -> messages
    | true, current -> receiveAllMessages ((current |> toMessage) :: messages) enumerator

let receiveQueue queuePath = 
    let queue = queuePath |> mandatoryQueue
    { path = queue.Path
      messages = 
          queue.GetMessageEnumerator2()
          |> receiveAllMessages []
          |> List.toSeq }

let purge queuePath = 
    match MessageQueue.Exists queuePath with
    | false -> false
    | true -> 
        use queue = new MessageQueue(queuePath)
        queue.Purge()
        true

let delete queuePath = 
    match MessageQueue.Exists queuePath with
    | false -> false
    | true -> 
        MessageQueue.Delete(queuePath)
        true

let journal toggle queuePath = 
    use queue = new MessageQueue(queuePath)
    queue.UseJournalQueue <- toggle
    queue.UseJournalQueue

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

let toBegin (stream : Stream) = 
    match stream with
    | :? MemoryStream as memoryStream -> 
        memoryStream.Flush()
        memoryStream.Position <- 0L
        stream
    | _ -> stream

let export (stream : Stream) from = 
    let formatter = new BinaryFormatter()
    let queue = from |> peekQueue
    formatter.Serialize(stream, queue)
    queue

let import (stream : Stream) tO = 
    let formatter = new BinaryFormatter()
    let queue = formatter.Deserialize(stream |> toBegin) :?> Queue
    
    let target = 
        match tO with
        | "" -> queue.path
        | _ -> tO
    queue.messages
    |> Seq.map (fun m -> sendMessage m (target |> toQueuePath))
    |> Seq.toList
    |> ignore
    let peekedQueue = peekQueue target
    { peekedQueue with path = peekedQueue.path |> toQueuePath }
