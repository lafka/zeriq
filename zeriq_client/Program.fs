open ZeroMQ

// connect works like this:
// - open ZMQ REQ/REP
// - send REQ/REP Configuration to port
// - send REQ/REP Start to port
// - data should now be readable on `port/:port/io` PUB/SUB; subscribe
// - send REQ/REP data `port/:port/io`
// - send Stop to port on termination

let reqrep = "tcp://[::1]:5555"
let pubsub = "tcp://[::1]:5556"


let bytes (s : string) = System.Text.Encoding.ASCII.GetBytes(s)

let zmqsend (req : ZSocket) path (body : byte []) =
    let message = new ZMessage([ new ZFrame("port/" + path) ; new ZFrame(body) ])
    req.SendMessage message

let zmqrecv (req : ZSocket) path =
    use reply = req.ReceiveMessage()

    let key = reply.Unwrap()
    let value = reply.Unwrap()

    let recvkey = "port/" + path

    Some (value.Read())
    //match key.ReadString() with
    //    | "port/dev/ttyUSB0" -> Some (value.Read())
    //    | _ -> None



let configure (req : ZSocket) path baudrate =
    zmqsend req path (bytes ("configure;" + baudrate.ToString()))

    match zmqrecv req path with
        | Some value -> printf "reply-cfg: %A\n" value
        | None -> printf "reply-cfg: None .... wtf?"

let start (req : ZSocket) path =
    zmqsend req path (bytes "start")

    match zmqrecv req path with
        | Some value -> printf "reply-start: %A\n" value
        | None -> printf "reply-start: None .... wtf?"

let stop (req : ZSocket) path =
    zmqsend req path (bytes "stop")

    match zmqrecv req path with
        | Some value -> printf "reply-stop: %A\n" value
        | None -> printf "reply-stop: None .... wtf?"

type Message = Close | Data of byte[]

type Subscriber(req, path, handler) =
    let io onData =
        // opens a new subscriber socket
        printf "port/%s: opening zmq subscriber" path
        use sub = new ZSocket(ZSocketType.SUB)
        sub.Connect(pubsub)

        sub.Subscribe("port/" + path)
        let rec f sub path =
            async {
                match zmqrecv sub path with
                    | Some buf ->
                        printf "port/%s - recv -> %A" path buf
                        onData sub path buf
                        return! f sub path

                    | None ->
                        printf "port/%s - unrecv????" path
                        return! f sub path
            }
           
        f sub path

    let agent = MailboxProcessor.Start(fun inbox ->
        printf "starting serialport"
        let ioloop = io (fun _sub _path buf -> inbox.Post(Data(buf)))

        let rec loop(port) =
            async { let! msg = inbox.Receive()
                    match msg with
                        | Close ->
                            return ()

                        | Data buf ->
                            printf "port/%s - recv -> %A" path buf
                            handler buf
                            return! loop port
                    }
        loop 0)
       
    member a.Close() = agent.Post(Close)
    member a.Input(buf) = agent.Post(Data(buf))
    //io (fun buf -> agent.Post(Data(buf)))

     

let receive req path handler =
    new Subscriber(req, path, handler)


let connect (path : string) (baudrate : int) =
    printf "use ZContext!\n"
    use ctx = new ZContext()
    printf "use ZSocket REQ\n"
    use req = new ZSocket(ctx, ZSocketType.REQ)
    printf "ZSocket.Connect\n"
    req.Connect(reqrep)

    printf "port: configure\n"
    configure req path baudrate
    printf "port: start\n"
    start req path
    printf "port: subscriber\n"
    let subscriber = receive req path (fun buf -> printf "recv: %A\n" buf)

    printf "port <- readline\n"
    let readline = System.Console.ReadLine()

    stop req path

    0 // return an integer exit code

[<EntryPoint>]
let main argv =
    match Array.toList argv with
    | [path; baudrate] -> connect path (int baudrate)
    | [path] -> connect path 19200
    | _argv ->
        printf "usage: $0 <path> [baudrate] [verbose]\n"
        1