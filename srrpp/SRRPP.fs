namespace srrps

// (S)imple(R)equest(R)eply(P)rotocol(P)arser
//
// parses space separated data of the form:
// [ magic : 0xfffb; length : int32 ; to : byte[] ; from : byte[] ; flags : byte[] ; data : byte[] ]
// into something `Some(Packet)`. It's expected that to and from have size > 0
//
// The protocol is extremely minimalistic and worst case carries a 11 bytes (4+3+4) overhead,
// around 2x when using 1byte params, it's intended use is to make some basic
// communication patterns extremly simple where performance is non-critical.
// On the other hand it allows us to build simple semi-transparent protocols on top.
// it's primary use is to allow serialport discovery, control and communication available
// over loopback nic's

type Packet = {
    dst: byte[] ;
    src: byte[] ;
    flags: byte[] ;
    data: byte[] }

module SRRPP =

    let magic = [| byte 255 ; byte 251 |]

    let rec split (arr : byte[]) parts =
        try
            match Array.findIndex (fun (c : byte) -> c = byte 32) arr with
                | -1 -> [arr]
                | idx when 1 = parts -> [arr]
                | idx ->
                    let (h, t) = Array.splitAt idx arr
                    List.Cons(h, split (Array.skip 1 t) (parts - 1))
        with
            | :? System.Collections.Generic.KeyNotFoundException -> [arr]



    let decode (input : byte[]) =
        match Array.splitAt 2 input with
            | (moremagic, input) when magic = moremagic ->
                // get length
                let length = System.BitConverter.ToInt32 (input, 0)
                // split out the packet based on `length
                let (packet, rest) = Array.splitAt length input

                // skip length and split the packet on spaces
                match split (Array.skip 4 packet) 4 with
                    // a "well formated" packet
                    | [dst; src; flags; data] ->
                        Some ( { src = dst; dst = src; flags = flags; data = data }, rest )
                    // invalid data
                    | _ ->
                        None

            // unmagical, let's skip this
            | _ ->
                None

    // little helper to take string input
    let fromStr (input : string) =
        let buf = System.Text.Encoding.ASCII.GetBytes(input)
        decode buf
    
    //let packet =
    //    Array.append magic [|
    //        byte 11 ;
    //        byte 0 ;
    //        byte 0 ;
    //        byte 0 ;

    //        byte 1 ; byte 32; // to a
    //        byte 2 ; byte 32; // from b
    //        byte 0  ; byte 32; // flags
    //        byte 255 ;         // data
    //        |]

    //printf "got: %A" (decode packet)