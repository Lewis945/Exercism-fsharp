module RnaTranscription

let toRna = String.map (function
    | 'G' -> 'C'
    | 'C' -> 'G'
    | 'T' -> 'A'
    | 'A' -> 'U'
    | _ -> failwith "Unknown nucleoitide")
