module ProteinTranslation

let private codon =
    function
    | "AUG" -> "Methionine"
    | "UUU"
    | "UUC" -> "Phenylalanine"
    | "UUA"
    | "UUG" -> "Leucine"
    | "UCU"
    | "UCC"
    | "UCA"
    | "UCG" -> "Serine"
    | "UAU"
    | "UAC" -> "Tyrosine"
    | "UGU"
    | "UGC" -> "Cysteine"
    | "UGG" -> "Tryptophan"
    | "UAA"
    | "UAG"
    | "UGA" -> "STOP"
    | _ -> failwith "unknown"

let proteins rna =
    rna
    |> Seq.chunkBySize 3
    |> Seq.map (System.String >> codon)
    |> Seq.takeWhile ((<>) "STOP")
    |> Seq.toList
