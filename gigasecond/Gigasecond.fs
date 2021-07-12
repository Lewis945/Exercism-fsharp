module Gigasecond

open System

let add (beginDate: DateTime) = 
    beginDate.AddSeconds (10.0 ** 9.0)