module ErrorHandling

let handleErrorByThrowingException() = failwith "Exception occured."

let handleErrorByReturningOption (input: string) =
    match System.Int32.TryParse input with
    | true, value -> Some value
    | _ -> None

let handleErrorByReturningResult (input: string) =
    match System.Int32.TryParse input with
    | true, value -> Ok value
    | _ -> Error "Could not convert input to integer"

let bind switchFunction twoTrackInput =
    match twoTrackInput with
    | Ok v -> switchFunction v
    | Error e -> Error e

let cleanupDisposablesWhenThrowingException (resource: System.IDisposable) =
    try
        failwith "Exception occured."
    finally
        resource.Dispose()
