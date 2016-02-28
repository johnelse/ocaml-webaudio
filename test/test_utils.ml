type test_fun = unit -> unit

exception TestFailure of string

type result =
  | Error of exn
  | Failure of string
  | Success

let string_of_result = function
  | Error e -> Printf.sprintf "Error: %s" (Printexc.to_string e)
  | Failure msg -> Printf.sprintf "Failure: %s" msg
  | Success -> "Success"

type test =
  | TestCase of string * test_fun
  | TestList of string * test list

let run log test =
  let log_with_prefix prefix msg =
    log (Printf.sprintf "%s%s" prefix msg)
  in
  let rec run' prefix results = function
    | TestCase (label, f) ->
      let prefix = Printf.sprintf "%s%s:" prefix label in
      let log = log_with_prefix prefix in
      log "Start";
      let result =
        try
          f ();
          Success
        with
          | TestFailure msg -> Failure msg
          | e -> Error e
      in
      log "End";
      log (string_of_result result);
      result :: results
    | TestList (label, tests) ->
      let prefix = Printf.sprintf "%s%s:" prefix label in
      List.fold_left (run' prefix) results tests
  in
  List.rev (run' "" [] test)

let (>::) label f = TestCase (label, f)
let (>:::) label tests = TestList (label, tests)
