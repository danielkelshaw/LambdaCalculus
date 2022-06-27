module Context = Map.Make(String)

type expression =
  | Variable of string
  | Abstraction of {parameter: string; body: expression}
  | Application of {fn: expression; argument: expression}

type value =
  | Closure of {context: value Context.t; parameter: string; body: expression}
  | Native of (value -> value)

let rec interpreter context expression =
  match expression with
  | Variable name -> Context.find name context
  | Abstraction {parameter; body} -> Closure {context; parameter; body}
  | Application {fn; argument} ->
      let argument = interpreter context argument in
      match interpreter context fn with
      | Closure {context; parameter; body} ->
          interpreter (Context.add parameter argument context) body
      | Native f -> f argument
      

(* λf. (print_hello_world "f") *)
let lambda_abstraction = Abstraction {
  parameter = "f";
  body = Application {
    fn = Variable "print_hello_world";
    argument = Variable "f"
  }
}

let code = Application {fn = lambda_abstraction; argument = Variable "print_hello_world"}

let initial_context = 
  Context.empty 
  |> Context.add "print_hello_world"  (Native (fun v -> print_endline "Hello, world!"; v))

let _lambda = interpreter initial_context code
