type 'a iterator = < has_value : bool; get : 'a; next : unit >

class ['a] list_iterator init = object
  val mutable current : 'a list = init

  method has_value = not @@ List.is_empty current

  method get =
    match current with
    | hd :: tl -> ignore tl; hd
    | [] -> raise (Invalid_argument "no value")

  method next =
    match current with
    | _ :: tl -> current <- tl
    | [] -> raise (Invalid_argument "no value")
end

class ['a] stack init = object
  val mutable v : 'a list = init

  method pop =
    match v with
    | hd :: tl ->
      v <- tl;
      Some hd
    | [] -> None

  method push hd =
    v <- hd :: v

  method iterator : 'a iterator =
    new list_iterator v
end

class sstack init = object
  inherit [string] stack init

  method print =
    List.iter print_endline v
end

let () =
  let s = new stack [] in
  begin
    s#push 1;
    s#push 2;
    s#push 3;

    let iter = s#iterator in
    while iter#has_value do
      begin
        print_endline @@ string_of_int iter#get;
        iter#next
      end
    done
  end
