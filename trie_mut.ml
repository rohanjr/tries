open Core.Std

module type MutableTrie = sig
  type t
  val create : unit -> t
  val insert : t -> string -> bool
  val size : t -> int
  val subtrie : t -> string -> t option
  val mem : t -> string -> bool
  val is_prefix : t -> string -> bool
end

module MutableTrie = struct

  exception Internal_error of string

  type t = Trie of bool ref * int ref * t Char.Table.t

  let create () : t = Trie (ref false, ref 0, Char.Table.create ())

  let rec insert' (s : string) (from_index : int) : t -> bool =
    function Trie (b, c, m) ->
      let to_end = String.length s - from_index in
      if to_end < 0 then
        raise (Internal_error "insert' called with invalid index")
      else if to_end = 0 then begin
        if not !b then
          (b := true; incr c; true)
        else false end
      else begin
        let inserted = ref false in
        Hashtbl.change m s.[from_index] (fun tr_opt ->
          let subtr = Option.value tr_opt ~default:(create ()) in
          inserted := insert' s (from_index + 1) subtr;
          Some subtr);
        if !inserted then incr c; !inserted
      end

  let insert (tr : t) (s : string) : bool = insert' s 0 tr

  let size : t -> int = function Trie (_, c, _) -> !c

  let rec subtrie' (s : string) (from_index : int) (tr : t) : t option =
    let to_end = String.length s - from_index in
    if to_end < 0 then
      raise (Internal_error "subtrie' called with invalid index")
    else if to_end = 0 then
      Some tr
    else
      let Trie (_, _, m) = tr in
      match Hashtbl.find m s.[from_index] with
      | None -> None
      | Some subtr -> subtrie' s (from_index + 1) subtr

  let subtrie (tr : t) (s : string) : t option = subtrie' s 0 tr

  let lookup (tr : t) (s : string) (prefix : bool) : bool =
    match subtrie tr s with
    | None -> false
    | Some (Trie (b, _, _)) -> prefix || !b

  let mem (tr : t) (s : string) : bool = lookup tr s false

  let is_prefix (tr : t) (s : string) : bool = lookup tr s true
end

let is_prefix (dict : string list) : string -> bool =
  let tr = MutableTrie.create () in
  List.iter dict ~f:(fun s -> ignore (MutableTrie.insert tr s));
  MutableTrie.is_prefix tr

let count_completions (tr : MutableTrie.t) (s : string) : int =
  Option.value_map
    (MutableTrie.subtrie tr s)
    ~default:0
    ~f:MutableTrie.size

exception Input_error of string

let () =
  let _ = read_int () in
  let tr = MutableTrie.create () in
  let f line =
    let words = String.split line ~on:' ' in
    match words with
    | [op; s] ->
        if op = "add" then
          ignore (MutableTrie.insert tr s)
        else if op = "find" then begin
          print_int (count_completions tr s);
          print_newline () end
        else
          raise (Input_error ("Invalid operation: " ^ op ^ "\n"))
    | _ -> raise (Input_error ("Invalid command: " ^ line ^ "\n"))
  in
  In_channel.iter_lines In_channel.stdin ~f
