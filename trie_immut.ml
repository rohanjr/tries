open Core.Std

module type ImmutableTrie = sig
  type t
  val empty : t
  val insert : t -> string -> t
  val size : t -> int
  val subtrie : t -> string -> t option
  val mem : t -> string -> bool
  val is_prefix : t -> string -> bool
end

module ImmutableTrie = struct

  exception Internal_error of string

  type t = Trie of bool * int * t Char.Map.t

  let empty = Trie (false, 0, Char.Map.empty)

  let rec insert' (s : string) (from_index : int) : t -> t * bool =
    function Trie (b, c, m) ->
      let to_end = String.length s - from_index in
      if to_end < 0 then
        raise (Internal_error "insert' called with invalid index")
      else if to_end = 0 then
        let c' = if b then c else c + 1 in
        (Trie (true, c', m), not b)
      else
        let inserted = ref false in
        let m' = Map.change m s.[from_index] (fun tr_opt ->
          let subtr = Option.value tr_opt ~default:empty in
          let new_subtr, ins = insert' s (from_index + 1) subtr in
          inserted := ins; Some new_subtr) in
        let c' = if !inserted then c + 1 else c in
        (Trie (b, c', m'), !inserted)

  let insert (tr : t) (s : string) : t = fst (insert' s 0 tr)

  let size : t -> int = function Trie (b, c, m) -> c

  let rec subtrie' (s : string) (from_index : int) (tr : t) : t option =
    let to_end = String.length s - from_index in
    if to_end < 0 then
      raise (Internal_error "subtrie' called with invalid index")
    else if to_end = 0 then
      Some tr
    else begin
      let Trie (_, _, m) = tr in
      match Map.find m s.[from_index] with
      | None -> None
      | Some subtr -> subtrie' s (from_index + 1) subtr
    end

  let subtrie (tr : t) (s : string) : t option = subtrie' s 0 tr

  let lookup (tr : t) (s : string) (prefix : bool) : bool =
    match subtrie tr s with
    | None -> false
    | Some (Trie (b, _, _)) -> prefix || b

  let mem (tr : t) (s : string) : bool = lookup tr s false

  let is_prefix (tr : t) (s : string) : bool = lookup tr s true
end

let count_completions (tr : ImmutableTrie.t) (s : string) : int =
  Option.value_map
    (ImmutableTrie.subtrie tr s)
    ~default:0
    ~f:ImmutableTrie.size

let is_prefix (dict : string list) : string -> bool =
  let tr = List.fold dict ~init:ImmutableTrie.empty ~f:ImmutableTrie.insert in
  ImmutableTrie.is_prefix tr

exception Input_error of string

let () =
  let _ = read_int () in
  let init = ImmutableTrie.empty in
  let f tr line =
    let words = String.split line ~on:' ' in
    match words with
    | [op; s] ->
        if op = "add" then
          ImmutableTrie.insert tr s
        else if op = "find" then begin
          print_int (count_completions tr s);
          print_newline ();
          tr end
        else
          raise (Input_error ("Invalid operation: " ^ op ^ "\n"))
    | _ -> raise (Input_error ("Invalid command: " ^ line ^ "\n"))
  in
  ignore (In_channel.fold_lines In_channel.stdin ~init ~f)
