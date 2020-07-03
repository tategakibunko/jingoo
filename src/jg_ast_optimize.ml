open Jg_types
open Jg_ast_mapper

(** [dead_code_elimination ast] perform a dead code elimination on [ast].
    It is able to remove from ast:
    - macro and function definitions not being actually used in ast
*)
let dead_code_elimination stmts =
  let used = Hashtbl.create 512 in
  let local_variables : (string * string list) list ref = ref [("", [])] in
  let push_block name = local_variables := (name, []) :: !local_variables in
  let pop_block () = local_variables := List.tl !local_variables in
  let set_local x =
    let fst, snd = List.hd !local_variables in
    local_variables := (fst, x :: snd ) :: (List.tl !local_variables) in
  let is_local (x : string) = List.exists (fun (_, l) -> List.mem x l) !local_variables in
  let scope () =
    let rec loop = function
      | [] -> ""
      | ("", _) :: tl -> loop tl
      | (s, _) :: _ -> s in
    loop !local_variables in
  let rec maybe_set = function
    | SetExpr set -> List.iter maybe_set set
    | IdentExpr id -> set_local id
    | _ -> () in
  let statement self = function
    | SetStatement (id, _) as s ->
      maybe_set id ;
      default_mapper.statement self s
    | SetBlockStatement (id, _) as s ->
      set_local id;
      push_block id;
      let s = default_mapper.statement self s in
      pop_block () ;
      s
    | ForStatement (ids, _, _) as s ->
      push_block "" ;
      List.iter set_local ids ;
      let s = default_mapper.statement self s in
      pop_block () ;
      s
    | FunctionStatement (id, args, _)
    | MacroStatement (id, args, _) as s ->
      push_block id ;
      set_local id ;
      List.iter (fun (i, _) -> set_local i) args ;
      let s = default_mapper.statement self s in
      pop_block () ;
      s
    | CallStatement(id, args, _, _) as s ->
      Hashtbl.add used id (scope ()) ;
      push_block "" ;
      List.iter (fun (i, _) -> set_local i) args ;
      let s = default_mapper.statement self s in
      pop_block () ;
      s
    | TextStatement (_)
    | ExpandStatement (_)
    | IfStatement (_)
    | SwitchStatement (_, _)
    | IncludeStatement (_, _)
    | RawIncludeStatement _
    | ExtendsStatement _
    | ImportStatement (_, _)
    | FromImportStatement (_, _)
    | BlockStatement (_, _)
    | FilterStatement (_, _)
    | WithStatement (_, _)
    | AutoEscapeStatement (_, _)
    | NamespaceStatement (_, _)
    | Statements (_)
      as s -> default_mapper.statement self s
  in
  let expression self = function
    | IdentExpr name as x when not (is_local name) ->
      Hashtbl.add used name (scope ()) ; x
    | e -> default_mapper.expression self e in
  let mapper = { default_mapper with expression ; statement } in
  let _ = mapper.ast mapper stmts in
  let statement self = function
    | MacroStatement (id, _, _)
    | FunctionStatement (id, _, _) as s ->
       (* Find if name is present in instructions called from toplevel *)
       let rec loop lists =
         match lists with
         | [] -> Statements []
         | [[]] -> Statements []
         | list :: _ when List.mem "" list -> default_mapper.statement self s (* id is used in toplevel *)
         | list :: _ ->
            let list' =
              list
              |> List.map (Hashtbl.find_all used)
              |> List.flatten
              |> List.sort_uniq compare in
            if List.mem list' lists then Statements []
            else loop (list' :: lists)
       in
       loop [ List.sort_uniq compare @@ Hashtbl.find_all used id ]
    | s -> default_mapper.statement self s
  in
  let mapper = { default_mapper with statement } in
  mapper.ast mapper stmts

(** [inline_include env ast]
    Inline the templates included in [ast] so it won't be necessary to
    open and parse theses parts when execution [ast].
*)
let inline_include env stmts =
  let open Jg_ast_mapper in
  let statement self = function
    | IncludeStatement (LiteralExpr (Tstr file), true) ->
      Statements (self.ast self @@ Jg_interp.ast_from_file ~env file)
    | RawIncludeStatement (LiteralExpr (Tstr file)) ->
      Statements (self.ast self @@ Jg_interp.ast_from_file ~env file)
    | e -> default_mapper.statement self e in
  let mapper = { default_mapper with statement } in
  mapper.ast mapper stmts
