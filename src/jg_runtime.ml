(*
  jg_runtime.ml

  Copyright (c) 2011- by Masaki WATANABE

  License: see LICENSE
*)

(** Builtin functions, value lookup and context initialization. *)

open Jg_utils
open Jg_types

(**/**)

let jg_strp = function
  | Tstr _ -> Tbool true
  | _ -> Tbool false

let jg_intp = function
  | Tint _ -> Tbool true
  | _ -> Tbool false

let jg_floatp = function
  | Tfloat _ -> Tbool true
  | _ -> Tbool false

let jg_listp = function
  | Tlist _ -> Tbool true
  | _ -> Tbool false

let jg_setp = function
  | Tset _ -> Tbool true
  | _ -> Tbool false

let jg_objp = function
  | Tobj _ -> Tbool true
  | _ -> Tbool false

let jg_hashp = function
  | Thash _ -> Tbool true
  | _ -> Tbool false

let jg_patp = function
  | Tpat _ -> Tbool true
  | _ -> Tbool false

let jg_funp = function
  | Tfun _ -> Tbool true
  | _ -> Tbool false

let jg_arrayp = function
  | Tarray _ -> Tbool true
  | _ -> Tbool false

let jg_push_frame ctx =
  {ctx with frame_stack = (Hashtbl.create 10) :: ctx.frame_stack}

let jg_set_value ctx name value =
  match ctx.frame_stack with
    | [] -> raise @@ Invalid_argument "jg_set_value"
    | frame :: _ -> Hashtbl.add frame name value

let jg_set_values ctx names values =
  let values = Jg_utils.take (List.length names) values ~pad:Tnull in
  List.iter2 (jg_set_value ctx) names values

let rec jg_force = function
  | Tlazy x -> jg_force (Lazy.force x)
  | Tvolatile x -> jg_force (x ())
  | x -> x

let rec string_of_tvalue ?(default = "") = function
  | Tint x -> string_of_int x
  | Tfloat x -> string_of_float x
  | Tstr x -> x
  | Tbool x -> string_of_bool x
  | Tobj _ as x -> string_of_obj "<obj>" x
  | Thash _ as x -> string_of_obj "<hash>" x
  | Tpat _ as x -> string_of_obj "<pat>" x
  | Tnull -> default
  | Tlist _ -> "<list>"
  | Tset _ -> "<set>"
  | Tfun _ -> "<fun>"
  | Tarray _ -> "<array>"
  | Tlazy _ -> "<lazy>"
  | Tvolatile _ -> "<volatile>"

and string_of_obj default obj =
  string_of_tvalue ~default @@ jg_obj_lookup obj "__str__"

and dump_expr = function
  | IdentExpr(str) -> spf "IdentExpr(%s)" str
  | LiteralExpr(tvalue) -> spf "LiteralExpr(%s)" (string_of_tvalue tvalue)
  | NotOpExpr(_) -> "NotOpExpr"
  | NegativeOpExpr(_) -> "NegativeOpExpr"
  | PlusOpExpr(_,_) -> "PlusOpExpr"
  | MinusOpExpr(_,_) -> "MinusExpr"
  | TimesOpExpr(_,_) -> "TimesOpExpr"
  | PowerOpExpr(_,_) -> "PowerOpExpr"
  | DivOpExpr(_,_) -> "DivOpExpr"
  | ModOpExpr(_,_) -> "ModOpExpr"
  | AndOpExpr(_,_) -> "AndOpExpr"
  | OrOpExpr(_,_) -> "OrOpExpr"
  | NotEqOpExpr(_,_) -> "NotEqOpExpr"
  | EqEqOpExpr(_,_) -> "EqEqExpr"
  | LtOpExpr(_,_) -> "LtOpExpr"
  | GtOpExpr(_,_) -> "GtOpExpr"
  | LtEqOpExpr(_,_) -> "LtEqOpExpr"
  | GtEqOpExpr(_,_) -> "GtEqOpExpr"
  | DotExpr(_,_) -> "DotExpr"
  | BracketExpr(_,_) -> "BracketExpr"
  | ListExpr(_) -> "ListExpr"
  | SetExpr(_) -> "SetExpr"
  | ObjExpr(_) -> "ObjExpr"
  | InOpExpr(_,_) -> "InOpExpr"
  | KeywordExpr(_,_) -> "KeywordExpr"
  | AliasExpr(_,_) -> "AliasExpr"
  | ApplyExpr(_,_) -> "ApplyExpr"
  | TestOpExpr(_,_) -> "TestOpExpr"

and jg_bind_names ctx names values =
  match names, values with
    | [name], value -> jg_set_value ctx name value
    | _, Tset values -> jg_set_values ctx names values
    | _, (Tobj _ | Thash _ | Tpat _) ->
      jg_set_values ctx names (List.map (jg_obj_lookup values) names)
    | _ -> ()

and jg_get_value ctx name =
  let rec get_value name = function
    | frame :: rest ->
      (try jg_force (Hashtbl.find frame name)
       with Not_found -> get_value name rest)
    | [] ->
      (try Thash (Hashtbl.find ctx.namespace_table name)
       with Not_found -> Tnull) in
  get_value name ctx.frame_stack

and jg_obj_lookup obj prop_name =
  jg_force @@
  match obj with
    | Tobj(alist) -> (try List.assoc prop_name alist with Not_found -> Tnull)
    | Thash(hash) -> (try Hashtbl.find hash prop_name with Not_found -> Tnull)
    | Tpat(fn) -> (try fn prop_name with Not_found -> Tnull)
    | Tlazy _ | Tvolatile _ -> jg_obj_lookup (jg_force obj) prop_name
    | _ -> failwith_type_error_1 ("jg_obj_lookup(\"" ^ prop_name ^ "\")") obj

let jg_obj_lookup_by_name ctx obj_name prop_name =
  match jg_get_value ctx obj_name with
    | (Tobj _ | Thash _ | Tpat _) as obj -> jg_obj_lookup obj prop_name
    | _ -> (try Jg_stub.get_func ~namespace:obj_name ~func_name:prop_name
            with Not_found -> Tnull)

let jg_get_func ctx name =
  match jg_get_value ctx name with
    | Tfun _ as f -> f
    | _ -> failwith @@ spf "undefined function %s" name

let jg_set_macro ctx name macro =
  Hashtbl.add ctx.macro_table name macro

let jg_get_macro ctx name =
  try Some(Hashtbl.find ctx.macro_table name) with Not_found -> None

let jg_remove_macro ctx name =
  Hashtbl.remove ctx.macro_table name

let jg_set_filter ctx name =
  {ctx with active_filters = name :: ctx.active_filters}

let jg_pop_filter ctx =
  match ctx.active_filters with
    | [] -> ctx
    | _ :: rest -> {ctx with active_filters = rest}

(**/**)

(**/**)
let jg_nth_aux value i =
  match value with
  | Tarray a -> a.(i)
  | Tset l | Tlist l -> List.nth l i
  | Tstr s -> Tstr (String.make 1 @@ String.get s i)
  | _ -> failwith_type_error_1 "jg_nth_aux" value
(**/**)

(** [jg_nth n seq] returns the [n]-th value of sequence [seq] *)
let jg_nth i value =
  match i with
  | Tint i -> jg_nth_aux value i
  | _ -> failwith_type_error_1 "jg_nth" i

(** [jg_escape_html x] escape [x] string representation using {!Jg_utils.escape_html} *)
let jg_escape_html str =
  match str with
    | Tstr str -> Tstr(Jg_utils.escape_html str)
    | other -> Tstr(Jg_utils.escape_html @@ string_of_tvalue other)

(**/**)
let jg_apply ?(name="<lambda>") ?kwargs f args =
  if args = [] then
    let fn = unbox_fun f in
    Tfun (fun ?kwargs:kw x -> fn ?kwargs:(merge_kwargs kwargs kw) x)
  else
    try List.fold_left (fun fn a -> (unbox_fun fn) ?kwargs a) f args
    with _ -> func_failure ~name ?kwargs args

let jg_apply_filters ?(autoescape=true) ?(safe=false) ctx text filters =
  let (safe, text) = List.fold_left (fun (safe, text) name ->
    if name = "safe" then
      (true, text)
    else if name = "escape" && autoescape = true then
      (safe, text)
    else
      (safe, jg_apply (jg_get_func ctx name) [text] ~name)
  ) (safe, text) filters in
  if safe || not autoescape then text else jg_escape_html text

let jg_output ?(autoescape=true) ?(safe=false) ctx value =
  if ctx.serialize || (ctx.active_filters = [] && safe) then
    ctx.output value
  else
    ctx.output @@
    jg_apply_filters ctx value ctx.active_filters ~safe ~autoescape ;
  ctx

let jg_obj_lookup_path obj path =
  List.fold_left (fun obj key -> jg_obj_lookup obj key) obj path

let jg_length_aux x =
  match x with
    | Tlist lst -> List.length lst
    | Tset lst -> List.length lst
    | Tstr str -> Jg_utils.strlen str
    | Tarray arr -> Array.length arr
    | _ -> failwith_type_error_1 "jg_length" x

(* FIXME: only declare loop once and use volatile values updated with each iteration *)
let jg_iter_mk_ctx ctx iterator itm len i =
  let cycle = func_1 (fun set -> jg_nth_aux set (i mod jg_length_aux set)) in
  let ctx = jg_push_frame ctx in
  let () = jg_bind_names ctx iterator itm in
  let () =
    jg_set_value ctx "loop" @@
    Tpat (function
        | "index0" -> Tint i
        | "index" -> Tint (i+1)
        | "revindex0" -> Tint (len - i - 1)
        | "revindex" -> Tint (len - i)
        | "first" -> Tbool (i=0)
        | "last" -> Tbool (i=len-1)
        | "length" -> Tint len
        | "cycle" -> cycle
        | _ -> raise Not_found
      ) in
  ctx

let jg_iter_hash ctx iterator f h =
  let i = ref 0 in
  let len = Hashtbl.length h in
  Hashtbl.iter
    (fun k v ->
       let itm = Tset [ box_string k ; v ] in
       let () = f @@ jg_iter_mk_ctx ctx iterator itm len (!i) in
       incr i)
    h

let jg_iter_obj ctx iterator f l =
  let len = List.length l in
  List.iteri
    (fun i (k, v) ->
       let itm = Tset [ box_string k ; v ] in
       f @@ jg_iter_mk_ctx ctx iterator itm len i)
    l

let jg_iter_array ctx iterator f a =
  let len = Array.length a in
  Array.iteri (fun i itm -> f @@ jg_iter_mk_ctx ctx iterator itm len i) a

let jg_iter_str ctx iterator f s =
  let len = String.length s in
  String.iteri (fun i itm ->
      let itm = Tstr (String.make 1 itm) in
      f @@ jg_iter_mk_ctx ctx iterator itm len i) s

let jg_iter ctx iterator f iterable =
  match iterable with
  | Thash h -> jg_iter_hash ctx iterator f h
  | Tobj l -> jg_iter_obj ctx iterator f l
  | Tarray a -> jg_iter_array ctx iterator f a
  | Tstr s -> jg_iter_str ctx iterator f s
  | Tlist l | Tset l ->
    let len = List.length l in
    List.iteri (fun i itm -> f @@ jg_iter_mk_ctx ctx iterator itm len i) l
  | _ -> ()

let jg_eval_aux ctx args kwargs macro f =
  let Macro (arg_names, defaults, code) = macro in
  let arg_names_len = List.length arg_names in
  let () =
    let values = Jg_utils.take arg_names_len args ~pad:Tnull in
    List.iter2 (jg_set_value ctx) arg_names values in
  let () =
    List.iter
      (fun (name, value) ->
         let value = try List.assoc name kwargs with Not_found -> value in
         jg_set_value ctx name value )
      defaults in
  f ctx code

let jg_eval_macro ?(caller=false) ctx macro_name args kwargs macro f =
  let Macro (arg_names, defaults, _) = macro in
  let args_len = List.length args in
  let arg_names_len = List.length arg_names in
  let ctx' = jg_push_frame ctx in
  let () = jg_set_value ctx' "varargs" @@ Tlist (Jg_utils.after arg_names_len args) in
  let () = jg_set_value ctx' "kwargs" @@ Tobj kwargs in
  let () = jg_set_value ctx' macro_name @@ Tpat (function
      | "name" -> Tstr macro_name
      | "arguments" -> Tlist (List.map box_string arg_names)
      | "defaults" -> Tobj defaults
      | "catch_kwargs" -> Tbool (kwargs <> [])
      | "catch_vargs" -> Tbool (args_len > arg_names_len)
      | "caller" -> Tbool caller
      | _ -> raise Not_found
    ) in
  ignore @@ jg_eval_aux ctx' args kwargs macro f;
  ctx

let jg_test_defined_aux ctx name fn =
  fn (jg_get_value ctx name)
(**/**)

(* TODO: do not hard code them in jg_interp but use something like std_filters instead. *)
(* FIXME: remove the ctx (as filters do not need ctx) *)
let jg_test_defined ctx name =
  Tbool (jg_test_defined_aux ctx name @@ (<>) Tnull)

let jg_test_undefined ctx name =
  Tbool (jg_test_defined_aux ctx name @@ (=) Tnull)

(** Alias for [jg_test_undefined]  *)
let jg_test_none = jg_test_undefined

let jg_test_obj_defined ctx obj_name prop_name =
  match jg_get_value ctx obj_name with
    | Tobj(alist) -> Tbool (List.mem_assoc prop_name alist)
    | _ -> Tbool(false)

let jg_test_obj_undefined ctx obj_name prop_name =
  match jg_test_obj_defined ctx obj_name prop_name with
    | Tbool status -> Tbool (not status)
    | _ -> failwith "invalid test:jg_test_obj_defined"

(** FIXME: this should check the value and not the context *)
let jg_test_escaped ctx =
  Tbool(List.mem "safe" @@ ctx.active_filters)

let jg_negative = function
  | Tint x -> Tint(-x)
  | Tfloat x -> Tfloat(-.x)
  | x -> failwith_type_error_1 "jg_negative" x

let rec jg_is_true = function
  | Tbool x -> x
  | Tstr x -> x <> ""
  | Tint x -> x != 0
  | Tfloat x -> (x > epsilon_float) || (x < -. epsilon_float)
  | Tlist x -> x <> []
  | Tset x -> x <> []
  | Tobj x -> x <> []
  | Thash x -> Hashtbl.length x > 0
  | Tpat _ -> true
  | Tnull -> false
  | Tfun _ -> true
  | Tarray a -> Array.length a > 0
  | Tlazy fn -> jg_is_true (Lazy.force fn)
  | Tvolatile fn -> jg_is_true (fn ())

let jg_not x =
  Tbool (not (jg_is_true x))

(** [jg_plus a b]
    The multi-purpose [+] operator.
    Can add two numbers,
    concat two strings or a string and a number,
    append two sequences (list or array).
  *)
let jg_plus left right =
  match left, right with
    | Tint x1, Tint x2 -> Tint(x1+x2)
    | Tint x1, Tfloat x2 -> Tfloat(float_of_int x1+.x2)
    | Tint x1, Tstr x2 -> Tstr (string_of_int x1 ^ x2)

    | Tfloat x1, Tfloat x2 -> Tfloat(x1+.x2)
    | Tfloat x1, Tint x2 -> Tfloat(x1+.float_of_int x2)
    | Tfloat x1, Tstr x2 -> Tstr (string_of_float x1 ^ x2)

    | Tstr x1, Tstr x2 -> Tstr (x1 ^ x2)
    | Tstr x1, Tint x2 -> Tstr (x1 ^ string_of_int x2)
    | Tstr x1, Tfloat x2 -> Tstr (x1 ^ string_of_float x2)

    | Tlist l1, Tlist l2 -> Tlist (List.append l1 l2)
    | Tarray a1, Tlist l2 -> Tlist (List.append (Array.to_list a1) l2)
    | Tlist l1, Tarray a2 -> Tlist (List.append l1 (Array.to_list a2))
    | Tarray a1, Tarray a2 -> Tarray (Array.append a1 a2)

    | _, _ -> failwith_type_error_2 "jg_plus" left right

let jg_minus left right =
  match left, right with
    | Tint x1, Tint x2 -> Tint(x1-x2)
    | Tfloat x1, Tfloat x2 -> Tfloat(x1-.x2)
    | Tint x1, Tfloat x2 -> Tfloat(float_of_int x1-.x2)
    | Tfloat x1, Tint x2 -> Tfloat(x1-.float_of_int x2)
    | _, _ -> failwith_type_error_2 "jg_minus" left right

let jg_times left right =
  match left, right with
    | Tint x1, Tint x2 -> Tint(x1*x2)
    | Tfloat x1, Tfloat x2 -> Tfloat(x1*.x2)
    | Tint x1, Tfloat x2 -> Tfloat(float_of_int x1*.x2)
    | Tfloat x1, Tint x2 -> Tfloat(x1*.float_of_int x2)
    | _, _ -> failwith_type_error_2 "jg_times" left right

let jg_power left right =
  let rec power m n a =
    if n <= 0 then a
    else if n mod 2 = 0 then power (m *. m) (n / 2) a
    else power m (n - 1) (m *. a) in
  match left, right with
    | Tint x1, Tint x2 -> Tfloat (power (float_of_int x1) x2 1.0)
    | _, _ -> failwith_type_error_2 "jg_power" left right

let jg_div left right =
  match left, right with
    | _, Tint 0 | _, Tfloat 0.0 -> failwith "jg_div:zero division error"
    | Tint x1, Tint x2 -> Tint(x1/x2)
    | Tfloat x1, Tfloat x2 -> Tfloat(x1/.x2)
    | Tint x1, Tfloat x2-> Tfloat(float_of_int x1/.x2)
    | Tfloat x1, Tint x2 -> Tfloat(x1/.float_of_int x2)
    | _, _ -> failwith_type_error_2 "jg_div" left right

let jg_mod left right =
  match left, right with
    | _, Tint 0 -> failwith "jg_mod:zero division error"
    | Tint x1, Tint x2 -> Tint(x1 mod x2)
    | _, _ -> failwith_type_error_2 "jg_mod" left right

(** [jg_or e1 e2] The boolean [and]. *)
let jg_and left right =
  Tbool(jg_is_true left && jg_is_true right)

(** [jg_or e1 e2] The boolean [or]. *)
let jg_or left right =
  Tbool(jg_is_true left || jg_is_true right)

(** [jg_compare x y] returns [0] if [x] is equal to [y],
    a negative integer if [x] is less than [y],
    and a positive integer if [x] is greater than [y]. *)
let rec jg_compare left right =
  Tint (jg_compare_aux left right)

(**/**)
and jg_compare_aux left right =
  match left, right with
  | Tint x1, Tint x2 -> compare x1 x2
  | Tfloat x1, Tfloat x2 -> compare x1 x2
  | Tstr x1, Tstr x2 -> strcmp x1 x2
  | Tbool x1, Tbool x2 -> compare x1 x2
  | Tlist x1, Tlist x2 -> jg_compare_list ~filter:(fun x -> x) x1 x2
  | Tset x1, Tset x2 -> jg_compare_list ~filter:(fun x -> x) x1 x2
  | Tarray x1, Tarray x2 ->
    begin
      let l1 = Array.length x1 in
      let l2 = Array.length x2 in
      match compare l1 l2 with
      | 0 ->
        let rec loop i =
          if i = l1 then 0
          else match jg_compare_aux x1.(i) x2.(i) with
            | 0 -> loop (i + 1)
            | c -> c
        in loop 0
      | c -> c
    end
  | (Tpat _ | Thash _ | Tobj _), (Tpat _ | Thash _ | Tobj _) ->
    begin
      try unbox_int @@ jg_apply (jg_obj_lookup left "__compare__") [ left ; right ]
      with Not_found -> jg_compare_obj left right
    end
  | _, _ -> -1

and jg_compare_list
  : 'a . filter:('a -> tvalue) -> 'a list -> 'a list -> int =
  fun ~filter x1 x2 -> match x1, x2 with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | x1 :: acc1, x2 :: acc2 ->
      match jg_compare_aux (filter x1) (filter x2) with
      | 0 -> jg_compare_list ~filter acc1 acc2
      | c -> c

and jg_compare_obj left right = match left, right with
  | Tobj x1, Tobj x2 ->
    jg_compare_list ~filter:snd
      (List.sort (fun (a, _) (b, _) -> compare a b) x1)
      (List.sort (fun (a, _) (b, _) -> compare a b) x2)
  | Thash x1, Thash x2 ->
    let x1 = Hashtbl.fold (fun k v acc -> (k, v) :: acc) x1 [] in
    let x2 = Hashtbl.fold (fun k v acc -> (k, v) :: acc) x2 [] in
    jg_compare_obj (Tobj x1) (Tobj x2)
  | _ -> -1

let rec jg_eq_eq_aux left right =
  match left, right with
    | Tint x1, Tint x2 -> x1=x2
    | Tfloat x1, Tfloat x2 -> x1=x2
    | Tstr x1, Tstr x2 -> x1=x2
    | Tbool x1, Tbool x2 -> x1=x2
    | Tlist x1, Tlist x2
    | Tset x1, Tset x2 -> jg_list_eq_eq x1 x2
    | Tobj _, Tobj _ -> jg_obj_eq_eq left right
    | Tarray x1, Tarray x2 -> jg_array_eq_eq x1 x2
    | _, _ -> false

and jg_array_eq_eq a1 a2 =
  try
    array_iter2
      (fun a b ->
         if not @@ jg_eq_eq_aux a b
         then raise @@ Invalid_argument "jg_array_eq_eq")
      a1 a2 ;
    true
  with
    Invalid_argument _ -> false

and jg_list_eq_eq l1 l2 =
  List.length l1 = List.length l2
  && List.for_all2 jg_eq_eq_aux l1 l2

and jg_obj_eq_eq obj1 obj2 =
  let alist1 = unbox_obj obj1 in
  let alist2 = unbox_obj obj2 in
  List.length alist1 = List.length alist2
  &&
  try
    List.for_all
      (fun (prop, value) -> jg_eq_eq_aux value (List.assoc prop alist2))
      alist1
  with
    Not_found -> false
(**/**)

let jg_eq_eq left right =
  Tbool (jg_eq_eq_aux left right)

let jg_not_eq left right =
  Tbool (not @@ jg_eq_eq_aux left right)

let jg_lt left right =
  match left, right with
    | Tint x1, Tint x2 -> Tbool(x1<x2)
    | Tfloat x1, Tfloat x2 -> Tbool(x1<x2)
    | Tstr x1, Tstr x2 -> Tbool(x1<x2)
    | _, _ -> failwith_type_error_2 "jg_lt" left right

let jg_gt left right =
  match left, right with
    | Tint x1, Tint x2 -> Tbool(x1>x2)
    | Tfloat x1, Tfloat x2 -> Tbool(x1>x2)
    | Tstr x1, Tstr x2 -> Tbool(x1>x2)
    | _, _ -> failwith_type_error_2 "jg_gt" left right

let jg_lteq left right =
  match left, right with
    | Tint x1, Tint x2 -> Tbool(x1<=x2)
    | Tfloat x1, Tfloat x2 -> Tbool(x1<=x2)
    | Tstr x1, Tstr x2 -> Tbool(x1<=x2)
    | _, _ -> failwith_type_error_2 "jg_lteq" left right

let jg_gteq left right =
  match left, right with
    | Tint x1, Tint x2 -> Tbool(x1>=x2)
    | Tfloat x1, Tfloat x2 -> Tbool(x1>=x2)
    | Tstr x1, Tstr x2 -> Tbool(x1>=x2)
    | _, _ -> failwith_type_error_2 "jg_gteq" left right

(** [jg_inop x seq] Test if [seq] contains element [x]. *)
let jg_inop left right =
  match left, right with
    | value, Tlist lst -> Tbool (List.exists (jg_eq_eq_aux value) lst)
    | value, Tarray a -> Tbool (array_exists (jg_eq_eq_aux value) a)
    | _ -> Tbool false

(**/**)
let jg_get_kvalue ?(defaults=[]) name kwargs =
  try List.assoc name kwargs with Not_found ->
    (try List.assoc name defaults with Not_found -> Tnull)
(**/**)

(** [jg_upper s] Apply upper case to [s]. *)
let jg_upper x =
  match x with
    | Tstr str -> Tstr (Jg_utils.UTF8.uppercase str)
    | other -> Tstr (string_of_tvalue other)

(** [jg_upper s] Apply lower case to [s]. *)
let jg_lower x =
  match x with
    | Tstr str -> Tstr (Jg_utils.UTF8.lowercase str)
    | other -> Tstr (string_of_tvalue other)

(** [jg_capitalize txt]
    Apply uppercase to the first letter of [txt] and lowercase to the rest *)
let jg_capitalize value =
  match value with
    | Tstr str -> Tstr (Jg_utils.UTF8.capitalize str)
    | _ -> failwith_type_error_1 "jg_capitalize" value

(** [jg_title txt] Apply titlecase (lower case except for first letter
    of all words which is upper cased) to [txt]. *)
let jg_title text =
  match text with
    | Tstr text -> Tstr (Jg_utils.UTF8.titlecase text)
    | _ -> failwith_type_error_1 "jg_title" text

(** [jg_int x] turns [x] into an integer.
    Support int, float and string types. *)
let jg_int x =
  match x with
    | Tint _ -> x
    | Tfloat x -> Tint (int_of_float  x)
    | Tstr s -> Tint (int_of_string s)
    | _ -> failwith_type_error_1 "jg_int" x

(** [jg_float x] turns [x] into a float.
    Support int, float and string types. *)
let jg_float x =
  match x with
    | Tfloat _ -> x
    | Tint x -> Tfloat (float_of_int x)
    | Tstr s -> Tfloat (float_of_string s)
    | _ -> failwith_type_error_1 "jg_float" x

(** [jg_join sep seq] concatenates the string representation of values
    in [seq], inserting the separator [sep] between each. *)
let jg_join join_str lst =
  match join_str, lst with
    | Tstr str, Tlist lst
    | Tstr str, Tset lst ->
      Tstr (String.concat str (List.map string_of_tvalue lst))
    | Tstr str, Tarray array ->
      let buf = Buffer.create 256 in
      let () =
        Array.iteri
          (fun i v ->
             if i > 0 then Buffer.add_string buf str ;
             Buffer.add_string buf (string_of_tvalue v) )
          array
      in Tstr (Buffer.contents buf)
    | _ -> failwith_type_error_2 "jg_join" join_str lst

(** [jg_split pat text] returns the list of all (possibly empty) substrings
    of [text] that are delimited by [pat] regex. *)
let jg_split pat text =
  match pat, text with
    | Tstr pat, Tstr text ->
      let lst =
	Re.Str.split (Re.Str.regexp pat) text |>
	  List.map (fun str -> Tstr str) in
      Tlist lst
  | _ -> failwith_type_error_2 "jg_split" pat text

(** [jg_substring start len s]
    returns a string of length [len], containing the substring of [s]
    that starts at position [start] and has length [len]
  *)
let jg_substring base count str =
  match base, count, str with
    | Tint base, Tint count, Tstr str ->
      Tstr (substring base count str)
    | Tint _, Tint _, Tnull ->
      Tstr ""
    | _ -> failwith_type_error_3 "jg_substring" base count str

(** [jg_truncate len str] is a shorthand for [jg_substring 0 len str] *)
let jg_truncate len str =
  match len, str with
    | Tint len, Tstr str ->
      Tstr (substring 0 len str)
    | _ -> failwith_type_error_2 "jg_truncate" len str

(** [jg_strlen s] returns the length (number of characters) of [s] *)
let jg_strlen x =
  match x with
    | Tstr str -> Tint (Jg_utils.strlen str)
    | _ -> failwith_type_error_1 "jg_strlen" x

(** [jg_length seq] returns the number of of elements in sequence [seq].
    If [seq] is a string [jg_strlen seq] is returned. *)
let jg_length x =
  Tint (jg_length_aux x)

let jg_md5 x =
  match x with
    | Tstr str ->
      Tstr(str |> Jg_utils.UTF8.lowercase |> Digest.string |> Digest.to_hex)
    | _ -> failwith_type_error_1 "jg_md5" x

(** [jg_abs x] Return the absolute value of [x]. *)
let jg_abs value =
  match value with
    | Tint x -> Tint (abs x)
    | _ -> failwith_type_error_1 "jg_abs" value

(** [jg_attr p o]
    Return the [p] property of object [o]. Support dotted notation. *)
let jg_attr prop obj =
  match prop with
  | Tstr path ->
    jg_obj_lookup_path obj (string_split_on_char '.' path)
  | _ -> failwith_type_error_2 "jg_attr" prop obj

(** TODO *)
(* defaults=[ ("width", Tint 80) ] *)
let jg_center ?defaults:_ value =
  value (* TODO *)

(** [jg_default default value]
    Return [value] if different from [Tnull], and [default] otherwise. *)
let jg_default default value =
  match value with
    | Tnull -> default
    | other -> other

(** TODO: keyword arguments *)
(* defaults = [ ("case_sensitive", Tbool true) ; ("by", Tstr "key")] *)
let jg_dictsort ?defaults:_ value =
  match value with
    | Tobj alist ->
      Tobj (List.sort (fun a b -> String.compare (fst a) (fst b)) alist)
    | _ -> value

let jg_reverse lst =
  match lst with
    | Tlist lst -> Tlist (List.rev lst)
    | Tarray a ->
      let len = Array.length a in
      Tarray (Array.init len (fun i -> Array.get a (len - 1 - i)))
    | _ -> failwith_type_error_1 "jg_reverse" lst

(** [jg_last seq].
    Return the last element of sequence [seq].
*)
let jg_last lst =
  match lst with
    | Tlist lst
    | Tset lst ->
      let rec last = function
        | [] -> List.hd [] (* same exception as previous implementation *)
        | [x] -> x
        | _ :: tl -> last tl in
      last lst
    | Tarray a -> Array.get a (Array.length a - 1)
    | _ -> failwith_type_error_1 "jg_last" lst

let jg_random lst =
  let knuth a =
    for i = Array.length a - 1 downto 1 do
      let j = Random.int i in
      let t = a.(i) in
      a.(i) <- a.(j);
      a.(j) <- t
    done ;
    a
  in
  match lst with
    | Tlist l -> Tlist (Array.to_list @@ knuth @@ Array.of_list l)
    | Tarray a -> Tarray (knuth @@ Array.copy a)
    | _ -> failwith_type_error_1 "jg_random" lst

let jg_replace src dst str =
  match src, dst, str with
    | Tstr src, Tstr dst, Tstr str ->
      Tstr (Re.Str.global_replace (Re.Str.regexp src) dst str)
    | _ -> failwith_type_error_3 "jg_replace" src dst str

(** [jg_add a b] is [a + b]. It only support int and float.
    If both [a] and [b] are int, the result is an int, it is a float otherwise.
 *)
let jg_add a b = match a, b with
  | Tint a, Tint b -> Tint (a + b)
  | Tfloat a, Tfloat b -> Tfloat (a +. b)
  | Tint a, Tfloat b
  | Tfloat b, Tint a -> Tfloat (float_of_int a +. b)
  | _ -> failwith_type_error_2 "jg_add" a b

(** [jg_sum seq] is the sum of elements in [seq] produced using {!val:jg_add}.
 *)
let jg_sum lst =
  match lst with
  | Tset l
  | Tlist l -> List.fold_left jg_add (Tint 0) l
  | Tarray a -> Array.fold_left jg_add (Tint 0) a
  | _ -> failwith_type_error_1 "jg_sum" lst

(** [jg_trim s] returns [s] with leading and trailing whitespace. *)
let jg_trim str =
  match str with
    | Tstr str -> Tstr (Jg_utils.UTF8.trim str)
    | _ -> failwith_type_error_1 "jg_trim" str

(** [jg_list x] convert [x] to a list. Support list, tuple, string and array. *)
let jg_list value =
  match value with
    | Tlist lst | Tset lst -> Tlist lst
    | Tstr str ->
      let len = strlen str in
      let rec iter ret i =
	if i >= len then
	  List.rev ret
	else
	  let s1 = Tstr (substring i 1 str) in
	  iter (s1 :: ret) (i+1) in
      Tlist (iter [] 0)
    | Tarray a -> Tlist (Array.to_list a)
    | _ -> failwith_type_error_1 "jg_list" value

(* FIXME: What if we do not want to fill last chunk? remove defaults? *)
(** [jg_batch count value] split [value] into chunks containing [count] values.

    If [fill_with=v] keyword argument is given, the last chunk will be filled
    with [v] instead of [null] if needed.
 *)
let jg_batch ?(defaults = [ ("fill_with", Tnull) ]) ?(kwargs = []) count value =
  let fill_value = jg_get_kvalue "fill_with" kwargs ~defaults in
  match count, value with
    | Tint slice_count, Tlist lst ->
      let pad = match fill_value with Tnull -> None | other -> Some other in
      let rec batch ret left_count rest =
	if left_count > slice_count then
	  batch ((box_list @@ take slice_count rest) :: ret) (left_count - slice_count) (after slice_count rest)
	else if left_count > 0 then
          batch ((box_list @@ take slice_count rest ?pad) :: ret) 0 []
	else
	  box_list @@ List.rev ret in
      batch [] (List.length lst) lst
    | Tint c, Tarray arr ->
      let len1 = Array.length arr in
      let len2 = len1 / c + (if len1 mod c = 0 then 0 else 1) in
      box_array @@
      Array.init len2 @@ fun i ->
      if i * c + c < len1
      then box_array (Array.sub arr (i * c) c)
      else if fill_value = Tnull then box_array @@ Array.init (len1 mod c) (fun j -> arr.(i * c + j))
      else box_array @@ Array.init c (fun j -> if i * c + j < len1 then arr.(i * c + j) else fill_value)
    | a, b -> failwith_type_error_2 "jg_batch" a b

(** [jg_slice nb value] split [value] into [nb] chunks.

    If [fill_with=v] keyword argument is given, [v] will be used
    when buckets will need to be filled so that they all contain the
    same number of elements.

    See also {!val:jg_batch}.
 *)
let rec jg_slice ?(kwargs=[]) count value =
  let pad = jg_get_kvalue "fill_with" kwargs in
  match count, value with
  | Tint c, Tlist l ->
    let len = List.length l in
    let min = len / c in
    let rest = len mod c in
    let max = if rest = 0 then min else min + 1 in
    let nb i = if i < rest then max else min in
    let rec slice res i rem =
      if i = c then box_list @@ List.rev res
      else
        let n = nb i in
        let s =
          if n < max && pad <> Tnull then take n rem @ [ pad ] else take n rem
        in
        slice
          (box_list s :: res)
          (i + 1)
          (after n rem)
    in
    slice [] 0 l
  | _ -> jg_slice count (jg_list value) ~kwargs

(** [jg_sublist i len list] returns the sub-list of [list],
    starting at the [i]-th element, and containing [len] elements, or
    all elements after the [i]-th if len is [null].
*)
let jg_sublist base count lst =
  match base, count, lst with
    | Tint base, Tint count, Tlist lst -> Tlist (after base lst |> take count)
    | Tint base, Tnull, Tlist lst -> Tlist (after base lst)
    | _ -> failwith_type_error_3 "jg_sublist" base count lst

(** [jg_wordcount str] count (non-empty) words in [str] using spacing
    as delimiter between words. *)
let jg_wordcount str =
  match str with
    | Tstr str ->
      let space = ref true in
      let n = ref 0 in
      Uutf.String.fold_utf_8
        (fun _ _ -> function
           | `Uchar u when Jg_utils.UTF8.is_space u -> space := true
           | `Uchar _ when !space -> space := false ; incr n
           | _ -> ())
        () str ;
      Tint !n
    | _ -> failwith_type_error_1 "jg_word_count" str

(* FIXME: common method make no sens to me... *)
(** [jg_round meth x] rounds [x] using [meth] rounding method.
    Supported methods are ["ceil"] and ["floor"]. *)
let jg_round how value =
  match how, value with
    | _, Tint x -> Tint x
    | Tstr "floor", Tfloat x -> Tfloat (floor x)
    | Tstr ("ceil" | "common"), Tfloat x -> Tfloat (ceil x)
    | Tstr other, Tfloat _ ->
      failwith @@
      spf "invalid args:round method %s not supported(jg_round)" other
    | _ -> failwith_type_error_2 "jg_round" how value

let jg_fmt_float digit_count value =
  match digit_count, value with
    | Tint digit_count, Tfloat value ->
      let fmt = Scanf.format_from_string (spf "%%.%df" digit_count) "%f" in
      Tfloat (float_of_string @@ spf fmt value)
    | _, _ -> failwith_type_error_2 "jg_fmt_float(count, value)" digit_count value

(** [jg_range start stop] returns a sequence of values going from [start]
     to (or downto) [stop].
    Support integers and string with one ascii character. *)
let jg_range start stop =
  let range start stop fn =
    let sign = if start < stop then (+) else (-) in
    let len = (if start < stop then stop - start else start - stop) + 1 in
    Tarray (Array.init len (fun i -> fn (sign start i)))
  in
  match start, stop with
    | Tint start, Tint stop -> range start stop box_int
    | Tstr strt, Tstr stp when String.length strt = 1 && String.length stp = 1 ->
      let strt = Char.code @@ String.unsafe_get strt 0 in
      let stp = Char.code @@ String.unsafe_get stp 0 in
      if strt < 0 || strt > 255 || stp < 0 || stp > 255
      then failwith_type_error_2 "jg_range(start, stop)" start stop
      else range strt stp (fun i -> Tstr (String.make 1 @@ Char.chr i))
    | _ -> failwith_type_error_2 "jg_range(start, stop)" start stop

let jg_urlize_regexp =
  let open Re in
  lazy (compile @@
        group @@
        seq
          [ alt [ str "http" ; str "https" ; str "ftp" ; str "file" ]
          ; str "://"
          ; rep (alt [ alnum ; set "._-@:~/\\" ])
          ; opt (seq [ char '?' ; rep (alt [ alnum ; set "._-@:!/\\" ; set "%+=&" ]) ])
          ; opt (seq [ char '#' ; rep (alt [ alnum ; set "._-" ]) ])
          ; compl [ set ":.?" ]
          ])

let jg_urlize text =
  match text with
    | Tstr text ->
      Tstr (Re.replace (Lazy.force jg_urlize_regexp) text
              ~f:(fun g -> let h = Re.Group.get g 1 in "<a href=\"" ^ h ^ "\">" ^ h ^ "</a>") )
    | _ -> failwith_type_error_1 "jg_urlize" text

let jg_striptags_regexp =
  let open Re in
  lazy (compile @@ seq [ char '<' ; opt (char '?') ; rep1 (compl [ char '>' ]) ; char '>' ])

let jg_striptags text =
  match text with
    | Tstr text ->
      Tstr (Re.replace_string (Lazy.force jg_striptags_regexp) ~by:"" text)
    | _ -> failwith_type_error_1 "jg_striptags" text

(** [jg_sort ?kwargs seq] sort the [seq].
    Support the following keyword arguments:
    - ["reverse"]: sorted in descending order.
    - ["attribute"]: use attribute of elements to sort the sequence.
      Support dotted notation.
    - ["compare"]: provide a comparison function to be use instead of the
      built-in one.
 *)
let jg_sort ?(kwargs=[]) lst =
  let reverse = ref false in
  let attribute = ref "" in
  let jg_compare = ref jg_compare_aux in
  List.iter (function ("reverse", Tbool true) -> reverse := true
                    | ("attribute", Tstr name) -> attribute := name
                    | ("compare", fn) -> jg_compare := fun a b -> unbox_int (jg_apply fn [ a ; b ])
                    | (kw, _) -> failwith kw) kwargs;
  let compare = match !attribute with
    | "" -> !jg_compare
    | att ->
      let path = string_split_on_char '.' att in
      fun a b -> !jg_compare (jg_obj_lookup_path a path) (jg_obj_lookup_path b path) in
  let compare = if !reverse then fun a b -> compare b a else compare in
  match lst with
    | Tlist l -> Tlist (List.sort compare l)
    | Tarray a -> Tarray (let a = Array.copy a in Array.sort compare a ; a)
    | _ -> failwith_type_error_1 "jg_sort" lst

(** [jg_xmlattr o] Format a string containing keys/values representation
    of object [o] so it can be used as xml attributes.
    i.e. ['key1="value1" key2="value2"] *)
let jg_xmlattr obj =
  match obj with
    | Tobj alist ->
      List.map (fun (name, value) -> spf "%s=\"%s\"" name (string_of_tvalue value)) alist |>
	String.concat " " |> box_string
    | _ -> failwith_type_error_1 "jg_xmlattr" obj

let jg_wordwrap width break_long_words text =
  match width, break_long_words, text with
    | Tint width, Tbool break_long_words, Tstr text ->
      let concat_line s1 s2 = if s1 = "" then s2 else String.concat " " [s1; s2] in
      let push_line s1 s2 = if s1 = "" then s2 else String.concat "\n" [s1; s2] in
      let rec iter lines line count = function
	| "" :: rest -> iter lines line count rest
	| word :: rest ->
	  let len = strlen word in
	  if count + len + 1 <= width then
	    iter lines (concat_line line word) (count + len + 1) rest
	  else if break_long_words then
	    let left_len = width - count in
	    let over_len = len + 1 - left_len in
	    let left_word = substring 0 left_len word in
	    let over_word = substring left_len over_len word in
	    iter (push_line lines @@ concat_line line left_word) "" 0 (over_word :: rest)
	  else
	    iter (push_line lines @@ concat_line line word) "" 0 rest
	| [] -> if line = "" then lines else push_line lines line in
      let words = Jg_utils.UTF8.split text in
      Tstr (iter "" "" 0 words)
    | _ -> failwith_type_error_3 "jg_wordwrap" width break_long_words text

module JgHashtbl = Hashtbl.Make (struct
    type t = Jg_types.tvalue
    let equal a b = jg_compare_aux a b = 0
    let hash = Hashtbl.hash
  end)

let fun_or_attribute ~kwargs ~arg =
  match arg with
  | Tfun fn -> fun x -> fn ~kwargs x
  | _ -> match kwargs with
    | [ ("attribute", Tstr path) ] ->
      fun x -> jg_obj_lookup_path x (string_split_on_char '.' path)
    | _ -> raise Not_found

let jg_groupby_aux fn length iter collection =
  let h = JgHashtbl.create length in
  iter
    (fun obj ->
      let k = fn obj in
      if JgHashtbl.mem h k then
        JgHashtbl.replace h k (obj :: JgHashtbl.find h k)
      else
        JgHashtbl.add h k [obj]) collection;
  box_list @@ JgHashtbl.fold
    (fun key list acc ->
      Tpat (function
      | "grouper" -> key
      | "list" -> Tlist (List.rev list)
      | _ -> raise Not_found) :: acc)
    h []

(** [jg_groupby ?kwargs fn seq]
    For each element [x] of [seq], [fn x] returns a key used to group elements
    with the same key together.

    Resulting list is a list of objects with two fields: [gouper], the key,
    and [list], the list containing values from [seq] whose key is [grouper].

   ["attribute"=path] keyword argument will apply [attr(path)] function.
  *)
let jg_groupby ?(kwargs=[]) fn list =
  try
    let f = fun_or_attribute ~kwargs ~arg:fn in
    match list with
    | Tarray ary -> jg_groupby_aux f (Array.length ary) Array.iter ary
    | Tlist list -> jg_groupby_aux f (List.length list) List.iter list
    | _ -> failwith_type_error_2 "jg_groupby" fn list
  with Not_found ->
    failwith_type_error "jg_groupby" @@ ("", fn) :: ("", list) :: kwargs

(** [jg_map ?kwargs fn seq] build the sequence [seq']
    such as [ seq'[n] = fn (seq[n]) ].

   ["attribute"=path] keyword argument will apply [attr(path)] function.
 *)
let jg_map ?(kwargs=[]) fn list =
  try
    let f = fun_or_attribute ~kwargs ~arg:fn in
    match list with
    | Tarray x -> Tarray (Array.map f x)
    | Tlist x -> Tlist (List.map f x)
    | _ -> failwith_type_error_2 "jg_map" fn list
  with Not_found ->
    failwith_type_error "jg_map" @@ ("", fn) :: ("", list) :: kwargs

let jg_max_min_aux is_max fst iter value kwargs =
  let compare =
    match kwargs with
    | [("attribute", Tstr att)] ->
      let path = string_split_on_char '.' att in
      fun a b -> jg_compare_aux (jg_obj_lookup_path a path) (jg_obj_lookup_path b path)
    | _ -> jg_compare_aux in
  let compare = if is_max then compare else fun a b -> compare b a in
  let result = ref fst in
  iter (fun x -> if compare !result x = -1 then result := x) value;
  !result

(** [jg_max ?kwargs sequence] return the maximal value of [sequence].
    [kwargs] may be used to specify an attribute (["attribute", str])
    to use when comparing values (dotted notation is supported).
 *)
let jg_max ?(kwargs=[]) arg =
  match arg with
  | Tarray [||] | Tlist [] -> Tnull
  | Tarray a -> jg_max_min_aux true a.(0) Array.iter a kwargs
  | Tlist (hd :: tl) -> jg_max_min_aux true hd List.iter tl kwargs
  | _ -> failwith_type_error_1 "jg_max" arg

(* TODO: ~kwargs:["compare", fn] *)
(** [jg_min ?kwargs sequence] return the minimal value of [sequence].
    [kwargs] may be used to specify an attribute (["attribute", str])
    to use when comparing values (dotted notation is supported).
 *)
let jg_min ?(kwargs=[]) arg =
  match arg with
  | Tarray [||] | Tlist [] -> Tnull
  | Tarray a -> jg_max_min_aux false a.(0) Array.iter a kwargs
  | Tlist (hd :: tl) -> jg_max_min_aux false hd List.iter tl kwargs
  | _ -> failwith_type_error_1 "jg_min" arg

let jg_select_aux name select =
  fun fn seq ->
    let selected l = match fn with
      | Tfun fn -> Tlist (List.filter (fun x -> select (unbox_bool @@ fn x)) l)
      | _ -> failwith_type_error_2 name fn seq
    in
    match seq with
    | Tarray a -> selected (Array.to_list a)
    | Tlist l -> selected l
    | _ -> failwith_type_error_2 name fn seq

(** [jg_reject fn seq]
    returns the elements of [seq] that {b don't} satify [fn]. *)
let jg_reject = jg_select_aux "jg_reject" not

(** [jg_select fn seq]
    returns the elements of [seq] that satify [fn]. *)
let jg_select = jg_select_aux "jg_select" (fun x -> x)

(** [jg_fold fn acc [b1, ..., bn]] is [fn (... (fn (fn acc b1) b2) ...) bn].
*)
let jg_fold = fun fn acc seq ->
  let wrap fn acc x = jg_apply fn [ acc ; x ] in
  match fn, seq with
  | Tfun _, Tarray a -> Array.fold_left (wrap fn) acc a
  | Tfun _, Tlist l -> List.fold_left (wrap fn) acc l
  | Tfun _, Tstr s ->
    let len = UTF8.length s in
    let rec loop i acc =
      if i >= len then acc
      else
        let x = UTF8.sub s i 1 in
        loop (i + 1) ((wrap fn) acc (Tstr x))
    in
    loop 0 acc
  | _ -> failwith_type_error_3 "jg_fold" fn acc seq

(** [for_all fn seq]
    checks if all elements of the sequence [seq] satisfy the predicate [fn].
*)
let jg_forall = fun fn seq ->
  match seq, fn with
  | (Tlist l, Tfun fn) -> Tbool (List.for_all (fun x -> unbox_bool @@ fn x) l)
  | (Tarray l, Tfun fn) -> Tbool (Jg_utils.array_for_all (fun x -> unbox_bool @@ fn x) l)
  | _ -> failwith_type_error_2 "jg_forall" fn seq

(** [jg_test_divisibleby divisor dividend]
    tests if [dividend] is divisible by [divisor]. *)
let jg_test_divisibleby num target =
  match num, target with
    | Tint 0, _ -> Tbool(false)
    | Tint n, Tint t ->  Tbool(t mod n = 0)
    | _ -> Tbool(false)

(** [jg_test_even x] tests if [x] is even (only works with int). *)
let jg_test_even x =
  match x with
    | Tint x -> Tbool(x mod 2 = 0)
    | _ -> Tbool(false)

(** [jg_test_odd x] tests if [x] is odd (only works with int). *)
let jg_test_odd x =
  match x with
    | Tint x -> Tbool(x mod 2 = 1)
    | _ -> Tbool(false)

let jg_test_iterable_aux = function
  | Tlist _ | Tset _ | Thash _ | Tobj _ | Tarray _ | Tstr _ | Tnull-> true
  | _ -> false

(** [jg_test_upper x] tests if [x] is iterable. *)
let jg_test_iterable = fun x -> Tbool (jg_test_iterable_aux x)

(** [jg_test_upper x] tests if [x] is an lowercased string. *)
let jg_test_lower x =
  match x with
    | Tstr str -> Tbool(Jg_utils.UTF8.is_lower str)
    | _ -> Tbool(false)

(** [jg_test_upper x] tests if [x] is an uppercased string. *)
let jg_test_upper x =
  match x with
    | Tstr str -> Tbool(Jg_utils.UTF8.is_upper str)
    | _ -> Tbool(false)

(** [jg_test_number x] tests if [x] is a number (i.e. an int or a float). *)
let jg_test_number x =
  match x with
    | Tint _ -> Tbool(true)
    | Tfloat _ -> Tbool(true)
    | _ -> Tbool(false)

(** [jg_test_sameas x y] tests if [y] and [x] are physically equals. *)
let jg_test_sameas value target =
  match value, target with
    | Tstr x, Tstr y -> Tbool(x == y)
    | Tint x, Tint y -> Tbool(x == y)
    | Tfloat x, Tfloat y -> Tbool(x == y)
    | Tbool x, Tbool y -> Tbool(x == y)
    | Tfun x, Tfun y -> Tbool(x == y)
    | Tobj x, Tobj y -> Tbool(x == y)
    | Tlist x, Tlist y -> Tbool(x == y)
    | Tset x, Tset y -> Tbool(x == y)
    | Tarray x, Tarray y -> Tbool(x == y)
    | _ -> Tbool(false)

(** [jg_test_sequence x] tests if [x] is sequence (i.e. a list or an array). *)
let jg_test_sequence target =
  jg_test_iterable target

(** [jg_test_string x] tests if [x] is of type string. *)
let jg_test_string target =
  jg_strp target

let std_filters = [
  (* built-in filters *)
  ("abs", func_1 jg_abs);
  ("capitalize", func_1 jg_capitalize);
  ("escape", func_1 jg_escape_html);
  ("e", func_1 jg_escape_html); (* alias for escape *)
  ("float", func_1 jg_float);
  ("int", func_1 jg_int);
  ("last", func_1 jg_last);
  ("length", func_1 jg_length);
  ("list", func_1 jg_list);
  ("lower", func_1 jg_lower);
  ("max", func_kw_1 jg_max);
  ("md5", func_1 jg_md5);
  ("min", func_kw_1 jg_min);
  ("strlen", func_1 jg_strlen);
  ("sum", func_1 jg_sum);
  ("striptags", func_1 jg_striptags);
  ("sort", func_kw_1 jg_sort);
  ("upper", func_1 jg_upper);
  ("random", func_1 jg_random);
  ("reverse", func_1 jg_reverse);
  ("title", func_1 jg_title);
  ("trim", func_1 jg_trim);
  ("urlize", func_1 jg_urlize);
  ("wordcount", func_1 jg_wordcount);
  ("xmlattr", func_1 jg_xmlattr);

  ("attr", func_2 jg_attr);
  ("batch", func_kw_2 (jg_batch ?defaults:None));
  ("default", func_2 jg_default);
  ("d", func_2 jg_default); (* alias for default *)
  ("fmt_float", func_2 jg_fmt_float);
  ("join", func_2 jg_join);
  ("split", func_2 jg_split);
  ("slice", func_kw_2 jg_slice);
  ("truncate", func_2 jg_truncate);
  ("range", func_2 jg_range);
  ("round", func_2 jg_round);
  ("groupby", func_kw_2 jg_groupby);
  ("map", func_kw_2 jg_map);
  ("reject", func_2 jg_reject);
  ("select", func_2 jg_select);
  ("nth", func_2 jg_nth);
  ("forall", func_2 jg_forall);

  ("replace", func_3 jg_replace);
  ("substring", func_3 jg_substring);
  ("sublist", func_3 jg_sublist);
  ("wordwrap", func_3 jg_wordwrap);
  ("fold", func_3 jg_fold);

  (* built-in tests *)
  ("divisibleby", func_2 jg_test_divisibleby);
  ("even", func_1 jg_test_even);
  ("iterable", func_1 jg_test_iterable);
  ("number", func_1 jg_test_number);
  ("odd", func_1 jg_test_odd);
  ("sameas", func_2 jg_test_sameas);
  ("compare", func_2 jg_compare);
  ("sequence", func_1 jg_test_sequence);
  ("string", func_1 jg_test_string);
]

let jg_load_extensions extensions =
  List.iter (fun ext ->
    try
      Dynlink.loadfile ext
    with
	Dynlink.Error e -> failwith @@ Dynlink.error_message e
  ) extensions

(** [jg_init_context ?models output env]
    Define a context to use with evaluation functions from
    {!module:Jg_interp}.
    See {!type:Jg_types.context}.
*)
let jg_init_context ?(models=[]) output env =
  let model_frame = Hashtbl.create (List.length models) in
  let top_frame = Hashtbl.create (List.length std_filters + List.length env.filters + 2) in
  let ctx = {
    frame_stack = [ model_frame ; top_frame ];
    macro_table = Hashtbl.create 10;
    namespace_table = Hashtbl.create 10;
    active_filters = [];
    serialize = false;
    output
  } in
  let set_values hash alist = List.iter (fun (n, v) -> Hashtbl.add hash n v) alist in
  set_values model_frame models;
  set_values top_frame std_filters;
  set_values top_frame env.filters;

  Hashtbl.add top_frame "jg_is_autoescape" (Tbool env.autoescape);
  ctx
