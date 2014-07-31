(*
  jg_runtime.ml

  Copyright (c) 2011- by Masaki WATANABE

  License: see LICENSE
*)
open Jg_utils
open Jg_types

let box_int i = Tint i
let box_float f = Tfloat f
let box_string s = Tstr s
let box_bool b = Tbool b
let box_list lst = Tlist lst
let box_set lst = Tset lst
let box_obj alist = Tobj alist
let box_hash hash = Thash hash

let unbox_int = function
  | Tint x -> x
  | _ -> failwith "invalid arg:not int(unbox_int)"
;;

let unbox_float = function
  | Tfloat f -> f
  | _ -> failwith "invalid arg:not float(unbox_float)"
;;

let unbox_string = function
  | Tstr s -> s
  | _ -> failwith "invalid arg:not string(unbox_string)"
;;

let unbox_bool = function
  | Tbool b -> b
  | _ -> failwith "invalid arg:not bool(unbox_bool)"
;;

let unbox_list = function
  | Tlist lst -> lst
  | _ -> failwith "invalid arg:not list(unbox_list)"
;;

let unbox_set = function
  | Tset lst -> lst
  | _ -> failwith "invalid arg:not set(unbox_set)"
;;

let unbox_obj = function
  | Tobj alist -> alist
  | _ -> failwith "invalid arg:not obj(unbox_obj)"
;;

let unbox_hash = function
  | Thash hash -> hash
  | _ -> failwith "invalid arg:not hahs(unbox_hash)"
;;

let merge_defaults defaults kwargs =
  List.map (fun (name, value) ->
    try (name, List.assoc name kwargs) with Not_found -> (name, value)
  ) defaults
;;

let union_defaults defaults kwargs =
  let rec append alist = function
    | (name, value) :: rest ->
      if List.mem_assoc name alist then
	append alist rest
      else
	append ((name,value) :: alist) rest
    | [] -> alist in
  append (merge_defaults defaults kwargs) kwargs
;;

let string_of_tvalue = function
  | Tint x -> string_of_int x
  | Tfloat x -> string_of_float x
  | Tstr x -> x
  | Tbool x -> string_of_bool x
  | Tobj x -> "<obj>"
  | Thash x -> "<hash>"
  | Tlist x -> "<list>"
  | Tset x -> "<set>"
  | Tfun _ -> "<fun>"
  | Tnull -> ""
;;

let type_string_of_tvalue = function
  | Tint x -> "int"
  | Tfloat x -> "float"
  | Tstr x -> "string"
  | Tbool x -> "bool"
  | Tobj x -> "obj"
  | Thash x -> "hash"
  | Tlist x -> "list"
  | Tset x -> "set"
  | Tfun _ -> "function"
  | Tnull -> "null"
;;

let dump_expr = function
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
  | ListExpr(_) -> "ListExpr"
  | SetExpr(_) -> "SetExpr"
  | ObjExpr(_) -> "ObjExpr"
  | InOpExpr(_,_) -> "InOpExpr"
  | KeywordExpr(_,_) -> "KeywordExpr"
  | AliasExpr(_,_) -> "AliasExpr"
  | ApplyExpr(_,_) -> "ApplyExpr"
  | TestOpExpr(_,_) -> "TestOpExpr"
;;

let jg_strp = function
  | Tstr _ -> Tbool true
  | _ -> Tbool false
;;

let jg_intp = function
  | Tint _ -> Tbool true
  | _ -> Tbool false
;;
    
let jg_floatp = function
  | Tfloat _ -> Tbool true
  | _ -> Tbool false
;;

let jg_listp = function
  | Tlist _ -> Tbool true
  | _ -> Tbool false
;;

let jg_setp = function
  | Tset _ -> Tbool true
  | _ -> Tbool false
;;

let jg_objp = function
  | Tobj _ -> Tbool true
  | _ -> Tbool false
;;

let jg_hashp = function
  | Thash _ -> Tbool true
  | _ -> Tbool false
;;

let jg_funp = function
  | Tfun _ -> Tbool true
  | _ -> Tbool false
;;

let jg_push_frame ctx =
  {ctx with frame_stack = (Hashtbl.create 10) :: ctx.frame_stack}
;;

let jg_pop_frame ctx =
  match ctx.frame_stack with
    | [] -> ctx (* never happen *)
    | [top_frame] -> ctx (* because top frame always remain *)
    | frame :: rest -> {ctx with frame_stack = rest} (* other case, pop latest *)
;;

let jg_set_value ctx name value =
  match ctx.frame_stack with
    | [] ->
      let frame = Hashtbl.create 10 in
      Hashtbl.add frame name value;
      {ctx with frame_stack = frame :: []}
    | frame :: rest ->
      Hashtbl.add frame name value;
      {ctx with frame_stack = frame :: rest}
;;

let jg_set_values ctx names values =
  List.fold_left (fun ctx (name, value) ->
    jg_set_value ctx name value
  ) ctx @@ List.combine names @@ take (List.length names) values ~pad:Tnull
;;

let jg_bind_names ctx names values =
  let set_values names values =
    List.fold_left (fun ctx (name, value) ->
      jg_set_value ctx name value
    ) ctx @@ List.combine names @@ Jg_utils.take (List.length names) values ~pad:Tnull in
  match names, values with
    | [name], value -> jg_set_value ctx name value
    | name :: rest, Tset values -> set_values names values
    | _ -> ctx
;;

let rec jg_get_value ctx name =
  let rec get_value name = function
    | frame :: rest ->
      (try Hashtbl.find frame name with Not_found -> get_value name rest)
    | [] -> Tnull in
  get_value name ctx.frame_stack
;;

let jg_get_func ctx name = 
  match jg_get_value ctx name with
    | Tfun f -> Tfun f
    | _ -> failwith @@ spf "undefined function %s" name
;;

let jg_set_macro ctx name macro =
  Hashtbl.add ctx.macro_table name macro;
  ctx
;;

let jg_get_macro ctx name =
  try Some(Hashtbl.find ctx.macro_table name) with Not_found -> None
;;

let jg_remove_macro ctx name =
  Hashtbl.remove ctx.macro_table name;
  ctx
;;

let jg_set_filter ctx name =
  {ctx with active_filters = name :: ctx.active_filters}
;;

let jg_pop_filter ctx =
  match ctx.active_filters with
    | [] -> ctx
    | head :: rest -> {ctx with active_filters = rest}
;;

let jg_escape_html str kwargs =
  match str with
    | Tstr str -> Tstr(Jg_utils.escape_html str)
    | other -> Tstr(Jg_utils.escape_html @@ string_of_tvalue other)
;;

let jg_apply ?(name="<lambda>") ?(kwargs=[]) f args =
  match f with
    | Tfun fn -> fn args kwargs
    | _ -> failwith @@ spf "invalid apply: %s(%s) is not function(%s)" name (string_of_tvalue f) (type_string_of_tvalue f)
;;

let jg_apply_filters ?(autoescape=true) ?(safe=false) ctx text filters =
  let (safe, text) = List.fold_left (fun (safe, text) name ->
    if name = "safe" then
      (true, text)
    else if name = "escape" && autoescape = true then
      (safe, text)
    else
      (safe, jg_apply (jg_get_func ctx name) [text] ~name)
  ) (safe, text) filters in
  if safe || not autoescape then text else jg_escape_html text []
;;

let jg_output ?(autoescape=true) ?(safe=false) ctx value =
  (match ctx.active_filters, safe, value with
    | [], true, Tstr text -> Buffer.add_string ctx.buffer text
    | [], true, value -> Buffer.add_string ctx.buffer @@ string_of_tvalue value
    | _ ->
      Buffer.add_string ctx.buffer @@ string_of_tvalue @@
	jg_apply_filters ctx value ctx.active_filters ~safe ~autoescape
  );
  ctx
;;

let jg_obj_lookup ctx obj prop_name =
  match obj with
    | Tobj(alist) -> (try List.assoc prop_name alist with Not_found -> Tnull)
    | Thash(hash) -> (try Hashtbl.find hash prop_name with Not_found -> Tnull)
    | _ -> failwith "jg_obj_lookup:not object"
;;      

let jg_obj_lookup_by_name ctx obj_name prop_name =
  match jg_get_value ctx obj_name with
    | Tobj(alist) as obj -> jg_obj_lookup ctx obj prop_name
    | Thash(hash) as hobj -> jg_obj_lookup ctx hobj prop_name
    | _ -> (try Jg_stub.get_func obj_name prop_name with Not_found -> Tnull)
;;

let jg_iter ctx iterator f iterable =
  let lst =
    match iterable with
      | Tlist lst -> lst
      | Tset lst -> lst
      | _ -> failwith "jg_iter:not iterable object" in
  let len = List.length lst in
  let rec iter ctx i = function
    | [] -> ctx
    | item :: rest ->
      let ctx = jg_push_frame ctx in
      let ctx = jg_bind_names ctx iterator item in
      let cycle = Tfun (fun args kwargs ->
	let args_len = List.length args in
	List.nth args (i mod args_len)
      ) in
      let ctx = jg_set_value ctx "loop" (
	Tobj [
	  ("index0", Tint i);
	  ("index", Tint (i+1));
	  ("revindex0", Tint (len - i - 1));
	  ("revindex", Tint (len - i));
	  ("first", Tbool (i=0));
	  ("last", Tbool (i=len-1));
	  ("length", Tint len);
	  ("cycle", cycle);
	]) in
      let ctx = f ctx in
      let ctx = jg_pop_frame ctx in
      iter ctx (i+1) rest in
  iter ctx 0 lst
;;

let jg_eval_macro ?(caller=false) env ctx macro_name args kwargs macro f =
  match macro with
    | Macro(arg_names, defaults, code) ->
      let args_len = List.length args in
      let arg_names_len = List.length arg_names in
      let ctx = jg_push_frame ctx in
      let ctx = jg_set_value ctx "varargs" @@ Tlist (Jg_utils.after arg_names_len args) in
      let ctx = jg_set_value ctx "kwargs" @@ Tobj kwargs in
      let ctx = jg_set_value ctx macro_name @@ Tobj [
	("name", Tstr macro_name);
	("arguments", Tlist (List.map box_string arg_names));
	("defaults", Tobj defaults);
	("catch_kwargs", Tbool (kwargs <> []));
	("catch_vargs", Tbool (args_len > arg_names_len));
	("caller", Tbool caller);
      ] in
      let ctx = List.fold_left (fun ctx (name, value) ->
	jg_set_value ctx name value
      ) ctx @@ List.combine arg_names (Jg_utils.take arg_names_len args ~pad:Tnull) in
      let ctx = List.fold_left (fun ctx (name, value) ->
	jg_set_value ctx name value
      ) ctx @@ merge_defaults defaults kwargs in
      let ctx = List.fold_left (fun ctx (name, value) ->
	try jg_set_value ctx name @@ List.assoc name kwargs with Not_found ->
	  jg_set_value ctx name value
      ) ctx defaults in
      let ctx = f ctx code in
      jg_pop_frame ctx
;;

let jg_test_defined ctx name =
  match jg_get_value ctx name with
    | Tnull -> Tbool(false)
    | _ -> Tbool(true)
;;

let jg_test_undefined ctx name =
  match jg_test_defined ctx name with
    | Tbool status -> Tbool (not status)
    | _ -> failwith "invalid test:jg_test_defined"
;;

let jg_test_obj_defined ctx obj_name prop_name =
  match jg_get_value ctx obj_name with
    | Tobj(alist) -> Tbool (List.mem_assoc prop_name alist)
    | _ -> Tbool(false)
;;

let jg_test_obj_undefined ctx obj_name prop_name =
  match jg_test_obj_defined ctx obj_name prop_name with
    | Tbool status -> Tbool (not status)
    | _ -> failwith "invalid test:jg_test_obj_defined"
;;

let jg_test_escaped ctx = 
  Tbool(List.mem "safe" @@ ctx.active_filters)
;;

let jg_test_none ctx name =
  match jg_get_value ctx name with
    | Tnull -> Tbool(true)
    | _ -> Tbool(false)
;;

let jg_negative = function
  | Tint x -> Tint(-x)
  | Tfloat x -> Tfloat(-.x)
  | _ -> failwith "jg_negative:type error"
;;

let jg_is_true = function
  | Tbool x -> x
  | Tstr x -> not (x = "")
  | Tint x -> x != 0
  | Tfloat x -> x != 0.0
  | Tlist x -> List.length x > 0
  | Tset x -> List.length x > 0
  | Tobj x -> List.length x > 0
  | Thash x -> Hashtbl.length x > 0
  | Tnull -> false
  | Tfun(f) -> failwith "jg_is_true:type error(function)"
;;

let jg_not x = 
  Tbool (not (jg_is_true x))
;;

let jg_plus left right =
  match left, right with
    | Tint x1, Tint x2 -> Tint(x1+x2)
    | Tint x1, Tfloat x2 -> Tfloat(float_of_int x1+.x2)
    | Tint x1, Tstr x2 -> Tstr (String.concat "" [string_of_int x1; x2])

    | Tfloat x1, Tfloat x2 -> Tfloat(x1+.x2)
    | Tfloat x1, Tint x2 -> Tfloat(x1+.float_of_int x2)
    | Tfloat x1, Tstr x2 -> Tstr (String.concat "" [string_of_float x1; x2])

    | Tstr x1, Tstr x2 -> Tstr (String.concat "" [x1; x2])
    | Tstr x1, Tint x2 -> Tstr (String.concat "" [x1; string_of_int x2])
    | Tstr x1, Tfloat x2 -> Tstr (String.concat "" [x1; string_of_float x2])

    | _, _ -> failwith "jg_plus:type error"
;;

let jg_minus left right =
  match left, right with
    | Tint x1, Tint x2 -> Tint(x1-x2)
    | Tfloat x1, Tfloat x2 -> Tfloat(x1-.x2)
    | Tint x1, Tfloat x2 -> Tfloat(float_of_int x1-.x2)
    | Tfloat x1, Tint x2 -> Tfloat(x1-.float_of_int x2)
    | _, _ -> failwith "jg_minus:type error"
;;

let jg_times left right =
  match left, right with
    | Tint x1, Tint x2 -> Tint(x1*x2)
    | Tfloat x1, Tfloat x2 -> Tfloat(x1*.x2)
    | Tint x1, Tfloat x2 -> Tfloat(float_of_int x1*.x2)
    | Tfloat x1, Tint x2 -> Tfloat(x1*.float_of_int x2)
    | _, _ -> failwith "jg_times:type error"
;;

let jg_power left right =
  let rec power m n a =
    if n <= 0 then a
    else if n mod 2 = 0 then power (m *. m) (n / 2) a
    else power m (n - 1) (m *. a) in
  match left, right with
    | Tint x1, Tint x2 -> Tfloat (power (float_of_int x1) x2 1.0)
    | _, _ -> failwith "jg_powew:type error"
;;

let jg_div left right =
  match left, right with
    | _, Tint 0 -> failwith "jg_div:zero division error"
    | _, Tfloat 0.0 -> failwith "jg_div:zero division error"
    | Tint x1, Tint x2 -> Tint(x1/x2)
    | Tfloat x1, Tfloat x2 -> Tfloat(x1/.x2)
    | Tint x1, Tfloat x2-> Tfloat(float_of_int x1/.x2)
    | Tfloat x1, Tint x2 -> Tfloat(x1/.float_of_int x2)
    | _, _ -> failwith "jg_div:type error"
;;

let jg_mod left right =
  match left, right with
    | _, Tint 0 -> failwith "jg_mod:zero division error"
    | Tint x1, Tint x2 -> Tint(x1 mod x2)
    | _, _ -> failwith "jg_mod:type error"
;;

let jg_and left right =
  Tbool(jg_is_true left && jg_is_true right)
;;

let jg_or left right =
  Tbool(jg_is_true left || jg_is_true right)
;;

let rec jg_eq_eq left right =
  match left, right with
    | Tint x1, Tint x2 -> Tbool(x1=x2)
    | Tfloat x1, Tfloat x2 -> Tbool(x1=x2)
    | Tstr x1, Tstr x2 -> Tbool(x1=x2)
    | Tbool x1, Tbool x2 -> Tbool(x1=x2)
    | Tlist x1, Tlist x2-> jg_list_same left right
    | Tset x1, Tset x2 -> jg_list_same left right
    | Tobj x1, Tobj x2 -> jg_obj_same left right
    | _, _ -> Tbool(false)

and jg_list_same lst1 lst2 =
  let l1 = unbox_list lst1 in
  let l2 = unbox_list lst2 in
  if List.length l1 != List.length l2 then
    Tbool false
  else
    let result = 
      List.for_all2 (fun a b ->
	match jg_eq_eq a b with
	  | Tbool true -> true
	  | _ -> false
      ) l1 l2 in
    Tbool result
      
and jg_obj_same obj1 obj2 =
  let alist1 = unbox_obj obj1 in
  let alist2 = unbox_obj obj2 in
  if List.length alist1 != List.length alist2 then
    Tbool false
  else
    let result = 
      try
	List.for_all (fun (prop, value) ->
	  match jg_eq_eq value (List.assoc prop alist2) with
	    | Tbool true -> true
	    | _ -> false
	) alist1
      with
	  Not_found -> false in
    Tbool result
;;

let jg_not_eq left right =
  match left, right with
    | Tint x1, Tint x2 -> Tbool(x1!=x2)
    | Tfloat x1, Tfloat x2 -> Tbool(x1!=x2)
    | Tstr x1, Tstr x2 -> Tbool(x1<>x2)
    | _, _ -> Tbool(true)
;;

let jg_lt left right = 
  match left, right with
    | Tint x1, Tint x2 -> Tbool(x1<x2)
    | Tfloat x1, Tfloat x2 -> Tbool(x1<x2)
    | _, _ -> failwith "jg_lt:type error"
;;

let jg_gt left right = 
  match left, right with
    | Tint x1, Tint x2 -> Tbool(x1>x2)
    | Tfloat x1, Tfloat x2 -> Tbool(x1>x2)
    | _, _ -> failwith "jg_gt:type error"
;;

let jg_lteq left right = 
  match left, right with
    | Tint x1, Tint x2 -> Tbool(x1<=x2)
    | Tfloat x1, Tfloat x2 -> Tbool(x1<=x2)
    | _, _ -> failwith "jg_lt:type error"
;;

let jg_gteq left right = 
  match left, right with
    | Tint x1, Tint x2 -> Tbool(x1>=x2)
    | Tfloat x1, Tfloat x2 -> Tbool(x1>=x2)
    | _, _ -> failwith "jg_gt:type error"
;;

let jg_inop left right =
  match left, right with
    | value, Tlist lst -> Tbool (List.exists (unbox_bool $ jg_eq_eq value) lst)
    | _ -> Tbool false
;;

let jg_get_kvalue ?(defaults=[]) name kwargs =
  try List.assoc name kwargs with Not_found ->
    (try List.assoc name defaults with Not_found -> Tnull)
;;

let jg_safe value kwargs =
  value
;;

let jg_upper x kwargs =
  match x with
    | Tstr str -> Tstr (String.uppercase str)
    | other -> Tstr (string_of_tvalue other)
;;

let jg_lower x kwargs =
  match x with
    | Tstr str -> Tstr (String.lowercase str)
    | other -> Tstr (string_of_tvalue other)
;;

let jg_int x kwargs =
  match x with
    | Tint x -> Tint x
    | Tfloat x -> Tint (int_of_float  x)
    | _ -> failwith "invalid arg:not number(jg_int)"
;;

let jg_float x kwargs =
  match x with
    | Tfloat x -> Tfloat x
    | Tint x -> Tfloat (float_of_int x)
    | _ -> failwith "invalid arg:not number(jg_float)"
;;

let jg_join join_str lst kwargs =
  match join_str, lst with
    | Tstr str, Tlist lst ->
      Tstr (String.concat str (List.map string_of_tvalue lst))
    | Tstr str, Tset lst ->
      Tstr (String.concat str (List.map string_of_tvalue lst))
    | _ -> failwith "invalid arg:jg_join"
;;

let jg_split pat text kwargs =
  match pat, text with
    | Tstr pat, Tstr text ->
      let lst = 
	Pcre.split ~rex:(Pcre.regexp pat) text +>
	  List.map (fun str -> Tstr str) in
      Tlist lst

  | _ -> failwith "invalid args: split"
;;

let jg_substring base count str kwargs =
  match base, count, str with
    | Tint base, Tint count, Tstr str ->
      Tstr (substring base count str)
    | Tint _, Tint _, Tnull ->
      Tstr ""
    | _ -> failwith "invalid args: substring"
;;

let jg_truncate len str kwargs =
  match len, str with
    | Tint len, Tstr str ->
      Tstr (substring 0 len str)

    | _ -> failwith "invalid args: truncate"
;;

let jg_strlen x kwargs =
  match x with
    | Tstr str -> Tint (Jg_utils.strlen str)
    | _ -> failwith "invalid args: strlen"
;;

let jg_length x kwargs =
  match x with
    | Tlist lst -> Tint (List.length lst)
    | Tset lst -> Tint (List.length lst)
    | Tstr str -> Tint (Jg_utils.strlen str)
    | _ -> failwith "invalid args: not list(length)"
;;

let jg_md5 x kwargs =
  match x with
    | Tstr str ->
      Tstr(str +> String.lowercase +> Digest.string +> Digest.to_hex)
    | _ -> failwith "invalid arg: not string(jg_md5)"
;;

let jg_abs value kwargs =
  match value with
    | Tint x -> Tint (abs x)
    | _ -> failwith "type error: not integer(abs)"
;;

let jg_attr obj prop kwargs =
  match obj, prop with
    | Tobj alist, Tstr prop ->
      (try List.assoc prop alist with Not_found -> Tnull)
    | _ -> Tnull
;;

let jg_batch ?(defaults=[
  ("fill_with", Tnull)
]) count value kwargs =
  match count, value with
    | Tint slice_count, Tlist lst ->
      let fill_value = match jg_get_kvalue "fill_with" kwargs ~defaults with Tnull -> None | other -> Some other in
      let rec batch ret left_count rest =
	if left_count > slice_count then
	  batch ((box_list @@ take slice_count rest) :: ret) (left_count - slice_count) (after slice_count rest)
	else if left_count > 0 then
	  batch ((box_list @@ take slice_count rest ?pad:fill_value) :: ret) 0 []
	else
	  box_list @@ List.rev ret in
      batch [] (List.length lst) lst
    | _ -> failwith "invalid args: batch"
;;

let jg_center ?(defaults=[
  ("width", Tint 80)
]) value kwargs =
  value (* TODO *)
;;

let jg_capitalize value kwargs =
  match value with
    | Tstr str -> Tstr (String.capitalize str)
    | _ -> failwith "invalid args: not string(capitalize)"
;;

let jg_default default value kwargs =
  match value with
    | Tnull -> default
    | other -> other
;;

let jg_dictsort ?(defaults=[
  (* these optional arguments are ignored yet *)
  ("case_sensitive", Tbool true);
  ("by", Tstr "key");
]) value kwargs =
  match value with
    | Tobj alist ->
      Tobj (List.sort (fun a b -> String.compare (fst a) (fst b)) alist)
    | _ -> value
;;

let jg_reverse lst kwargs =
  match lst with
    | Tlist lst -> Tlist (List.rev lst)
    | _ -> failwith "invalid args: not list(jg_reverse)"
;;

let jg_last lst kwargs =
  match lst with
    | Tlist lst -> List.hd (List.rev lst)
    | Tset lst -> List.hd (List.rev lst)
    | _ -> failwith "invalid args: not list(jg_last)"
;;

let jg_random lst kwargs =
  match lst with
    | Tlist lst ->
      let swap a i j =
	let t = a.(i) in
	a.(i) <- a.(j);
	a.(j) <- t in
      let shuffle a =
	Array.iteri (fun i _ -> swap a i (Random.int (i+1))) a in
      let array = Array.of_list lst in
      shuffle array;
      Tlist (Array.to_list array)
    | _ -> failwith "invalid args: not list(jg_random)"
;;

let jg_replace src dst str kwargs =
  match src, dst, str with
    | Tstr src, Tstr dst, Tstr str ->
      Tstr (Pcre.replace ~rex:(Pcre.regexp src ~flags:[`UTF8]) ~templ:dst str)
    | _ -> failwith "invalid arg:not string(jg_replace)"
;;

let jg_sum lst kwargs =
  let rec iter sum lst =
    match sum, lst with
      | Tint sum, (Tint x) :: rest ->
	let sum' = sum + x in
	iter (Tint sum') rest
      | Tint sum, (Tfloat x) :: rest ->
	let sum' = float_of_int sum +. x in
	iter (Tfloat sum') rest
      | Tfloat sum, (Tfloat x) :: rest ->
	let sum' = sum +. x in
	iter (Tfloat sum') rest
      | Tfloat sum, (Tint x) :: rest ->
	let sum' = (float_of_int x) +. sum in
	iter (Tfloat sum') rest
      | _, [] -> sum
      | _ -> failwith "invalid args:non numerical list(jg_sum)" in
  match lst with
    | Tlist lst -> iter (Tint 0) lst
    | _ -> failwith "invalid args: not list(jg_sum)"
;;

let jg_trim str kwargs =
  match str with
    | Tstr str ->
      let head_white = Pcre.regexp "^\\s+" in
      let tail_white = Pcre.regexp "\\s+$" in
      let str = Pcre.replace ~rex:head_white ~templ:"" str in
      let str = Pcre.replace ~rex:tail_white ~templ:"" str in
      Tstr str
    | _ -> failwith "invalid args: not string(jg_trim)"
;;

let jg_list value kwargs =
  match value with
    | Tlist lst -> value
    | Tset lst -> Tlist lst
    | Tstr str ->
      let len = strlen str in
      let rec iter ret i =
	if i >= len then
	  List.rev ret
	else
	  let s1 = Tstr (substring i 1 str) in
	  iter (s1 :: ret) (i+1) in
      Tlist (iter [] 0)
    | _ -> failwith "invalid_arg:can't make sequence(jg_list)"
;;

let jg_slice ?(defaults=[
  ("fill_with", Tnull);
]) len value kwargs =
  jg_batch len (jg_list value []) kwargs
;;

let jg_sublist base count lst kwargs =
  match base, count, lst with
    | Tint base, Tint count, Tlist lst -> Tlist (after base lst +> take count)
    | Tint base, Tnull, Tlist lst -> Tlist (after base lst)
    | _ -> failwith "lnvalid args:jg_sublist"
;;

let jg_wordcount str kwargs =
  match str with
    | Tstr str ->
      Pcre.split ~rex:(Pcre.regexp "[\\s\\t　]+" ~flags:[`UTF8]) str +>
	List.length +> fun count -> Tint count
    | _ -> failwith "invalid arg: not string(jg_word_count)"
;;

let jg_round how value kwargs =
  match how, value with
    | _, Tint x -> Tint x
    | Tstr "floor", Tfloat x -> Tfloat (floor x)
    | Tstr "ceil", Tfloat x -> Tfloat (ceil x)
    | Tstr other, Tfloat x -> failwith @@ spf "invalid args:round method %s not supported(jg_round)" other
    | _ -> failwith "invalid args:jg_round"
;;

let jg_fmt_float digit_count value kwargs =
  match digit_count, value with
    | Tint digit_count, Tfloat value ->
      let fmt = Scanf.format_from_string (spf "%%.%df" digit_count) "%f" in
      Tfloat (float_of_string @@ spf fmt value)
    | _, _ -> failwith "invalid args:fmt_float(digit_count, float_value)"
;;

let jg_range start stop kwargs =
  match start, stop with
    | Tint start, Tint stop ->
      if start = stop then
	Tlist [Tint start]
      else
	let is_end i = if start < stop then i > stop else i < stop in
	let next i = if start < stop then i + 1 else i - 1 in
	let rec iter ret i = if is_end i then List.rev ret else iter ((Tint i) :: ret) (next i) in
	Tlist (iter [] start)
    | _ -> failwith "invalid args: not int(jg_range)"
;;

let jg_urlize text kwargs =
  match text with
    | Tstr text ->
      let reg = Pcre.regexp "((http|ftp|https):\\/\\/[\\w\\-_]+(\\.[\\w\\-_]+)+([\\w\\-\\.,@?^=%&amp;:/~\\+#]*[\\w\\-\\@?^=%&amp;/~\\+#])?)" in
      Tstr (Pcre.replace ~rex:reg ~templ:"<a href='$1'>$1</a>" text)
    | _ -> failwith "invalid arg: not string(jg_urlize)"
;;

let jg_title text kwargs =
  match text with
    | Tstr text ->
      (match Pcre.split ~rex:(Pcre.regexp "[\\s\\t　]+" ~flags:[`UTF8]) text with
	| head :: rest ->
	  ((String.capitalize head) :: rest) +> String.concat " " +> fun text' -> Tstr text'
	| _ -> Tstr text)
    | _ -> failwith "invalid arg: not string(jg_title)"
;;

let jg_striptags text kwargs =
  match text with
    | Tstr text ->
      let reg = Pcre.regexp "<\\/?[^>]+>" ~flags:[`UTF8] in
      let text' = Pcre.replace ~rex:reg ~templ:"" text in
      Tstr text'
    | _ -> failwith "invalid arg: not string(jg_striptags)"
;;

let jg_sort lst kwargs =
  match lst with
    | Tlist lst ->
      (match lst with
	| (Tstr s) :: rest -> List.map unbox_string lst +> List.sort strcmp +> List.map box_string +> box_list
	| (Tint i) :: rest -> List.map unbox_int lst +> List.sort (-) +> List.map box_int +> box_list
	| (Tfloat f) :: rest -> List.map unbox_float lst +> List.sort (fun a b ->
	  if a > b then 1 else if a = b then 0 else -1) +> List.map box_float +> box_list
	| _ -> failwith "invalid_arg:can't sort(jg_sort)")
    | _ -> failwith "invalid_arg:can't sort(jg_sort)"
;;

let jg_xmlattr obj kwargs =
  match obj with
    | Tobj alist ->
      List.map (fun (name, value) -> spf "%s='%s'" name (string_of_tvalue value)) alist +>
	String.concat " " +> box_string
    | _ -> failwith "invalid_arg:not obj(jg_xmlattr)"
;;

let jg_wordwrap width break_long_words text kwargs =
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
      let words = Pcre.split ~rex:(Pcre.regexp "[\\s\\t　]+" ~flags:[`UTF8]) text in
      Tstr (iter "" "" 0 words)
    | _ -> failwith "invalid args:jg_wordwrap"
;;

let jg_test_divisibleby num target kwargs =
  match num, target with
    | Tint 0, _ -> Tbool(false)
    | Tint n, Tint t ->  Tbool(t mod n = 0)
    | _ -> Tbool(false)
;;

let jg_test_even x kwargs =
  match x with
    | Tint x -> Tbool(x mod 2 = 0)
    | _ -> Tbool(false)
;;

let jg_test_odd x kwargs =
  match x with
    | Tint x -> Tbool(x mod 2 = 1)
    | _ -> Tbool(false)
;;

let jg_test_iterable x kwargs =
  match x with
    | Tlist _ -> Tbool(true)
    | Tset _ -> Tbool(true)
    | _ -> Tbool(false)
;;

let jg_test_lower x kwargs =
  match x with
    | Tstr str -> Tbool(Jg_utils.is_lower str)
    | _ -> Tbool(false)
;;

let jg_test_upper x kwargs =
  match x with
    | Tstr str -> Tbool(Jg_utils.is_upper str)
    | _ -> Tbool(false)
;;

let jg_test_number x kwargs =
  match x with
    | Tint _ -> Tbool(true)
    | Tfloat _ -> Tbool(true)
    | _ -> Tbool(false)
;;

let jg_test_sameas value target kwargs =
  match value, target with
    | Tstr x, Tstr y -> Tbool(x == y)
    | Tint x, Tint y -> Tbool(x == y)
    | Tfloat x, Tfloat y -> Tbool(x == y)
    | Tbool x, Tbool y -> Tbool(x == y)
    | Tfun x, Tfun y -> Tbool(x == y)
    | Tobj x, Tobj y -> Tbool(x == y)
    | Tlist x, Tlist y -> Tbool(x == y)
    | Tset x, Tset y -> Tbool(x == y)
    | _ -> Tbool(false)
;;

let jg_test_sequence target kwargs =
  jg_test_iterable target kwargs
;;

let jg_test_string target kwargs =
  jg_strp target
;;

let func_arg0 f = Tfun (fun args kwargs ->
  f kwargs ()
);;

let func_arg1 f = Tfun (fun args kwargs ->
  match args with
    | a1 :: rest -> f a1 kwargs
    | _ -> Tnull
);;

let func_arg2 f = Tfun (fun args kwargs ->
  match args with
    | a1 :: a2 :: rest -> f a1 a2 kwargs
    | a1 :: rest -> func_arg1 (f a1)
    | _ -> Tnull
);;

let func_arg3 f = Tfun (fun args kwargs ->
  match args with
    | a1 :: a2 :: a3 :: rest -> f a1 a2 a3 kwargs
    | a1 :: a2 :: rest -> func_arg1 (f a1 a2)
    | a1 :: rest -> func_arg2 (f a1)
    | _ -> Tnull
);;

let std_filters = [
  (** built-in filters *)
  ("abs", func_arg1 jg_abs);
  ("capitalize", func_arg1 jg_capitalize);
  ("escape", func_arg1 jg_escape_html);
  ("e", func_arg1 jg_escape_html); (* alias for escape *)
  ("float", func_arg1 jg_float);
  ("int", func_arg1 jg_int);
  ("last", func_arg1 jg_last);
  ("length", func_arg1 jg_length);
  ("list", func_arg1 jg_list);
  ("lower", func_arg1 jg_lower);
  ("md5", func_arg1 jg_md5);
  ("safe", func_arg1 jg_safe);
  ("strlen", func_arg1 jg_strlen);
  ("sum", func_arg1 jg_sum);
  ("striptags", func_arg1 jg_striptags);
  ("sort", func_arg1 jg_sort);
  ("upper", func_arg1 jg_upper);
  ("random", func_arg1 jg_random);
  ("reverse", func_arg1 jg_reverse);
  ("title", func_arg1 jg_title);
  ("trim", func_arg1 jg_trim);
  ("urlize", func_arg1 jg_urlize);
  ("wordcount", func_arg1 jg_wordcount);
  ("xmlattr", func_arg1 jg_xmlattr);

  ("attr", func_arg2 jg_attr);
  ("batch", func_arg2 jg_batch);
  ("default", func_arg2 jg_default);
  ("d", func_arg2 jg_default); (* alias for default *)
  ("fmt_float", func_arg2 jg_fmt_float);
  ("join", func_arg2 jg_join);
  ("split", func_arg2 jg_split);
  ("slice", func_arg2 jg_slice);
  ("truncate", func_arg2 jg_truncate);
  ("range", func_arg2 jg_range);
  ("round", func_arg2 jg_round);

  ("replace", func_arg3 jg_replace);
  ("substring", func_arg3 jg_substring);
  ("sublist", func_arg3 jg_sublist);
  ("wordwrap", func_arg3 jg_wordwrap);

  (** built-in tests *)
  ("divisibleby", func_arg2 jg_test_divisibleby);
  ("even", func_arg1 jg_test_even);
  ("iterable", func_arg1 jg_test_iterable);
  ("number", func_arg1 jg_test_number);
  ("odd", func_arg1 jg_test_odd);
  ("sameas", func_arg2 jg_test_sameas);
  ("sequence", func_arg1 jg_test_sequence);
  ("string", func_arg1 jg_test_string);
]
;;

let jg_load_extensions extensions =
  List.iter (fun ext ->
    try
      Dynlink.loadfile ext
    with
	Dynlink.Error e -> failwith @@ Dynlink.error_message e
  ) extensions
;;

let jg_post_process text =
  text
  +> Pcre.qreplace ~rex:(Pcre.regexp "[\\s]*{%<%}") ~templ:""
  +> Pcre.qreplace ~rex:(Pcre.regexp "{%>%}[\\s]*") ~templ:""

let jg_init_context ?(models=[]) env =
  let model_frame = Hashtbl.create (2 * List.length models) in
  let top_frame = Hashtbl.create (List.length std_filters + List.length env.filters + 2) in
  let rec set_values hash alist = List.fold_left (fun h (n, v) -> Hashtbl.add h n v; h) hash alist in
  ignore @@ set_values model_frame models;
  ignore @@ set_values top_frame std_filters;
  ignore @@ set_values top_frame env.filters;
  ignore @@ set_values top_frame [
    ("jg_is_compiled", Tbool env.compiled);
    ("jg_is_autoescape", Tbool env.autoescape);
  ];
  { frame_stack = [model_frame; top_frame];
    macro_table = Hashtbl.create 10;
    active_filters = [];
    buffer = Buffer.create 1024;
  }
;;
