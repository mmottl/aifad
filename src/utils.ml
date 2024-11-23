(* AIFAD - Automated Induction of Functions over Algebraic Datatypes

   Copyright © 2002 Austrian Research Institute for Artificial Intelligence
   Copyright © 2003- Markus Mottl <markus.mottl@gmail.com>

   This library is free software; you can redistribute it and/or modify it under
   the terms of the GNU Lesser General Public License as published by the Free
   Software Foundation; either version 2.1 of the License, or (at your option)
   any later version.

   This library is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
   details.

   You should have received a copy of the GNU Lesser General Public License
   along with this library; if not, write to the Free Software Foundation, Inc.,
   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA *)

(* Integer sets and maps *)

module IntSet = Set.Make (struct
  type t = int

  let compare = compare
end)

module IntMap = Map.Make (struct
  type t = int

  let compare = compare
end)

(* Option functions *)

let unlift_opt = function Some x -> x | None -> failwith "unlift_opt"
let unlift_opts opts = Array.map unlift_opt opts

(* Either *)

type ('a, 'b) either = Left of 'a | Right of 'b

(* One_list *)

type 'a one_list = OneEl of 'a | OneCons of 'a * 'a one_list

let rec one_list_rev_loop acc = function
  | OneCons (el, one_lst) -> one_list_rev_loop (OneCons (el, acc)) one_lst
  | OneEl el -> OneCons (el, acc)

let one_list_rev = function
  | OneCons (el, one_lst) -> one_list_rev_loop (OneEl el) one_lst
  | OneEl _ as one_lst -> one_lst

let rec one_list_fold_left f acc = function
  | OneEl el -> f acc el
  | OneCons (el, one_lst) -> one_list_fold_left f (f acc el) one_lst

(* List functions *)

let rec list_fold_lefti_loop f ix acc = function
  | [] -> acc
  | h :: t -> list_fold_lefti_loop f (ix + 1) (f ix acc h) t

let list_fold_lefti f acc lst = list_fold_lefti_loop f 0 acc lst

let list_to_rev_array n = function
  | [] -> [||]
  | h :: t ->
      let ar = Array.make n h in
      let rec loop ix = function
        | h :: t ->
            Array.unsafe_set ar ix h;
            if ix >= 0 then loop (ix - 1) t else ar
        | [] -> assert false
      in
      loop (n - 1) t

(* Array functions *)

let array_is_empty ar = Array.length ar = 0

let rec array_of_nlist_loop ar lix ix lst =
  if ix > lix then ar
  else
    match lst with
    | [] -> failwith "array_of_nlist_loop: list too short"
    | h :: t ->
        Array.unsafe_set ar ix h;
        array_of_nlist_loop ar lix (ix + 1) t

let array_of_nlist n = function
  | [] -> [||]
  | h :: t -> array_of_nlist_loop (Array.make n h) (n - 1) 1 t

let rec array_map_of_nlist_loop f ar lix ix lst =
  if ix > lix then ar
  else
    match lst with
    | [] -> failwith "array_map_of_nlist_loop: list too short"
    | h :: t ->
        Array.unsafe_set ar ix (f h);
        array_map_of_nlist_loop f ar lix (ix + 1) t

let array_map_of_nlist f n = function
  | [] -> [||]
  | h :: t -> array_map_of_nlist_loop f (Array.make n (f h)) (n - 1) 1 t

let rec array_of_rev_list_loop ar ix = function
  | [] -> ar
  | h :: t ->
      Array.unsafe_set ar ix h;
      array_of_rev_list_loop ar (ix - 1) t

let array_of_rev_list = function
  | [] -> [||]
  | h :: t ->
      let len = List.length t in
      array_of_rev_list_loop (Array.make (len + 1) h) (len - 1) t

let array_of_nrev_list n = function
  | [] -> [||]
  | h :: t -> array_of_rev_list_loop (Array.make n h) (n - 1) t

let rec array_map_of_rev_list_loop f ar ix = function
  | [] -> ar
  | h :: t ->
      Array.unsafe_set ar ix (f h);
      array_map_of_rev_list_loop f ar (ix - 1) t

let array_map_of_rev_list f = function
  | [] -> [||]
  | h :: t ->
      let len = List.length t in
      array_map_of_rev_list_loop f (Array.make (len + 1) (f h)) (len - 1) t

let lcons lst el = el :: lst
let array_to_rev_list ar = Array.fold_left lcons [] ar

let array_map_to_rev_list f ar =
  let coll lst el = f el :: lst in
  Array.fold_left coll [] ar

let array_rev_fold_left f acc ar =
  let acc_ref = ref acc in
  for i = Array.length ar - 1 downto 0 do
    acc_ref := f !acc_ref (Array.unsafe_get ar i)
  done;
  !acc_ref

let array_map_to_list f ar =
  let coll lst el = f el :: lst in
  array_rev_fold_left coll [] ar

let array_rev_iter f ar =
  for i = Array.length ar - 1 downto 0 do
    f (Array.unsafe_get ar i)
  done

let array_map2 f ar =
  let len = Array.length ar in
  if len = 0 then ([||], [||])
  else
    let a, b = f (Array.unsafe_get ar 0) in
    let res1 = Array.make len a in
    let res2 = Array.make len b in
    for i = 1 to len - 1 do
      let a, b = f (Array.unsafe_get ar i) in
      Array.unsafe_set res1 i a;
      Array.unsafe_set res2 i b
    done;
    (res1, res2)

let array_mapi2 f ar =
  let len = Array.length ar in
  if len = 0 then ([||], [||])
  else
    let a, b = f 0 (Array.unsafe_get ar 0) in
    let res1 = Array.make len a in
    let res2 = Array.make len b in
    for i = 1 to len - 1 do
      let a, b = f i (Array.unsafe_get ar i) in
      Array.unsafe_set res1 i a;
      Array.unsafe_set res2 i b
    done;
    (res1, res2)

let rec array_forall_loop ix len p ar =
  ix = len || (p (Array.unsafe_get ar ix) && array_forall_loop (ix + 1) len p ar)

let array_forall p ar = array_forall_loop 0 (Array.length ar) p ar

let array_filter p ar =
  let els_ref = ref [] in
  let cnt_ref = ref 0 in
  for i = 0 to Array.length ar - 1 do
    let el = Array.unsafe_get ar i in
    if p el then (
      incr cnt_ref;
      els_ref := el :: !els_ref)
  done;
  let cnt = !cnt_ref in
  if cnt = 0 then [||]
  else
    match !els_ref with
    | h :: t ->
        let res = Array.make cnt h in
        array_of_rev_list_loop res (cnt - 1) t
    | [] -> assert false

let array_iter1 f ar =
  for i = 1 to Array.length ar - 1 do
    f (Array.unsafe_get ar i)
  done

let rec array_fold_left_loop ix len f acc ar =
  if ix = len then acc
  else
    let new_acc = f acc (Array.unsafe_get ar ix) in
    array_fold_left_loop (ix + 1) len f new_acc ar

let array_fold_left1 f acc ar =
  array_fold_left_loop 1 (Array.length ar) f acc ar

let rec array_fold_lefti_loop ix len f acc ar =
  if ix = len then acc
  else
    let new_acc = f ix acc (Array.unsafe_get ar ix) in
    array_fold_lefti_loop (ix + 1) len f new_acc ar

let array_fold_lefti f acc ar =
  array_fold_lefti_loop 0 (Array.length ar) f acc ar

let array_fold_left1i f acc ar =
  array_fold_lefti_loop 1 (Array.length ar) f acc ar

let rec array_fold_righti_loop ix f ar acc =
  if ix < 0 then acc
  else
    let new_acc = f ix (Array.unsafe_get ar ix) acc in
    array_fold_righti_loop (ix - 1) f ar new_acc

let array_fold_righti f ar acc =
  array_fold_righti_loop (Array.length ar - 1) f ar acc

let rec array_find_index_loop ix p ar len =
  if ix = len then raise Not_found
  else if p (Array.unsafe_get ar ix) then ix
  else array_find_index_loop (ix + 1) p ar len

let array_find_index p ar = array_find_index_loop 0 p ar (Array.length ar)

let transpose_matrix m =
  let z = Array.length m in
  if z = 0 then [||]
  else
    let m0 = Array.unsafe_get m 0 in
    let s = Array.length m0 in
    if s = 0 then Array.make s [||]
    else
      let m00 = Array.unsafe_get m0 0 in
      let new_m = Array.make_matrix s z m00 in
      let z_1 = z - 1 in
      for i = 0 to s - 1 do
        let ar = Array.unsafe_get new_m i in
        for j = 0 to z_1 do
          Array.unsafe_set ar j (Array.unsafe_get (Array.unsafe_get m j) i)
        done
      done;
      new_m

(* Miscellaneous functions *)

let id x = x
let cmp_fst (x1, _) (x2, _) = compare x1 x2
let cmp_snd (_, y1) (_, y2) = compare y1 y2
let make_pair a b = (a, b)

let unwind_protect f fx f_exit fx_exit =
  let res =
    try f fx
    with exc ->
      f_exit fx_exit;
      raise exc
  in
  f_exit fx_exit;
  res

let do_open_in name f =
  let ic = open_in name in
  unwind_protect f ic close_in ic

let do_open_out name f =
  let oc = open_out name in
  unwind_protect f oc close_out oc
