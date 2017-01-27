module G = Graf.StringGraph
open G

(**  En testgraf: (Sida 617, fig 12.41 i Grimaldi) *)
let gr1 =
  let a = "A" and b = "B" and c = "C"
  and d = "D" and e = "E" and f = "F"
  and g = "G" and h = "H" and i = "I"
  and j = "J"
  in
  G.of_list [a,b; a,c; a, d; a, e; a,f; a,i; c,e; f,d; d,g; d,h; g, h; h, j]
  |> undigraf

let _ = edges gr1 (* Evaluera i en toplevel *)

open Printf (* För printf, sprintf, etc.  *)

let print_list l =
  printf "\n[" ; 
  List.iteri (fun i s -> printf "%s;%s " s (if ((i+1) mod 10) = 0 then "\n" else "")) l;
  printf "]\n"

let print_edges g = 
  edges g
  |> List.map (fun (u,v) -> sprintf "(%s,%s)" u v)
  |> print_list

let _ = printf "\ngr1 = "; print_edges gr1

(** En rutin för DFS i en riktad graf. *)
let dfs graf rot =
  let rec build t s =
    match s with
    | [] -> t
    | e :: s ->
      let v = trg e in
      if isvertex v t
      then (* Backedge *)
        build t s
      else (* Unvisited vertex *)
        let t' = add_leaf e t
        and s' = outedges graf v
        in
        build t' (s' @ s) 
  in
  build (empty_tree rot) (outedges graf rot)

let tree1 = dfs gr1 "A"

let _ = printf "tree1 = "; print_edges tree1

(* skriv ut ett träd i preorder *)
let print_pre t v =
  let pr a v = (Printf.sprintf "%s; " v) :: a in
  preorder_fold t v pr [] |> List.rev |> print_list

let print_post t v =
  let pr a v = (Printf.sprintf "%s; " v) :: a in
  postorder_fold t v pr [] |> List.rev |> print_list

(** Skriv ut trädet (hörnet "A" är rot) i preordning *)
let _ = print_string "print_pre tree1 A = "; print_pre tree1 "A"
(** Skriv ut underträdet till hörnet "D" i postordning *)
let _ = print_string "print_post tree1 D = "; print_pre tree1 "D"

(* Definierar djupet som en map [vertex -> int]. *)
let set_depth t rot =
  let ddepth map v = match parent t v with
    | None -> Vmap.add v 0 map
    | Some u -> Vmap.add v (Vmap.find u map + 1) map
    (* Använder att [parent t v] redan har givits ett djup i preorder *)
  in
  preorder_fold t rot ddepth Vmap.empty

let dmap = set_depth tree1 "A"

let _ = Vmap.bindings dmap
        |> List.map (fun (u,d) -> sprintf "(%s,%d)" u d)
        |> print_list

let _ = Vmap.find "J" dmap |> print_int; print_string "\n"