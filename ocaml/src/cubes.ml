open Core

type bits = bool array
module G = Graph.Sigma

let bit_is_set k n = (1 lsl k) land n <> 0

(* The first b bits of n *)
let bits b n = Array.init b ~f:(fun i -> bit_is_set i n)

(* Set the kth bit of n *)
let set_bit k n = n lor (1 lsl k)

let two_to_the n = 1 lsl n

let flip_bit k n =
  if bit_is_set k n
  then n land (lnot (1 lsl k))
  else n lor (1 lsl k)

let from_bits bits =
  let b = Array.length bits in
  let rec loop i n =
    if i = b then n
    else
      loop (i + 1) (if bits.(i) then set_bit i n else n)
  in
  loop 0 0

let iter_range start stop ~f =
  let rec loop i =
    if i > stop then ()
    else (f i; loop (i + 1))
  in
  loop start

let cube_graph d =
  let g     = G.create () in
  let nodes = Array.init (two_to_the d) ~f:(fun i -> print i; G.add_node ~color:Color.black
    ~pos:(Random.float 1., Random.float 1.) ~size:1. g i) in
  print nodes;
  let () =
    iter_range 0 (two_to_the d - 1) ~f:(fun node_i ->
      print node_i;
      let node = nodes.(node_i) in
      iter_range 0 (d - 1) ~f:(fun b ->
        println (Printf.sprintf "b: %d" b);
        G.add_arc_exn g () ~src:node ~dst:(nodes.(flip_bit b node_i)) ~color:Color.black))
  in
  g
    

type face =
  { label : int
  ; stars : (int * int)
  }

type cube =
  { label : int
  ; stars : (int * int * int)
  }

let choose2 n = n * (n - 1) / 2

(* Random boolean which is true with probability p *)
let rand_bool p = if Random.float 1. < p then true else false

let iter_cubes d ~f =
  for star1 = 0 to (d - 1) do
    for star2 = star1 + 1 to (d - 1) do
      for star3 = star2 + 1 to (d - 1) do
        for label = 0 to (two_to_the (d - 3) - 1) do
          f {label; stars = (star1, star2, star3)}
        done
      done
    done
  done

(* shift the portion of n starting at (including) b by 1 bit *)
let all_filled = lnot 0
let shift_from b n =
  let lower_mask = two_to_the b - 1 in
  let upper_mask = lnot lower_mask in
  ((upper_mask land n) lsl 1) lor (lower_mask land n)


let faces_of_cube {label; stars = (star1, star2, star3)} : face array =
  [| {label = shift_from star1 label; stars = (star2, star3)}
  ;  {label = set_bit star1 (shift_from star1 label); stars = (star2, star3)}
  ;  {label = shift_from (star2 - 1) label; stars = (star1, star3)}
  ;  {label = set_bit (star2 - 1) (shift_from (star2 - 1) label); stars = (star1, star3)}
  ;  {label = shift_from (star3 - 2) label; stars = (star1, star2) }
  ;  {label = set_bit (star3 - 2) (shift_from (star3 - 2) label); stars = (star1, star2) }
  |]

let iteri_pairs arr ~f =
  let n = Array.length arr in
  for i = 0 to n - 1 do
    for j = i + 1 to n - 1 do
      f (i, arr.(i)) (j, arr.(j))
    done
  done

let foralli arr ~f =
  let n = Array.length arr in
  let rec loop i =
    if i = n
    then true
    else if f i arr.(i)
    then loop (i + 1)
    else false
  in
  loop 0

let face_graph d p =
  let g = G.create () in
  let tbl = Hashtbl.create (int_of_float (p *. float_of_int (two_to_the (d - 2) * choose2 d))) in
  for star1 = 0 to (d - 1) do
    for star2 = (star1 + 1) to (d - 1) do
      for label = 0 to (two_to_the (d - 2) - 1) do
        let node = G.add_node ~color:Color.black ~pos:(Random.float 1., Random.float 1.) ~size:1. g () in
        Hashtbl.add tbl ({label; stars = (star1, star2)} : face)
          (rand_bool p, node)
      done
    done
  done;
  iter_cubes d ~f:(fun cube ->
    let fs = faces_of_cube cube in
    iteri_pairs fs ~f:(fun (i, f_i) (j, f_j) ->
      if foralli fs ~f:(fun k f_k -> k = i || k = j || fst (Hashtbl.find tbl f_k))
      then G.add_arc_exn g ()
        ~shape:(`Straight, `Arrow)
        ~color:Color.black
        ~src:(snd (Hashtbl.find tbl f_i))
        ~dst:(snd (Hashtbl.find tbl f_j))));
  g

let get_element_by_id s =
  match Js.Opt.to_option (Dom_html.document##getElementById(Js.string s)) with
  | None -> failwith "get_element_by_id: Not found" | Some x -> x

let () = println "hi"

(*
let () = begin
  print (Js.array (Array.init 100 ~f:(fun _ -> Js.bool (G.weakly_connected (face_graph 7 0.82)))))
end

*)

let isolated_vertices (g : ('a, 'b) G.t) : int =
  Js.Unsafe.(fun_call (variable "isolatedNodes") [|inject g|])

let g = face_graph 4 0.82

let counts arr =
  let cs = Inttbl.create () in
  Array.iter arr ~f:(fun n -> Inttbl.add cs
    ~key:n ~data:(1 + Option.value ~default:0 (Inttbl.find cs n)));
  cs

(*
let () =
  print (counts (Array.init 300 ~f:(fun _ -> isolated_vertices (face_graph 4 0.7))))

*)
let () = begin
  print (Js.bool (G.weakly_connected g));
  G.display g (get_element_by_id "container");
  G.start_force_layout g;
  G.make_draggable g;

  set_global "g" g
end

