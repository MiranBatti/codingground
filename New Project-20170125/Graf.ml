(** Den här modulen implementerar signaturer för graf moduler erhållna med
    [Simplegraph(V)]. Enda skillnaden är typen av hörn. *)
module type SG = 
sig
  (** Typen av hörn *)
  type vertex

  (** Kanter är omärkta och endast ett par av hörn *)
  type edge = vertex * vertex

  (** Starthörn *)
  val src : edge -> vertex

  (** Sluthörn  *)
  val trg : edge -> vertex

  val edge : vertex * vertex -> edge

  (** [op e] ger den motsatta kanten *)
  val op : edge -> edge

  (** {2 Hörn och kant attribut} Hörn och kant attribut representaras med
      standardbibliotekets "funktoriella" Map-modul. *)

  (** En "map" för att lagra attribut kopplade till hörn/kanter. Anropet
      [vmap f] ger en map där [f: vertex -> 'a] är det värde som tilldelas som
      default till alla hörn i grafen. *)
  module Vset : Set.S with type elt = vertex
  module Eset : Set.S with type elt = edge 
  (** En "vertex-map" för att lagra attribut kopplade till kanter. En inital funktion *)
  module Vmap : Map.S with type key = vertex
  (** En "edge-map" för att lagra attribut kopplade till kanter. En inital funktion  *)
  module Emap : Map.S with type key = edge 

  (** {2  Typen för grafer}  *)

  type graf
  type t = graf

  (** Den tomma grafen: Utgångspunkt vid konstruktioner  *)
  val empty : graf

  (** [isvertex v g] är sant om [v] är ett hörn i [g] *)
  val isvertex : vertex -> graf -> bool

  (** [isedge e g] är sant om [e] är en kant i [g] *)
  val isedge : edge -> graf -> bool

  (** {2 Konstruktion av grafer} *)

  (** [add v g] ger grafen g med hörnet v adderat *)
  val addv : vertex -> graf -> graf

  (** [adde e g] adderar kanten e till grafen. Eventuella nya hörn adderas
      också. *)
  val adde : edge -> graf -> graf

  (** Konstruera en graf ur en lista *)
  val of_list : edge list -> graf


  (** {2 Incidenser och adjacenser som listor} *)

  (** [outedges g v] ger en lista av ut-kanter till hörnet *)
  val outedges : graf -> vertex -> edge list

  (** [inedges g v] ger en lista av in-kanter till hörnet *)
  val inedges : graf -> vertex -> edge list

  (** [outnbrs g v] ger en lista av ut-grannar till hörnet *)
  val outnbrs : graf -> vertex -> vertex list

  (** [innbrs g v] ger en lista av in-grannar till hörnet *)
  val innbrs : graf -> vertex -> vertex list

  (** Mängden av hörn som lista *)
  val vertices : graf -> vertex list

  (** Mängden av kanter som lista *)
  val edges : graf -> edge list


  (** {2 Några grafoperationer} *)

  (** [union g h] returnerar unionsgrafen *)
  val union : graf -> graf -> graf

  (** [subgraph vp ep g] returnerar den delgraf där både hörnpredikatet vp och
      kantpredikatet ep är uppfyllda *)
  val subgraph : (vertex->bool) -> (edge->bool) -> graf -> graf

  (** Dubblerar alla kanter (u,v) till paret (u,v) och (v,u). På så sätt kan en
      enkel oriktad graf representeras som en enkel riktad graf. *)
  val undigraf : graf -> graf

  (** {1 Rotade träd}

      Rutiner för rotade träd. Rotade träd kan konstrueras rekursivt från en tom
      enpunktsgraf med rutinen [add_leaf v t].

      Trädet representeras som ett graf [t : graf] där alla kanterna i trädet
      [t] riktas från roten [v]. Roten måste hålla reda på separat *)
  type tree = graf

  val empty_tree : vertex -> tree
  (** Det tomma trädet *)

  val add_leaf : edge -> tree -> tree
  (** Lägg till ett löv *)

  val newtree : vertex -> (vertex*tree) list -> tree
  (** Bygg från roten med rotade delträd som barn *)

  val parent   : tree -> vertex -> vertex option
  (** [parent t v] returnerar [Some u] där [u] är förälder i trädgrafen. [None]
      om [v] är rot *)

  val children   : tree -> vertex -> vertex list
  (** Ut-grannar trädgrafen *)

  (** [isancestor u v t] anger sant om u ligger ovanför v i trädet *)
  (*  val isancestor : vertex -> vertex -> tree -> bool *)
  val isancestor : vertex -> vertex -> tree -> bool

  (** {2 Olika aggregeringsrutiner (fold-rutiner)} *)

  val fold_children : tree -> vertex -> ('a -> vertex -> 'a) -> 'a -> 'a
  (** [fold_children t v f a] som [List.fold_left f a (children t v)] *)
  val postorder_fold: tree -> vertex -> ('a -> vertex -> 'a) -> 'a -> 'a
  (** [postorder_fold t rot f a] går rekursivt igenom underträden till [t] under
      hörnet [rot] före applikation av f a rot. *)
  val preorder_fold : tree -> vertex -> ('a -> vertex -> 'a) -> 'a -> 'a
  (** [preorder_fold t rot f a] går rekursivt igenom underträden till [t] under
      hörnet [rot] efter applikation av f a rot. *)
end

(** Module Simplegraph är en funktor *)
module Simplegraph(V:sig type t val compare: t -> t -> int end) :
  SG with type vertex = V.t =
struct
  type vertex = V.t
  type edge = vertex * vertex


  let src (e : edge) = fst e
  let trg (e : edge) = snd e

  let edge (u,v) = (u,v)
  let op e = edge (trg e, src e)

  module Vertex = V
  module Edge   =
  struct
    type t = edge
    let compare (u,v) (u',v') =
      let a = Vertex.compare u u' in 
      if a = 0 then Vertex.compare v v' else a
  end

  module Vset : Set.S with type elt = vertex = Set.Make(Vertex)
  module Eset : Set.S with type elt = edge   = Set.Make(Edge)

  module Vmap : Map.S with type key = vertex = Map.Make(Vertex)
  module Emap : Map.S with type key = edge   = Map.Make(Edge)

  type vset = Vset.t
  type eset = Eset.t

  type 'a vmap = 'a Vmap.t
  type 'a emap = 'a Emap.t

  (** Vi använder en enkel typ av graf-representation: Hörnen i grafen är av
      generisk typ ['a]. Kanter är par av hörn. Detta är tillräckligt för att
      representera en enkel riktad graf. Dessutom  *)
  type graf = {
    vertices: vset; 
    edges   : eset;
    outedges  : eset vmap; 
    inedges  : eset vmap 
  }
  type t = graf

  (** Den tomma grafen *)
  let empty = {
    vertices = Vset.empty;
    edges    = Eset.empty;
    outedges = Vmap.empty;
    inedges  = Vmap.empty
  }

  let isedge   e g = Eset.mem e g.edges
  let isvertex v g = Vset.mem v g.vertices

  let outedges g v = try Vmap.find v g.outedges |> Eset.elements with Not_found -> []
  let inedges g v  = try Vmap.find v g.inedges  |> Eset.elements with Not_found -> []
  let outnbrs g v  = outedges g v |> List.map trg
  let innbrs g v   = inedges  g v |> List.map src

  let edges    g = Eset.elements g.edges
  let vertices g = Vset.elements g.vertices

  let vmget m v = try Vmap.find v m with Not_found -> Eset.empty

  let addv v g =
    if isvertex v g then g else
      { g with  
        vertices = Vset.add v g.vertices;
        outedges = Vmap.add v Eset.empty g.outedges;
        inedges =  Vmap.add v Eset.empty g.inedges;
      }
  let adde e g =
    if Eset.mem e g.edges then
      g
    else
      let g' = addv (src e) g |> addv (trg e)
      and add v e m = Vmap.add v (Eset.add e (vmget m v)) m
      in { g' with
           edges    = Eset.add e  g'.edges;
           outedges = add (src e) e g'.outedges;
           inedges  = add (trg e) e g'.inedges;
         }

  let of_list edgelist =
    List.fold_left (fun g e -> adde e g) empty edgelist 

  let fixgraph g = Vset.fold addv g.vertices empty |> Eset.fold adde g.edges

  let union g h =
    { empty with
      vertices = Vset.union g.vertices h.vertices;
      edges    = Eset.union g.edges  h.edges;
    } |> fixgraph

  let subgraph vp ep g =
    { empty with
      vertices = Vset.filter  vp g.vertices;
      edges    = Eset.filter (fun e -> ep e && vp (trg e) && vp (src e)) g.edges;
    } |> fixgraph

  (** Dubblerar alla kanter [(u,v)]. På så sätt kan en enkel oriktad graf
      representeras som en enkel riktad graf. *)
  let undigraf g =
    let double e g = adde e g |> adde (op e) in
    Eset.fold double g.edges empty

  type tree = graf

  let empty_tree v = addv v empty (* Ett tomt rotat träd på ett hörn *)

  let add_leaf e t =
    if isvertex (src e) t && not (isvertex (trg e) t)
    then adde e t
    else t

  (** Bygger upp ett träd från roten med rotade delträd *)
  let newtree (rot:vertex) (treelist: (vertex*tree) list)  =
    let cjoin t (r',t')  = union t' t |> adde (rot,r') in
    List.fold_left cjoin (empty |> addv rot) treelist


  let children t v = outnbrs t v

  let parent t v =
    match innbrs t v with
    | [x] -> Some x (* At most one parent *)
    | _ -> None

  let fold_children t v f a =
    List.fold_left f a (children t v)
    
    let rec isancestor u v t = 
        let rec isAncestorChildren u cl =
            match cl with
            | [] -> false
            | v :: rest when isancestor u v t -> true
            | v :: rest -> isAncestorChildren u cl in
        let isParent p c =
            match parent c t with
                | Some parent -> p = parent
                | None -> false in
        isParent u v || isAncestorChildren u (children t v)
        
        (*if isParent u v then true
        else isAncestorChildren u (children t v)
        *)

  let rec postorder_fold t v f a =
    let a' = fold_children t v (fun a v' -> postorder_fold t v' f a) a
    in
    f a' v

  let rec preorder_fold t v f a  =
    let a' = f a v
    in
    fold_children t v (fun a v' -> preorder_fold t v' f a) a'
end 

(** Grafmodul med hörn som [string] *)
module StringGraph : SG with type vertex = string =
  Simplegraph(struct type t = string let compare = compare end)

(** Grafmodul med hörn som [int] *)
module IntGraph : SG with type vertex = int  =
  Simplegraph(struct type t = int let compare = compare end)
