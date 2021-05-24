open Core_kernel
open Lacaml.D

let pwd = Sys.getcwd ()

let cd = Sys.chdir

let pi = Stdlib.acos (-1.)

let (  *||  ) b c = Array.map2_exn b c ~f:(fun x y -> x *. y) 

let (  +||  ) b c = Array.map2_exn b c ~f:(fun x y -> x +. y) 

let (  -||  ) b c = Array.map2_exn b c ~f:(fun x y -> x -. y) 

let (  /||  ) b c = Array.map2_exn b c ~f:(fun x y -> x /. y) 

let (  *.|  ) b c = Array.map c ~f:(fun y -> b *. y) 

let (  **|.  ) b c = Array.map b ~f:(fun y -> y ** c ) 

let (  +.|  ) b c = Array.map c ~f:(fun y -> b +. y) 

let (  -.|  ) b c = Array.map c ~f:(fun y -> b -. y) 

let (  /.|  ) b c = Array.map c ~f:(fun y -> b /. y) 

let arrayexp c = Array.map c ~f:(fun y -> exp y ) 

let doublearrayadd b c = Array.map2_exn b c ~f:(fun x y -> x +|| y) 

let (  +||||  ) b c = Array.map2_exn b c ~f:(fun x y -> x +|| y) 

let doublearraymultiply b c = Array.map c ~f:(fun y -> b *.| y) 

let (  *.||  ) b c = Array.map c ~f:(fun y -> b *.| y) 

let doublearraysubtract b c = doublearrayadd b ( doublearraymultiply ( -1.0 ) c )

let (  -||||  ) b c = doublearrayadd b ( doublearraymultiply ( -1.0 ) c )

let arrayunroll data points =
  let length = Array.length data in
  Array.init (length + 2 * points) ~f:(fun i -> data.( (i-points+length) mod length ) )

let first_derivative_4th_order ~valuearray ~delta = 
  let h = arrayunroll valuearray 2 in
  Array.init ( Array.length valuearray ) ~f:( fun i -> ( 8.0 *. ( h.(i+3) -. h.(i+1) ) +. ( h.(i) -. h.(i+4) ) ) /. ( 12.0 *. delta ) )

let second_derivative_4th_order ~valuearray ~delta = 
  let h = arrayunroll valuearray 2 in
  Array.init ( Array.length valuearray ) ~f:( fun i -> ( ( -30.0 *. h.(i+2) +. 16.0 *. ( h.(i+1) +. h.(i+3) ) -. ( h.(i) +. h.(i+4) ) ) ) /. ( 12.0 *. delta *. delta ) )

(*

This returns the time in seconds since the epoch.

*)

let timenow x = Time.to_span_since_epoch ( Time.now x ) |> Time.Span.to_sec

type time_frame = { time : float ; data : float array array }

let (  +>>  ) b c = 
  { 
    time = b.time +. c.time ;
    data = b.data +|||| c.data
  }

let (  *.>  ) b c = 
  { 
    time = b *. c.time ;
    data = b *.|| c.data
  }

let (  ->>  ) b c = b +>> ( (-1.0) *.> c ) 

let changeframe2 frame deltat deltax =
  let exp c = Stdlib.exp c in
  let d0 = frame.data in
  let n = Array.length ( d0.(0) ) in
  let t = frame.time in

  let twist_squared = 1.0 in

  let d10 = first_derivative_4th_order d0.(0) deltax  in 
  let d12 = first_derivative_4th_order d0.(2) deltax  in 
  let d15 = first_derivative_4th_order d0.(5) deltax  in 
  let d20 = second_derivative_4th_order d0.(0) deltax in 
  let d22 = second_derivative_4th_order d0.(2) deltax in 
  
  let p   i = d0.(0).(i) in
  let pip i = d0.(1).(i) in
  let piq i = d0.(3).(i) in
  let l   i = d0.(4).(i) in
  let pil i = d0.(5).(i) in

  let dt0 i =   -1.0 *. ( pip i ) /. ( 2.0 *. ( pil i ) )  in
  let dt1 i =   -1.0 /. ( 2.0 *. ( pil i ) ) *.	(
                                                          exp( -2.0 *. ( p i ) ) *. ( piq i ) *. ( piq i )
                                                          +. exp( 2.0 *. t ) *. d20.(i)
                                                          -. exp( 2.0 *. t ) *. d10.(i) *. d15.(i) /. ( pil i )
                                                          -. exp( 2.0 *. ( ( p i ) +. t ) ) *. d12.(i) *. d12.(i)
							)
							+.  ( pil i ) *. twist_squared *. exp( ( ( l i ) +. 2.0 *. ( p i ) -. 3.0 *. t ) /. 2.0 )
  in
  let dt2 i =    -1.0 *. exp( -2.0 *. ( p i ) ) *. ( piq i ) /. ( 2.0 *. ( pil i ) )
  in
  let dt3 i =    -1.0 *. exp( 2.0 *. ( ( p i ) +. t ) ) /. ( 2.0 *. ( pil i ) ) *. (
                                                                                         d22.(i)
                                                                                         -. ( d12.(i) *. d15.(i) /. ( pil i ) )
                                                                                         +. 2.0 *. d10.(i) *. d12.(i)
                                                                                       )
  in
  let dt4 i =      1.0 /. ( 4.0 *. ( pil i ) *. ( pil i ) ) *. (
                                                                   ( pip i ) *. ( pip i ) 
                                                                   +. exp( -2.0 *. ( p i ) ) *. ( piq i ) *. ( piq i )   
                                                                   +. exp( 2.0 *. t ) *. d10.(i) *. d10.(i)
                                                                   +. exp( 2.0 *. ( ( p i ) +. t ) ) *. d12.(i) *. d12.(i)
                                                                 )
                                                                 -. twist_squared *. exp( ( ( l i ) +. 2.0 *. ( p i ) -. 3.0 *. t ) /. 2.0 )                             
  in
  let dt5 i =       0.5 *. ( pil i ) *. twist_squared *. exp( ( ( l i ) +. 2.0 *. ( p i ) -. 3.0 *. t ) /. 2.0 )
  in
  let equations =  Array.map ~f:( fun x -> Array.init n ~f:x ) [| dt0 ; dt1 ; dt2 ; dt3 ; dt4 ; dt5 |] in
  deltat *.> { time = 1.0 ; data = equations }



let cnchangeframe frame deltat deltax  =
  let tempout0 = frame +>> ( changeframe2 frame deltat deltax ) in
  let tempdata1 = 0.5 *.> ( frame +>> tempout0 ) in
  let tempout1 = frame +>> ( changeframe2 tempdata1 deltat deltax ) in
  let tempdata2 = 0.5 *.> ( frame +>> tempout1 ) in
  changeframe2 tempdata2 deltat deltax

let evolveframe frame deltat deltax  =
  frame +>> ( changeframe2 frame deltat deltax )

let cnevolveframe frame deltat deltax  =
  frame +>> ( cnchangeframe frame deltat deltax  )

let listtoarray2 listlist = List.to_array ( List.map listlist ~f:( fun x  -> List.to_array x ) ) 

(*

Take a Yojson.basic list list of numbers, and return it as a float list list.

*)

let filterlist2 jsonlistlist = List.map jsonlistlist ~f:( fun x -> Yojson.Basic.Util.filter_float x ) 

let jsonframetoframe frame = 
  let member a = Yojson.Basic.Util.member a frame in 
  let to_float  = Yojson.Basic.Util.to_float in 
  let to_list  = Yojson.Basic.Util.to_list in 
  let filter_list  = Yojson.Basic.Util.filter_list in 
    {
      time = ( member "time" |> to_float )                                                 ;
      data = ( member "data" |> to_list |> filter_list |> filterlist2 |> listtoarray2 ) 
    }

(*

Take a path as a text string and return an array of time frames generated from the json data found there.

*)

let importdata path = 
  let member = Yojson.Basic.Util.member  in
  let from_file = Yojson.Basic.from_file  in
  let to_list  = Yojson.Basic.Util.to_list in 
    from_file path |> member "evolution" |> to_list |> List.map  ~f:jsonframetoframe |> List.to_array

let arraytolist2 arrayarray = Array.to_list ( Array.map arrayarray ~f:( fun x  -> Array.to_list x ) ) 

let jsonfloatlist floatlist = List.map floatlist ~f:( fun x -> `Float x ) 

let jsonlist list = `List list 

let jsonlist2 listlist = `List ( List.map listlist ~f:( fun x ->  jsonlist x ) )

let jsonlist2float floatlistlist = `List ( List.map floatlistlist ~f:( fun x ->  jsonlist ( jsonfloatlist x ) ) )

let jsonframe frame = 
  let listdata = arraytolist2 frame.data in
    `Assoc
         [ 
          ( "time", `Float frame.time );
          ( "data" , jsonlist2float listdata )
         ]

let removeassoc x = 
  match x with 
    | `Assoc c -> c 

let filesizebytes path =
  let open Core.Unix in
  try
      let infos = stat path in
      infos.st_size
    with
      _ -> 
         let open Int64 in
         0. |> of_float

let fileexists path =
  let open Core.Unix in
  try
      let _ = path |> stat in
      true
    with
      _ -> false

let sizecheck filename = 
  let size = filename |> filesizebytes in
  let fivehundredM =  2. ** 29. |> Int64.of_float in
  ( Int64.compare size fivehundredM ) > 0

let restlast l =
  let open List in
  let a = l |> rev in
  let open String in 
  match a with
   | [] -> None 
   | h::t -> Some (t |> List.rev |> concat, h)
let jsonwritepretty jsondata path = 
  let message = Yojson.Basic.pretty_to_string jsondata in
  let oc = Out_channel.create path in
  let _ = Out_channel.output_string oc message in
  Out_channel.close oc

let rec jsonwriteprettyrec jsondata path n =
  let testpath = path ^ "." ^ ( n |> Int.to_string ) ^ ".json" in
  match ( fileexists testpath ) && not ( sizecheck testpath ) with
    | true  -> jsonwritepretty jsondata testpath
    | false -> jsonwriteprettyrec jsondata path ( n + 1 )

let jsonwriteprettyfinal jsondata path = jsonwriteprettyrec jsondata path 1

let jsonevolution framearray =
  let framelist = Array.to_list framearray in
  let jsonframelist = List.map framelist ~f:( fun x -> jsonframe x ) in
    `Assoc
         [
          ( "evolution", `List jsonframelist )
         ]

let approxtimeframes res sizeinbytes = (4.717773988047213 *. res -. sizeinbytes ) /. ( -18.926682809367268 -. 141.59813374610232 *. res )

(*
open Core_bench.Std
open Core

let run_bench functionlist testdata =
  let tests = List.init ( List.length functionlist ) ~f:(
    fun i -> Bench.Test.create ~name:(Int.to_string i) (fun () -> ignore ( ( List.nth_exn functionlist i ) testdata ) ) 
  ) in
  Command.run (Bench.make_command tests)
*)

type evolution = { frontmatter : (string * Yojson.Basic.t ) list ; timeframes : time_frame array }

let frontget key evo =
  let rep ( k , x ) = if ( String.equal k key ) then Some x else None in
  evo.frontmatter |> List.map ~f:( rep )

let frontdelete key evo = 
  let newfront = evo.frontmatter |> List.filter ~f:( fun ( x , _ ) -> not ( String.equal x key ) ) in
  { evo with frontmatter = newfront }

let frontadd pair evo = 
  let newfront = List.join [ evo.frontmatter ; [ pair ] ] in
  { evo with frontmatter = newfront }

let frontreplace ( key , v )  evo = evo |> frontdelete key |> frontadd ( key , v )

let frontgetdef key converter default evo =
  evo 
    |> frontget key 
    |> List.filter_opt 
    |> fun x -> match List.last x with
                 | None   -> default
                 | Some y -> y |> converter

let evolast evo =
  let arlast ar = ar.(Array.length ar - 1) in
  let lastframe = arlast evo.timeframes in
  { evo with timeframes = [| lastframe |] }

let evobutlast evo =  
  let n = Array.length evo.timeframes in
  { evo with timeframes = ( evo.timeframes |> Array.filteri ~f:( fun i _ -> i < ( n - 1 ) ) ) }

let importdataevo path = 
  let member = Yojson.Basic.Util.member  in
  let from_file = Yojson.Basic.from_file  in
  let to_list  = Yojson.Basic.Util.to_list in 
  let jsondata = from_file path in
  let maybeevoisempty inputjson =
    match inputjson with 
      | `Null -> [||]
      | x -> x |> to_list |> List.map  ~f:jsonframetoframe |> List.to_array
  in
  let frames = member "evolution" jsondata |> maybeevoisempty in
  let test input = 
    match input with 
      | ("evolution",_) -> false
      | _ -> true in
  let filteroutevolution = List.filter ~f:test ( Yojson.Basic.Util.to_assoc jsondata ) in
  { frontmatter = filteroutevolution ; timeframes = frames }

let removebadframes inevo = 
  let tfs = inevo.timeframes in
  let test frame = frame.data |> Array.to_list |> Array.concat |> Array.fold ~init:true ~f:( fun a b -> 
    let c = match Float.classify b with 
             | Core.Float.Class.Infinite -> false
             | Core.Float.Class.Nan -> false
             | _ -> true
    in
    a && c
    )
  in
  let newtfs = tfs |> Array.filter ~f:test in
  { inevo with timeframes = newtfs } 

let jsonextra evolution =
  let nobadframesevo = removebadframes evolution in
  let framelist = Array.to_list nobadframesevo.timeframes in
  let jsonframelist = List.map framelist ~f:( fun x -> jsonframe x ) in
    `Assoc ( List.append nobadframesevo.frontmatter [ ( "evolution", `List jsonframelist ) ] )


(* Here's where the constraints stuff begins. *)


type sorc = | Sine | Cosine

type index = sorc * int 

type fourier = { deg : int ; f : index -> float }

type solnf = { 
  deg : int ; 
  p   : index -> float ; 
  pip : index -> float ; 
  q   : index -> float ; 
  piq : index -> float ; 
  l   : index -> float ; 
  pil : index -> float 
  }

let (  ~~  ) b c f = 
  { 
    deg = max b.deg c.deg ; 
    p   = ( fun i -> f ( b.p i   ) ( c.p i   ) ) ; 
    pip = ( fun i -> f ( b.pip i ) ( c.pip i ) ) ; 
    q   = ( fun i -> f ( b.q i   ) ( c.q i   ) ) ; 
    piq = ( fun i -> f ( b.piq i ) ( c.piq i ) ) ; 
    l   = ( fun i -> f ( b.l i   ) ( c.l i   ) ) ; 
    pil = ( fun i -> f ( b.pil i ) ( c.pil i ) )
  }

let ( +~ ) b c = ( ~~ ) b c ( +. )

let ( -~ ) b c = ( ~~ ) b c ( -. )

let (  *~  ) b c = 
  { 
    deg = c.deg ; 
    p   = ( fun i -> b *. ( c.p i   ) ) ; 
    pip = ( fun i -> b *. ( c.pip i ) ) ; 
    q   = ( fun i -> b *. ( c.q i   ) ) ; 
    piq = ( fun i -> b *. ( c.piq i ) ) ; 
    l   = ( fun i -> b *. ( c.l i   ) ) ; 
    pil = ( fun i -> b *. ( c.pil i ) )
  }

(* We were writing some code to do this tensor multiplication, and realized we needed something that emulated Mathematica's which function. I think this can be written recursively. *)

let rec which pairlist = 
  match pairlist with
    | []     -> None
    | a :: t -> match a with
                  | ( true  , b ) -> Some  b 
                  | ( false , _ ) -> which t

(* For testing we need something that emulates the indices function we wrote in the mathematica notebook. *)

let indices l : index list =
  let twhich pairlist = match which pairlist with
    | Some a -> a
    | None   -> []
  in 
  twhich [
    ( l < 0 , [] ) ;
    ( l = 0 , [(Cosine, 0)] ) ;
    ( true  ,
              let rec sinelist l = match l < 2 with 
                | true -> [ ( Sine , 1 ) ]
                | false -> ( Sine , l ) :: ( sinelist (l-1) ) 
              in 
              let rec cosinelist l = match l < 1 with 
                | true -> [ ( Cosine , 0 ) ]
                | false -> ( Cosine , l ) :: ( cosinelist (l-1) ) 
              in
              List.concat [  sinelist l  ;  cosinelist l  ] |> List.rev
                                                                         )
   ]

(* Want an inner product for these functions. Given functions f,g , the inner product will be 1/Pi * int_0^{2Pi} f g dx so that cos n x , sin n x are orthonormal. *)

let fprod ( f : fourier ) ( g : fourier ) = 
  let n = max f.deg g.deg in
  let o i = match i with
             | ( Cosine , 0 ) -> 2.0 *. ( f.f ( Cosine , 0 ) ) *. ( g.f ( Cosine , 0 ) )
             | x              -> ( f.f x ) *. ( g.f x )
  in
  n |> indices |> List.map ~f:o |> List.fold_left ~f:( +. ) ~init:0.0

let fnorm f = fprod f f

let sprod ( s : solnf ) ( r : solnf ) =
  let a =
    [
      { deg = s.deg ; f = s.p   } ;
      { deg = s.deg ; f = s.pip } ;
      { deg = s.deg ; f = s.q   } ;
      { deg = s.deg ; f = s.piq } ;
      { deg = s.deg ; f = s.l   } ;
      { deg = s.deg ; f = s.pil } ;
    ] in
  let b = 
    [
      { deg = r.deg ; f = r.p   } ;
      { deg = r.deg ; f = r.pip } ;
      { deg = r.deg ; f = r.q   } ;
      { deg = r.deg ; f = r.piq } ;
      { deg = r.deg ; f = r.l   } ;
      { deg = r.deg ; f = r.pil } ;
    ] in
   List.map2_exn a b ~f:fprod |> List.fold_left ~f:( +. ) ~init:0.0

let snorm f = sprod f f

let sdist f g = snorm ( f -~ g )

let solnf_of_ar ar =  
  match ar with
   | [| 
        [| ops   ; opc   |] ;
        [| opips ; opipc |] ;
        [| oqs   ; oqc   |] ;
        [| opiqs ; opiqc |] ;
        [| ols   ; olc   |] ;
        [| opils ; opilc |] 
     |] -> 
     begin
       let lmax l = List.fold_left l ~f:max ~init:0 in
       let ns = List.map [ ops ; opips ; oqs ; opiqs ; ols ; opils ] ~f:Array.length |> lmax in
       let nc = List.map [ opc ; opipc ; oqc ; opiqc ; olc ; opilc ] ~f:Array.length |> lmax in
       let n = ( max (ns+1) nc ) - 1 in
       let o x j = 
         match j < Array.length x with
          | true -> x.(j)
          | false -> 0.0
       in
       let oo x ( c , j ) =
          match x with
           | [| xs ; xc |] ->
            begin
              match c with
               | Sine   -> o xs ( j - 1 )
               | Cosine -> o xc j
            end
           | _ -> 0.0
       in 
       { 
         deg = n ; 
         p   = oo [| ops   ; opc   |] ; 
         pip = oo [| opips ; opipc |] ; 
         q   = oo [| oqs   ; oqc   |] ; 
         piq = oo [| opiqs ; opiqc |] ; 
         l   = oo [| ols   ; olc   |] ; 
         pil = oo [| opils ; opilc |] 
       }
     end
   | _ -> 
       let zero _ = 0.0 in
       { 
         deg = 0 ; 
         p   = zero ; 
         pip = zero ; 
         q   = zero ; 
         piq = zero ; 
         l   = zero ; 
         pil = zero 
       }

let ar_of_solnf f =
  let n = f.deg in
  let sinds = n |> indices |> Array.of_list |> Array.filter_map ~f:( fun ( c , j ) -> match c with | Sine -> Some ( c , j ) | Cosine -> None ) in
  let cinds = n |> indices |> Array.of_list |> Array.filter_map ~f:( fun ( c , j ) -> match c with | Cosine -> Some ( c , j ) | Sine -> None ) in
  let nes f = Array.map ~f:f sinds in
  let nec f = Array.map ~f:f cinds in

      [| 
        [| nes f.p   ;  nec f.p   |] ;
        [| nes f.pip ;  nec f.pip |] ;
        [| nes f.q   ;  nec f.q   |] ;
        [| nes f.piq ;  nec f.piq |] ;
        [| nes f.l   ;  nec f.l   |] ;
        [| nes f.pil ;  nec f.pil |] ;
      |]


let getfourier evolution = 
  let testf input = 
    match input with 
      | ("fourier coefficients",_) -> true
      | _ -> false in 
  let fourier = List.filter ~f:testf evolution.frontmatter in
  let blah = List.nth_exn fourier 0 in
  let test2 shit =
    match shit with
      | (_,x) -> x in
  let json = test2 blah in
  let jsonlist = Yojson.Basic.Util.to_list json in
  let process onefunctionfourier = 
    let list = [ Yojson.Basic.Util.member "sine" onefunctionfourier ; Yojson.Basic.Util.member "cosine" onefunctionfourier ] in
    let list2 = List.map list ~f:Yojson.Basic.Util.to_list in
    List.map list2 ~f:Yojson.Basic.Util.filter_float |> listtoarray2
  in
  Array.of_list ( List.map jsonlist ~f:process ) |> solnf_of_ar

let arraysum ar = 
  let n = Array.length ar in
  let total = ref 0.0 in
  let i = ref 0 in
  while !i < n do
    total := !total +. ar.(!i) ;
    incr i
  done;
  !total

let ffromfourier coefarray =
  let sin x = Stdlib.sin x in
  let cos x = Stdlib.cos x in
  let i2f n = Float.of_int n in
  let sincoefs = coefarray.(0) in
  let coscoefs = coefarray.(1) in
  let sina n x = sincoefs.(n) *. sin( ( i2f( n + 1 ) ) *. x ) in
  let cosa n x = coscoefs.(n) *. cos( ( i2f  n       ) *. x ) in
  let sines x   = Array.init ( Array.length sincoefs ) ~f:( fun n -> sina n x ) in
  let cosines x = Array.init ( Array.length coscoefs ) ~f:( fun n -> cosa n x ) in
  ( fun x -> ( arraysum ( sines x ) ) +. ( arraysum ( cosines x ) ) )

let initialtime evolution = 
  let frontmatter = evolution.frontmatter in
  let selector input = 
    match input with
      | ("initial time",_) -> true
      | _ -> false
  in
  let strip input = 
    match input with
      | ("initial time", x ) -> x
      | _ -> `Null
  in
  ( List.nth_exn ( List.filter frontmatter ~f:selector ) 0 ) |> strip |> Yojson.Basic.Util.to_float

let generatefirstframe evolution n =
  let deltax = ( 2.0 *. pi ) /. ( Float.of_int n ) in
  let x j = ( Float.of_int j ) *. deltax in
  let coefs = getfourier evolution |> ar_of_solnf in
  let ithfun i = ffromfourier coefs.(i) in
  let numberoffunctions = Array.length coefs in
  match ( evolution.timeframes ) with
    | [||] -> { 
                 frontmatter = evolution.frontmatter ; 
                 timeframes =  [|
                   { 
                      time = initialtime evolution ; 
                      data = Array.init numberoffunctions ~f:( fun i -> Array.init n ~f:( fun j -> ( ithfun i ) ( x j ) ) ) 
                   }
                               |]
              }
    | _ -> evolution

let prepare n evolution =
  match evolution.timeframes with
    | [||] -> generatefirstframe evolution n
    | _ -> evolution

(*

Want functions to calculate the constraint.

*)

let galconstraint tf = 
  let p   = tf.data.(0) in
  let pip = tf.data.(1) in
  let q   = tf.data.(2) in
  let piq = tf.data.(3) in
  let l   = tf.data.(4) in
  let pil = tf.data.(5) in

  let d data = first_derivative_4th_order ~valuearray:data ~delta:( 2.0 *. pi /. ( Float.of_int ( Array.length p ) ) ) in
  let dp = d p in
  let dq = d q in
  let dl = d l in
  dp *|| pip +|| dq *|| piq +|| dl *|| pil 

let arraytail ar =
  let n = Array.length ar in
  ar.(n-1)

let mainconstraint 
  ~inevolution
  ~timelimit
  ~checkpointfrequency
  ~cn
  ~checkpointtfs
  ?( charspeedtest = ( fun _ -> 1.0 ) )
  ?( fileind = 1 )
  ~path
  () =

  let evolution = ref ( inevolution ) in (* the ref evolution holds the data to eventually be written *)
  let frame = ref ( arraytail( inevolution.timeframes ) ) in (* the ref frame holds the current data *)

  let deltax = 2.0 *. pi /. Float.of_int ( Array.length ( (!frame).data.(0) ) ) in (* deltax is the spatial separation computed supposing the domain is the circle with coordinates in 0 to 2pi *)
  let evolveonestep = (* here we set which evolver to use based on whether we want to do Crank-Nicolson or not *)
    match cn with
      | "false" -> evolveframe
      | "true"  -> cnevolveframe
  in

  let lasttime  = ref (!frame).time  in
  let lastprinttime  = ref ( timenow () )  in 
  let file = ref fileind in

  
  while begin let open Float in ( ( (!frame).time    < timelimit ) && ( not ( Sys.file_exists "stop" ) ) ) end do

    let deltat = 
      let schemeconstant = 
        match cn with
          | "false" -> 1.0 
          | "true"  -> 1.0 (* if this value is 2 or higher, there is nearly immediate NaN for generic data. values lower than 1 don't seem to provide any benefit. *)
          | _       -> 1.0 
      in
      let characteristicspeed = charspeedtest !frame in
      deltax *. schemeconstant *. characteristicspeed
    in

    begin
      frame := evolveonestep !frame deltat deltax ;
      if begin let open Float in ( (!frame).time   > ( !lasttime +. checkpointfrequency ) ) end then 
        begin
          evolution := { frontmatter = (!evolution).frontmatter ; timeframes = Array.append (!evolution).timeframes [| !frame |] } ; (* append a new checkpoint *)
          lasttime := (!frame).time ; (* update the last iterator we checkpointed *)
          lastprinttime := timenow () ;
          if checkpointtfs < ( (!evolution).timeframes |> Array.length ) then
            begin
              jsonwritepretty ( !evolution |> evobutlast |> jsonextra ) ( path ^ "." ^ ( !file |> Int.to_string ) ^ ".json" ) ;
              evolution := evolast !evolution ;
              file := !file + 1 ;
            end ;
          !lasttime |> Float.to_string |> Out_channel.output_string stdout ;
          Out_channel.newline stdout;
          Time.now () |> Time.to_string |> Out_channel.output_string stdout ;
          Out_channel.newline stdout;
          path |> Out_channel.output_string stdout ;
          Out_channel.newline stdout;
          Out_channel.newline stdout;
        end
    end;

  done ;
  evolution := { frontmatter = (!evolution).frontmatter ; timeframes = Array.append (!evolution).timeframes [| !frame |] } ;

  jsonwritepretty ( !evolution |> evobutlast |> jsonextra ) ( path ^ "." ^ ( !file |> Int.to_string ) ^ ".json" )

let sort ~evolution =
  let data = ref evolution.timeframes in
  let comparator frame1 frame2 = 
    match Float.sign_exn ( frame1.time -. frame2.time ) with
      | Sign.Zero -> 0
      | Sign.Pos  -> 1
      | Sign.Neg  -> -1
  in
  Array.sort ~compare:comparator !data;
  { frontmatter = evolution.frontmatter ; timeframes = !data }

let coarsify ~evolution ~sep =
  let data = (sort evolution).timeframes in
  let n = Array.length data in
  let i = ref 1 in
  let lastappend = ref 0 in
  let outdata = ref [| data.(!lastappend) |] in
  while ( !i < n  ) do
    begin
    if begin let open Float in data.( !i  ).time -. data.( !lastappend ).time >= sep end then 
      begin
        outdata := Array.append !outdata [| data.( !i  ) |] ;
        lastappend := !i ; 
      end;
    incr i 
    end;
  done;
  if not ( phys_equal ( arraytail !outdata ).time  ( arraytail data ).time ) then 
    begin
      outdata := Array.append !outdata [| arraytail data |] ;
    end;
  { frontmatter = evolution.frontmatter ; timeframes = !outdata }

(* Ok, now that we have which, let's directly copy the Mathematica we wrote. *)

let a ( ( ichar , ival ) : index ) ( ( jchar , jval ) : index ) ( ( lchar , lval ) : index ) =
  let twhich pairlist = match which pairlist with
    | Some a -> a
    | None   -> 0.0 
  in 
  let (==) a b = phys_equal a b in
  twhich 
    [
      ( ( ichar , ival ) == ( Cosine , 0 ) && ( jchar , jval ) == ( lchar , lval ) , 1.0 ) ;
      ( ( jchar , jval ) == ( Cosine , 0 ) && ( ichar , ival ) == ( lchar , lval ) , 1.0 ) ;
      ( ichar == jchar && jchar == Cosine ,
                              twhich [
                                       ( lchar == Cosine && lval == ival - jval , 0.5 ) ;
                                       ( lchar == Cosine && lval == jval - ival , 0.5 ) ;
                                       ( lchar == Cosine && lval == jval + ival , 0.5 ) ;
                                     ]
                                                                                     ) ;
      ( ichar == jchar && jchar == Sine ,
                              twhich [
                                       ( lchar == Cosine && lval == ival - jval ,  0.5 ) ;
                                       ( lchar == Cosine && lval == jval - ival ,  0.5 ) ;
                                       ( lchar == Cosine && lval == jval + ival , -0.5 ) ;
                                     ]
                                                                                     ) ;
      ( ichar == Cosine && jchar == Sine ,
                              twhich [
                                       ( lchar == Sine && lval == ival - jval , -0.5 ) ;
                                       ( lchar == Sine && lval == jval - ival ,  0.5 ) ;
                                       ( lchar == Sine && lval == jval + ival ,  0.5 ) ;
                                     ]
                                                                                     ) ;
      ( ichar == Sine && jchar == Cosine ,
                              twhich [
                                       ( lchar == Sine && lval == ival - jval ,  0.5 ) ;
                                       ( lchar == Sine && lval == jval - ival , -0.5 ) ;
                                       ( lchar == Sine && lval == jval + ival ,  0.5 ) ;
                                     ]
                                                                                     ) ;
    ]

(*

  We need a function that does the derivative on fourier coeffs. 
  The function we are taking the derivative of comes first.

*)

let d ( ( inchar , inval ) : index ) ( ( outchar , outval ) : index ) =
  let twhich pairlist = match which pairlist with
    | Some a -> a
    | None   -> 0.0 
  in 
  let (==) a b = phys_equal a b in
  twhich 
    [
      ( outval = 0 , 0.0 ) ;
      ( inval = outval && not ( inchar == outchar ) , 
                         twhich 
                           [ 
                             ( inchar == Cosine , -. float_of_int inval ) ;
                             ( inchar == Sine , float_of_int inval ) 
                           ] 
                                                                          )
    ]

(* Given a list of fourier indices, we want to sum some function over them, and obtain the result. *)

let contract f l =
   l |> indices |> List.map ~f:( fun i -> f i ) |> List.fold_left ~f:( +. ) ~init:0.0

(* Works correctly. *)

let b i k l =  
  let ( _ , n ) = k in
  contract ( fun j -> ( a i j l ) *. ( d k j ) ) n

(* This is here as part of a hack to resolve the fact that the lhs is not invertible. *)

let mdrop1 i m =
  let drop i ar = Array.filteri ar ~f:( fun j _ -> not ( j = i ) ) in
  let ardrop i ar = ar |> drop i in
  m |> Lacaml.D.Mat.to_array |> ardrop i |> Lacaml.D.Mat.of_array

let mdrop2 i j m =
  let drop i ar = Array.filteri ar ~f:( fun j _ -> not ( j = i ) ) in
  let t = Array.transpose_exn in
  let ardrop i j ar = ar |> drop i |> t |> drop j |> t in
  m |> Lacaml.D.Mat.to_array |> ardrop i j |> Lacaml.D.Mat.of_array

(* Given data straight from getfourier, this produces the function forms of the functions. *)

let fgpairs f =
  ( f.deg , [ ( f.pip , f.p ) ; ( f.pil , f.l ) ; ( f.piq , f.q ) ] )

let () = Lacaml.Io.Toplevel.lsc 20

(* Got the basic form of this from https://stackoverflow.com/a/22132595 
   For lists l1 and l2 and computes the set complement l1\l2            *)

let diff l1 l2 = List.filter ~f:( fun x -> not ( List.mem ~equal:( phys_equal ) l2 x ) ) l1

(* Make a Lacaml.D matrix from an float array array. *)

let makemat ar =
  let n = ar |> Array.length in
  let m = ar.(0) |> Array.length in
  Lacaml.D.Mat.init_cols n m ( fun i j -> ar.(i-1).(j-1) )

let makematfromcols collist =
  let mataddcol m c =
    let mlist = m |> Array.transpose_exn |> Array.to_list in
    let col = c |> Array.transpose_exn |> Array.to_list in
    List.join [ mlist ; col ] |> List.to_array |> Array.transpose_exn
  in
  let rec matjoin collist mat =
    match collist with
     | [] -> mat
     | col::tail -> matjoin tail ( mataddcol mat col )
  in
  match collist with
   | [] -> [|[||]|]
   | initial::remainder -> matjoin remainder initial

(* 
    The summands on the right side are contractions b i j pif i f j where the sum is over either 
      - i for some fixed j
      - j for some fixed i
 *)

let rhsummand ( f , g ) iinds jinds n =
  let init k =
    let l = List.nth_exn ( indices ( 2 * n ) ) k in
    match List.length iinds with
     | 1 -> begin
              let i::_ = iinds in
              contract ( fun j -> ( b i j l ) *. ( f i ) *. ( g j ) ) n
            end
     | _ -> begin
              let j::_ = jinds in
              contract ( fun i -> ( b i j l ) *. ( f i ) *. ( g j ) ) n
            end
  in
  [| Array.init ( 4 * n + 1 ) ~f:init |] |> Array.transpose_exn

(* Here is where we need the universal definitions of the RHS, LHS, and free indices. *)

type lrc = | Left | Right | Constrained

type fn = | P | Pip | Q | Piq | L | Pil

let inds lrc fn n = 
  let all  = indices n in
  let none = [] in
  match lrc with
   | Left ->
       begin
       match fn with
        | P   -> all
        | Pip -> none
        | Q   -> all
        | Piq -> none
        | L   -> none
        | Pil -> all
       end
   | Right ->
       begin
       match fn with
        | P   -> none
        | Pip -> [ ( Cosine , 1 ) ]
        | Q   -> none
        | Piq -> diff ( indices n ) [ ( Cosine , 1 ) ]
        | L   -> [ ( Cosine , 0 ) ]
        | Pil -> none
       end
   | Constrained ->
       begin
       match fn with
        | P   -> none
        | Pip -> diff ( indices n ) [ ( Cosine , 1 ) ]
        | Q   -> none
        | Piq -> [ ( Cosine , 1 ) ]
        | L   -> diff ( indices n ) [ ( Cosine , 0 ) ]
        | Pil -> none
       end

let pget lrc f = 
  let oo fn y i = 
    match List.exists ( inds lrc fn f.deg ) ( fun x -> phys_equal x i ) with
     | true  -> y i
     | false -> 0.0
  in
  {
     deg = f.deg ;
     p   = oo P   f.p   ;
     pip = oo Pip f.pip ;
     q   = oo Q   f.q   ;
     piq = oo Piq f.piq ;
     l   = oo L   f.l   ;
     pil = oo Pil f.pil 
  }
  

(* The vector on the right side should then be the sum of rhsummand over some choices of indices. *)

let rhs fglist n =
  let [ ( pip  , p ) ; ( pil , l ) ; ( piq , q ) ] = fglist in

  let pipcols = inds Right Pip n  |> List.map ~f:( fun i -> rhsummand ( pip  , p ) [ i ]         ( indices n ) n ) in

  let lcols   = inds Right L n    |> List.map ~f:( fun i -> rhsummand ( pil  , l ) ( indices n ) [ i ]         n ) in

  let piqcols = inds Right Piq n  |> List.map ~f:( fun i -> rhsummand ( piq  , q ) [ i ]         ( indices n ) n ) in

  let zero = [| Array.init ( 4 * n + 1 ) ~f:( fun _ -> 0.0 ) |] |> Array.transpose_exn in

  [ pipcols ; lcols ; piqcols ] |> List.join |> List.fold_left ~f:( +|||| ) ~init:zero |> ( fun m -> -1.0 *.|| m ) |> makemat

let lhscol ( f , g ) iinds jinds n =
  let init k =
    let l = List.nth_exn ( indices ( 2 * n ) ) k in
    match List.length iinds with
     | 1 -> begin
              let i::_ = iinds in
              contract ( fun j -> ( b i j l ) *. ( g j ) ) n
            end
     | _ -> begin
              let j::_ = jinds in
              contract ( fun i -> ( b i j l ) *. ( f i ) ) n
            end
  in
  [| Array.init ( 4 * n + 1 ) ~f:init |] |> Array.transpose_exn

let lhs fglist n =
  let [ ( pip  , p ) ; ( pil , l ) ; ( piq , q ) ] = fglist in

  let pipcols = inds Constrained Pip n  |> List.map ~f:( fun i -> lhscol ( pip  , p ) [ i ]         ( indices n ) n ) in

  let lcols   = inds Constrained L n    |> List.map ~f:( fun i -> lhscol ( pil  , l ) ( indices n ) [ i ]         n ) in

  let piqcols = inds Constrained Piq n  |> List.map ~f:( fun i -> lhscol ( piq  , q ) [ i ]         ( indices n ) n ) in

  [ pipcols ; lcols ; piqcols ] |> List.join |> makematfromcols |> makemat

let solvesystem a b = 
  let c = b in
  getrs a c;
  c

let solvecostraintsf dat =
  let ( n , _  ) = dat |> fgpairs in
  let ( _ , lfg ) = dat |> pget Left                                        |> fgpairs in
  let ( _ , rfg ) = dat |> ( fun f -> ( pget Right f ) +~ ( pget Left f ) ) |> fgpairs in
  let m = lhs lfg n in
  let x = rhs rfg n in
  let [| v |] = solvesystem m x |> Lacaml.D.Mat.to_array |> Array.transpose_exn in
  let [| 
        [| ops   ; opc   |] ;
        [| _     ; opipc |] ;
        [| oqs   ; oqc   |] ;
        [| opiqs ; opiqc |] ;
        [| _     ; olc   |] ;
        [| opils ; opilc |] 
      |] = dat |> ar_of_solnf in

  let npips = Array.init n ~f:( fun i -> v.(i+n) ) in
  let npipc = Array.init (n+1) ~f:( fun i -> 
                                      match i with
                                       | 1 -> opipc.(1)
                                       | 0 -> v.(0)
                                       | j -> v.(j-1)
                                  ) in
  let npiqc = Array.init (n+1) ~f:( fun i -> 
                                      match i with
                                       | 1 -> v.(4*n)
                                       | j -> opiqc.(j)
                                  ) in
  let nls = Array.init n ~f:( fun i -> v.(3*n+i) ) in
  let nlc = Array.init (n+1) ~f:( fun i ->
                                    match i with
                                     | 0 -> olc.(0)
                                     | j -> v.(2*n+j-1)
                                ) in
   
  [|
    [| ops   ; opc   |] ;
    [| npips ; npipc |] ;
    [| oqs   ; oqc   |] ;
    [| opiqs ; npiqc |] ;
    [| nls   ; nlc   |] ;
    [| opils ; opilc |] 
  |] |> solnf_of_ar

let jl l = List.map l ~f:(fun x -> `Float x )

let jll [ s ; c ] = `Assoc  (
    [("sine", `List ( jl s ) ) ; ("cosine", `List ( jl c ) ) ]
  )

let putfourier dat = 

  let t1 ar = Array.to_list ar in
  let t2 ar = Array.map ar ~f:t1 |> Array.to_list in
  let t3 ar = Array.map ar ~f:t2 |> Array.to_list in

  let dot = t3 dat in

  {frontmatter=[("initial time", `Float 0.); ("fourier coefficients", `List ( List.map dot ~f:jll ) ) ] ; timeframes = [||]}


let solvecostraints dat = dat |> getfourier |> solvecostraintsf 

let lin p s0 s1 = ( ( 1.0 -. p ) *~ s0 ) +~ ( p *~ s1 ) 

let hom s d0 d1=
  let n = max d0.deg d1.deg |> Float.of_int in
  let ld1 = pget Left d1 in
  let ld0 = pget Left d0 in
  let rd1 = pget Right d1 in
  let rd0 = pget Right d0 in
  begin
  match s with
   | 0.0 -> ld0 +~ rd0
   | 1.0 -> ld1 +~ rd1
   | _   -> ( lin s ld0 ld1 ) +~ ( lin ( s ** ( 4.0 *. n ) ) rd0 rd1 ) 
  end 
  |> solvecostraintsf

let arraymap2 ar f = Array.map ar ~f:( fun x -> Array.map x ~f:f ) 

let arraymap3 ar f = Array.map ar ~f:( fun x -> arraymap2 x f )

let zerosoln = 
  let z _ = 0.0 in
  {
     deg = 0 ;
     p   = z ;
     pip = z ;
     q   = z ;
     piq = z ;
     l   = z ;
     pil = z 
  }

let my_float_epsilon = 2.22044604925 *. 10.0 ** ( -16.0 ) 

let range low high step =
  let t = 1.0 +. ( high -. low ) /. step |> Float.round_down |> Int.of_float in
  List.init t ~f:( fun i -> low +. ( step *. Float.of_int i ) )

let table l ~f =
  List.init ( List.length l ) ~f:( fun i -> List.nth_exn l i |> f )

let random_soln n x =
  let rec h () = 
    let r () = ( Random.float ( 2.0 *. x ) ) -. x in
    let rands = Array.init 6 ~f:( fun _ -> Array.init ( 2 * n + 1 ) ~f:(fun _ -> r () ) ) in
    let f j ( x , i ) = match i < n+1 with
                     | true -> begin
                             match x with 
                              | Cosine -> rands.(j).(i) 
                              | Sine   -> rands.(j).(i+n)
                              end 
                     | false -> 0.0
    in
    let testsoln = {
      deg = n ;
      p   = f 0 ;
      pip = f 1 ;
      q   = f 2 ;
      piq = f 3 ;
      l   = f 4 ;
      pil = f 5
      } 
    in 
    let nsoln = testsoln |> ar_of_solnf |> putfourier |> ( fun x -> generatefirstframe x 10000 ) in
    let points = nsoln.timeframes.(0).data.(5) in
    let rec test l = 
      match l with
       | [] -> true 
       | x::tail -> 
          begin
          match begin let open Float in x < 0.0 end with
           | true -> false
           | false -> test tail
          end
    in
    match points |> Array.to_list |> test with
     | true -> testsoln |> solvecostraintsf
     | false -> h ()
  in
  h ()

let random_soln_2 n x =
    let r () = ( Random.float ( 2.0 *. x ) ) -. x in
    let rands = Array.init 6 ~f:( fun _ -> Array.init ( 2 * n + 1 ) ~f:(fun _ -> r () ) ) in
    let f j ( x , i ) = match i < n+1 with
                     | true -> begin
                             match x with 
                              | Cosine -> rands.(j).(i) 
                              | Sine   -> rands.(j).(i+n)
                              end 
                     | false -> 0.0
    in
    let testsoln = {
      deg = n ;
      p   = f 0 ;
      pip = f 1 ;
      q   = f 2 ;
      piq = f 3 ;
      l   = f 4 ;
      pil = f 5
      }
    in 
    let nsoln = testsoln |> ar_of_solnf |> putfourier |> ( fun x -> generatefirstframe x 10000 ) in
    let points = nsoln.timeframes.(0).data.(5) in
    let pilmin = points |> Array.fold ~f:Float.min ~init:Float.infinity in
    let outsoln = match begin let open Float in pilmin > 0.0 end with
                   | true -> testsoln
                   | false ->
    { testsoln with pil = ( fun ( k , i ) -> match ( k , i ) with 
                                              | ( Cosine , 0 ) -> -2.0 *. pilmin +. f 5 ( Cosine , 0 )
                                              | _              -> f 5 ( k , i )
                          ) } in
    outsoln |> solvecostraintsf

let bergerify s =
  let z _ = 0.0 in
  {
    deg = s.deg ;
    p   = z     ;
    pip = s.pip ;
    q   = s.q   ;
    piq = z     ;
    l   = z     ;
    pil = ( fun i -> match i with | ( Cosine , 0 ) -> s.pil ( Cosine , 0 ) | _ -> 0.0 )
  }

let bergerify2 s =
  let z _ = 0.0 in
  {
    deg = s.deg ;
    p   = s.p   ;
    pip = s.pip ;
    q   = s.q   ;
    piq = z     ;
    l   = s.l   ;
    pil = s.pil
  }

let b_soln i x = 
  let s = random_soln_2 i x in
  let c f = ( fun i -> match i with
             | ( Cosine , 0 ) -> 0.0
             | x              -> f x 
            ) in
  { s with piq = c s.piq } |> solvecostraintsf 

let bev_soln i x = 
  let s = random_soln_2 i x in
  let z = ( fun _ -> 0.0 ) in
  { s with p = z ; piq = z ; l = z }

let gen_soln = random_soln_2

let q_soln i x = 
  let s = random_soln_2 i x in
  let z = ( fun _ -> 0.0 ) in
  { s with piq = z } |> solvecostraintsf

let take_soln_norm f i x norm =
  let rec h () =
    let test = f i x in
    match ( let open Float in snorm test < norm ) with
     | true -> test
     | false -> h ()
  in
  h ()

let pol_soln i x = 
  let s = random_soln_2 i x  in
  let z = ( fun _ -> 0.0 ) in
  { s with piq = z } |> solvecostraintsf |> ( fun s -> { s with q = z ; piq = z } )

let write_solution soln tl chk res freq basepath =
  let p = ( basepath ^ "_" ^ ( res |> Int.to_string ) ^ ".json" ) in
  soln
   |> ar_of_solnf 
   |> putfourier 
   |> prepare res
   |> frontadd ("time limit", `Float tl ) 
   |> frontadd ("checkpoint size", `Int chk ) 
   |> frontadd ("resolution", `Int res )
   |> frontadd ("print frequency", `Float freq )  
   |> jsonextra 
   |> fun evo -> jsonwritepretty evo p

let write_soln_over_res soln tl chk freq basepath reslist =
  List.map ~f:(fun r -> write_solution soln tl chk r freq basepath) reslist

let gen_and_write_soln_over_reses kind order size tl chk freq basepath reslist =
  let soln =
    match kind with
     | "generic" -> gen_soln order size
     | "b0" -> b_soln order size
     | "polarised" -> pol_soln order size
     | _ -> 
         begin
           let z _ = 0.0 in
           { deg = 0 ; p = z ; pip = z ; q = z ; piq = z ; l = z ; pil = z }
        end
  in
  write_soln_over_res soln tl chk freq basepath reslist

let powers_of_two a b =
  match Int.min a b < 0 with
   | true -> []
   | false ->
      begin
        match a < b with
         | true -> List.init ~f:(fun i -> 2.0 ** ( Float.of_int ( i + a ) ) |> Int.of_float ) ( 1 + b - a )
         | false -> List.init ~f:(fun i -> 2.0 ** ( Float.of_int ( i + b ) ) |> Int.of_float ) ( 1 + a - b ) |> List.rev
      end

let writenewreses file reslist =
  let ev = importdataevo file in
  let basepath = 
    let l = file |> String.split ~on:'/' in
    let name = l |> List.last_exn |> String.chop_suffix_exn ~suffix:".json" in
    let k = name |> String.split ~on:'_' in
    let m = k |> List.length in
    let basefile = List.take k ( m - 1 ) |> String.concat ~sep:"_" in
    let n = l |> List.length in
    let path =  List.take l ( n - 1 ) |> String.concat ~sep:"/" in
    path ^ "/" ^ basefile
  in
  let writeoneres res = { ev with timeframes = [| |] } |> frontreplace ( "resolution" , `Int res ) |> jsonextra |> ( fun evo -> jsonwritepretty evo ( basepath ^ "_" ^ ( Int.to_string res ) ^ ".json" ) ) in
  List.map ~f:writeoneres reslist


let processbevline instring =
  let isfloat a =
    match a with
      | '0' -> true
      | '1' -> true  
      | '2' -> true
      | '3' -> true
      | '4' -> true
      | '5' -> true
      | '6' -> true
      | '7' -> true
      | '8' -> true
      | '9' -> true
      | '-' -> true
      | '+' -> true
      | 'e' -> true
      | 'E' -> true
      | '.' -> true
      | '\t'-> true
      | _   -> false
  in
  let filteredstringlist = String.filter instring ~f:isfloat in
  let istab x = ( if phys_equal x '\t' then true else false ) in
  let filteredstringliststrippedtrailingtab = String.strip filteredstringlist ~drop:istab in
  let stringlist = String.split filteredstringliststrippedtrailingtab ~on:( '\t' ) in
  let floatlist = List.map stringlist ~f:Float.of_string in
  let floatarray = Array.of_list floatlist in
  floatarray

let getbevfile path = 
  let linelist = In_channel.read_lines path in
  let containsnumeral instring =  String.contains instring '.'   in
  let checkedifavg = List.filter ~f:containsnumeral linelist in
  let listoffloatarrays = List.map checkedifavg ~f:processbevline in
  let floatarrayarray = Array.of_list listoffloatarrays in
  floatarrayarray

let processbevinputs pathtodir g=
  let checktrailingslash string =
    let n = - 1 + String.length string in
    let lastchar = String.get string n in
    match lastchar with
      | '/' -> string
      | _   -> string^"/"
  in
  let path = checktrailingslash pathtodir in
  let avg      = getbevfile ( path^"gwdyavg.txt" ) in
  let p        = getbevfile ( path^"gwdyp.txt" )   in
  let pip      = getbevfile ( path^"gwdypip.txt" ) in
  let q        = getbevfile ( path^"gwdyq.txt" )   in
  let piq      = getbevfile ( path^"gwdypiq.txt" ) in
  let lambda   = getbevfile ( path^"gwdyl.txt" )   in
  let pilambda = getbevfile ( path^"gwdypil.txt" ) in
  Array.init ( Array.length p ) ~f:( fun i -> { time = ( g avg.(i).(0) ) ; data = [| p.(i) ; pip.(i) ; q.(i) ; piq.(i) ; lambda.(i) ; pilambda.(i) |] } )

let bevtojsonfile f pathtobevdata pathtojsonfile = 
  let bevtimeframearray = processbevinputs pathtobevdata f in
  let jsonoutput = jsonevolution bevtimeframearray in
  jsonwritepretty jsonoutput pathtojsonfile


(*

Need to modify the program to assume that execution parameters are given in the command line argument.
Things that come from the script are usually:
	-path to the folder where we should write the output data
	-the name we should use for the output file
	-the spatial resolution
	-time limit
	-print frequency
That's all.

Here's what we need to do:
	-the command line parser needs to separate the input into a path and a filename, which we can then use for the first two
	-the resolution item needs to do the following: if the timeframes are not empty, read a timeframe and get the resolution from there, else look in the frontmatter for a resolution parameter
	-time time limit and print frequency also need to be specified in the frontmatter

*)


let main inputpath =
  let ( path , outpath ) = 
    let l = inputpath |> String.split ~on:'/' in
    let name = l |> List.last_exn |> String.chop_suffix_exn ~suffix:".json" in
    let n = l |> List.length in
    let path =  List.take l ( n - 1 ) |> String.concat ~sep:"/" in
    ( path , name ) 
  in
  let () = match path with 
            | "" -> () 
            | x  -> Sys.chdir x 
  in

  let finput = outpath ^ ".json" |> importdataevo in
  let checkpointfrequency = finput |> frontgetdef "print frequency" Yojson.Basic.Util.to_float Float.min_positive_normal_value in
  let n = finput |> frontgetdef "resolution" Yojson.Basic.Util.to_int 0 in
  let timelimit = ( finput |> frontgetdef "time limit" Yojson.Basic.Util.to_number 0. ) +. checkpointfrequency in
  let size = finput |> frontgetdef "checkpoint size" Yojson.Basic.Util.to_int ( 2.0 ** 29.0 |> Int.of_float ) in
  let tfs = Int.of_float ( approxtimeframes ( Float.of_int n ) ( Float.of_int size ) ) in

  let lastind path =
    let rec f path ind = 
      match path ^ "." ^ ( ind |> Int.to_string ) ^ ".json" |> fileexists with
        | false -> ind - 1
        | true  -> f path ( ind + 1 ) 
    in
    f path 1 |> ( fun y -> match y with | 0 -> 1 | x -> x )
  in
  
  let ind = lastind outpath in
  let firstfile = outpath ^ "." ^ ( ind |> Int.to_string ) ^ ".json" in
  let input =
    match Sys.file_exists firstfile with
      | true -> importdataevo firstfile 
      | false -> 
          let inevo = finput in
          let firstframe = (prepare n inevo).timeframes.(0) in
          { frontmatter = inevo.frontmatter ; timeframes = [| firstframe |] }
  in

  let test timeframe      = 
    let data = timeframe.data in
    let t =    timeframe.time in
    let pilambda = data.(5) in
    exp( -1.0 *. t ) *. ( Array.fold pilambda ~init:Float.infinity ~f:Float.min )
  in

  let _ = mainconstraint 
    ~inevolution:input
    ~timelimit:timelimit
    ~checkpointfrequency:checkpointfrequency
    ~cn:"true" 
    ~checkpointtfs:tfs
    ~charspeedtest:test 
    ~fileind:ind
    ~path:outpath 
    ()
  in
  ()

	
let filename_param =
  let open Command.Param in
  anon ("filename" %: string)

let command =
  Command.basic
    ~summary:"evolve t2 symmetric initial data"
    (
      Command.Param.map filename_param ~f:(
        fun filename -> (
          fun () -> main filename
        )
      )
    )

	
let () =
  Core.Command.run command
