(*
  a utility program for coarsening in time simulation runs.
  compile with
    ocamlfind ocamlopt -thread -linkpkg -package core -package yojson pcoarsify.ml
*)

open Core

type time_frame = { time : float ; data : float array array }

type evolution = { frontmatter : (string * Yojson.Basic.t ) list ; timeframes : time_frame array }

let listtoarray2 listlist = List.to_array ( List.map listlist ~f:( fun x  -> List.to_array x ) ) 

let filterlist2 jsonlistlist = List.map jsonlistlist ~f:( fun x -> Yojson.Basic.Util.filter_float x ) 

let arraytolist2 arrayarray = Array.to_list ( Array.map arrayarray ~f:( fun x  -> Array.to_list x ) ) 

let jsonfloatlist floatlist = List.map floatlist ~f:( fun x -> `Float x ) 

let jsonlist list = `List list 

let jsonlist2float floatlistlist = `List ( List.map floatlistlist ~f:( fun x ->  jsonlist ( jsonfloatlist x ) ) )

let jsonframetoframe frame = 
  let member a = Yojson.Basic.Util.member a frame in 
  let to_float  = Yojson.Basic.Util.to_float in 
  let to_list  = Yojson.Basic.Util.to_list in 
  let filter_list  = Yojson.Basic.Util.filter_list in 
    {
      time = ( member "time" |> to_float )                                                 ;
      data = ( member "data" |> to_list |> filter_list |> filterlist2 |> listtoarray2 ) 
    }

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

let fileexists path =
  let open Core.Unix in
  try
      let _ = path |> stat in
      true
    with
      _ -> false

let coarsifytime evo = 
  let mash tfs tf2 =
    match tfs with
     | [||] -> [| tf2 |]
     | _    ->
       let tf1 = tfs |> Array.last in 
       match Float.robust_sign ( tf2.time -. tf1.time -. 0.04 ) with
        | Sign.Neg  -> tfs
        | Sign.Zero -> Array.append tfs [| tf2 |] 
        | Sign.Pos  -> Array.append tfs [| tf2 |] 
  in
  match evo.timeframes with
   | [||] -> evo
   | x    -> { evo with timeframes = ( Array.fold ~init:[||] ~f:mash x  ) }


let jsonframe frame = 
  let listdata = arraytolist2 frame.data in
    `Assoc
         [ 
          ( "time", `Float frame.time );
          ( "data" , jsonlist2float listdata )
         ]

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

let jsonwritepretty jsondata path = 
  let message = Yojson.Basic.pretty_to_string jsondata in
  let oc = Out_channel.create path in
  let _ = Out_channel.output_string oc message in
  Out_channel.close oc

let pcoarsifytime inputpath =
  let basename = inputpath |> String.chop_suffix_exn ~suffix:".json" in
  let lastind path =
    let rec f path ind = 
      match path ^ "." ^ ( ind |> Int.to_string ) ^ ".json" |> fileexists with
        | false -> ind - 1
        | true  -> f path ( ind + 1 ) 
    in
    f path 1 |> ( fun y -> match y with | 0 -> 1 | x -> x )
  in
  let tfs = basename 
    |> lastind
    |> List.init ~f:( fun x -> basename ^ "." ^ ( Int.to_string ( x + 1 ) ) ^ ".json" ) 
    |> List.map ~f:( fun x -> ( importdataevo x ).timeframes )
    |> List.fold ~init:[||] ~f:Array.append 
  in
  let fevo = inputpath |> importdataevo in
  let _ = tfs |> Array.length |> Int.to_string |> print_endline in
  let outname = basename |> String.split ~on:'/' |> List.last_exn in
  let _ = outname |> print_endline in
  let outevo = { fevo with timeframes = tfs } |> coarsifytime in
  jsonwritepretty ( outevo |> jsonextra ) ( "/media/adam/8tbraid/science/ha/data2_100/0.04/" ^ outname ^ ".1.json" )

let filename_param =
  let open Command.Param in
  anon ("filename" %: string)

let command =
  Command.basic
    ~summary:"coarsify in time to 0.04"
    (
      Command.Param.map filename_param ~f:(
        fun filename -> (
          fun () -> pcoarsifytime filename
        )
      )
    )

	
let () =
  Core.Command.run command
