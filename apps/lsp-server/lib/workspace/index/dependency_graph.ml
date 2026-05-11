type edge_kind =
  | ICompoolImport
  | ICopyInclude
  | DefExport
  | RefImport
  | DefineUse
  | TypeUse
  | ProcedureCall
  | TableFieldUse

type edge = {
  source_uri : string;
  target : string;
  target_uri : string option;
  kind : edge_kind;
}

type t = { edges_rev : edge list ref }

let empty () = { edges_rev = ref [] }
let add_edge t edge = t.edges_rev := edge :: !(t.edges_rev)
let edges t = List.rev !(t.edges_rev)

let of_import_kind = function
  | Skeleton_index.Icompool -> ICompoolImport
  | Skeleton_index.Icopy -> ICopyInclude
  | Skeleton_index.DirectExternal -> RefImport

let of_skeleton (sk : Skeleton_index.skeleton_file) : t =
  let t = empty () in
  List.iter
    (fun (imp : Skeleton_index.import) ->
      add_edge t
        {
          source_uri = sk.uri;
          target = imp.name;
          target_uri = None;
          kind = of_import_kind imp.kind;
        })
    sk.imports;
  Skeleton_index.symbols sk
  |> List.iter (fun sym ->
         if sym.Skeleton_index.exported then
           add_edge t
             {
               source_uri = sk.uri;
               target = sym.normalized_name;
               target_uri = None;
               kind = DefExport;
             }
         else if sym.imported then
           add_edge t
             {
               source_uri = sk.uri;
               target = sym.normalized_name;
               target_uri = None;
               kind = RefImport;
             });
  t
