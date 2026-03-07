module Lib = Jovial_lsp_lib

let failf fmt = Printf.ksprintf failwith fmt

let expect_bool ~(name:string) (value:bool) : unit =
  if not value then failf "%s: expectation failed" name

let () =
  let init_params =
    `Assoc [
      ( "initializationOptions",
        `Assoc [
          ( "jovial",
            `Assoc [
              ( "workspace",
                `Assoc [
                  "diagnosticsMode", `String "all";
                  "profileMode", `String "large";
                  "rootModel", `String "manual";
                  "manualRootFiles", `List [ `String "main.j73"; `String "ops.j73" ];
                ] );
              ( "background",
                `Assoc [
                  "indexBudgetMs", `Int 21;
                  "diagBatchSize", `Int 9;
                ] );
              ( "server",
                `Assoc [
                  "parseMaxFileBytes", `Int 4096;
                  "pressureSoftMb", `Int 640;
                  "pressureCriticalMb", `Int 896;
                ] );
            ] );
        ] );
    ]
  in
  let overrides = Lib.Lsp_request.parse_client_overrides init_params in
  let ws_settings =
    Lib.Workspace_settings.from_env ()
    |> fun settings ->
    Lib.Workspace_settings.apply_client_overrides settings
      {
        Lib.Workspace_settings.workspace_diag_mode = overrides.workspace_diag_mode;
        workspace_profile_mode = overrides.workspace_profile_mode;
        root_model = overrides.root_model;
        root_manual_files = overrides.root_manual_files;
        parse_file_max_bytes = overrides.parse_file_max_bytes;
        pressure_soft_mb = overrides.pressure_soft_mb;
        pressure_critical_mb = overrides.pressure_critical_mb;
      }
  in
  let runtime_settings =
    Lib.Lsp_runtime_settings.from_env ()
    |> fun settings -> Lib.Lsp_runtime_settings.apply_client_overrides settings overrides
  in
  expect_bool
    ~name:"workspace diagnostics override"
    (ws_settings.workspace_diag_mode = Lib.Workspace_settings.WorkspaceDiagsAll);
  expect_bool
    ~name:"workspace profile override"
    (ws_settings.workspace_profile_mode = Lib.Workspace_settings.ProfileModeLarge);
  expect_bool
    ~name:"root model override"
    (ws_settings.root_model = Lib.Workspace_settings.RootModelManual);
  expect_bool
    ~name:"manual root files override"
    (ws_settings.root_manual_files = [ "main.j73"; "ops.j73" ]);
  expect_bool
    ~name:"parse max file bytes override"
    (ws_settings.parse_file_max_bytes = 4096);
  expect_bool
    ~name:"pressure soft override"
    (ws_settings.pressure_soft_mb = 640);
  expect_bool
    ~name:"pressure critical override"
    (ws_settings.pressure_critical_mb = 896);
  expect_bool
    ~name:"background budget override"
    (runtime_settings.bg_tick_budget_ms = 21);
  expect_bool
    ~name:"background diagnostics batch override"
    (runtime_settings.bg_diag_batch_size = 9);
  print_endline "config_override_merge_test: ok"
