let now = Option.get (Ptime.of_float_s 1_800_000_000.)
let key = Bytes.make 32 'k'

let measurement ?(value = 80000L) ?(date = 1_700_000_000L) group_id =
  Withings.{ group_id; measured_at = date; measure_type = 1; value; unit_ = -3 }

let setup measurements =
  let store, user = Test_support.store_with_user () in
  ignore (Result.get_ok (Store_sqlite.create_withings_connection store ~user ~withings_user_id:"upstream-alice"));
  let credentials = Withings.{ access_token = "access"; refresh_token = "refresh"; expires_at = Option.get (Ptime.add_span now (Ptime.Span.of_int_s 3600)); withings_user_id = "upstream-alice" } in
  let connection = Result.get_ok (Store_sqlite.save_withings_credentials store ~user ~key credentials) in
  let module Client = struct
    let exchange_code ~code:_ = Error (Error.Invalid_input "unused")
    let refresh ~refresh_token:_ = Error (Error.Invalid_input "unused")
    let get_measurements ~access_token:_ ~lastupdate:_ = Ok Withings.{ measurements; lastupdate = Some 77L }
    let subscribe ~access_token:_ ~callback_url:_ = Ok ()
  end in
  (store, user, connection, (module Client : Withings.S))

let sync_cursor store =
  let statement = Sqlite3.prepare store "SELECT sync_cursor FROM withings_connections" in
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.finalize statement)) (fun () ->
    match Sqlite3.step statement with
    | Sqlite3.Rc.ROW -> if Sqlite3.column_is_null statement 0 then None else Some (Sqlite3.column_int64 statement 0)
    | _ -> None)

let test_normalizes_deduplicates_and_advances_cursor () =
  let store, user, connection, client = setup [ measurement "one"; measurement "one"; measurement ~value:75000L "two" ] in
  Result.get_ok (Withings_sync.sync (Withings_sync.make ~store ~client ~token_key:key ~now:(fun () -> now)) ~user ~connection);
  let rows = Result.get_ok (Store_sqlite.query_weigh_ins store ~user ~from:None ~to_:None ~limit:10) in
  Alcotest.(check int) "deduplicated" 2 (List.length rows);
  Alcotest.(check (float 0.0001)) "kg normalization" 80.0 (List.hd rows).weight_kg;
  Alcotest.(check string) "imported provenance" "withings"
    (Weigh_in.source_to_string (List.hd rows).source);
  Alcotest.(check (option int64)) "cursor advanced" (Some 77L) (sync_cursor store)

let test_upstream_change_preserves_timestamp_and_tombstone () =
  let store, user, connection, client = setup [ measurement "one" ] in
  let sync measurements =
    let module Client = (val client : Withings.S) in
    ignore Client.get_measurements;
    let module Changed = struct
      include Client
      let get_measurements ~access_token:_ ~lastupdate:_ = Ok Withings.{ measurements; lastupdate = Some 80L }
    end in
    Withings_sync.sync (Withings_sync.make ~store ~client:(module Changed) ~token_key:key ~now:(fun () -> now)) ~user ~connection
  in
  Result.get_ok (sync [ measurement "one" ]);
  let original = List.hd (Result.get_ok (Store_sqlite.query_weigh_ins store ~user ~from:None ~to_:None ~limit:10)) in
  Result.get_ok (sync [ measurement ~value:81000L ~date:1_700_000_100L "one" ]);
  let changed = List.hd (Result.get_ok (Store_sqlite.query_weigh_ins store ~user ~from:None ~to_:None ~limit:10)) in
  Alcotest.(check (float 0.0001)) "upstream value updated" 81.0 changed.weight_kg;
  Alcotest.(check string) "timestamp preserved" (Time.to_utc_string original.measured_at) (Time.to_utc_string changed.measured_at);
  Result.get_ok (Store_sqlite.delete_weigh_in store ~user changed.id);
  Result.get_ok (sync [ measurement ~value:82000L "one" ]);
  Alcotest.(check int) "tombstone retained" 0 (List.length (Result.get_ok (Store_sqlite.query_weigh_ins store ~user ~from:None ~to_:None ~limit:10)))

let test_failed_transaction_retains_cursor () =
  let store, user, connection, _ = setup [] in
  ignore (Sqlite3.exec store "CREATE TRIGGER reject_import BEFORE INSERT ON weigh_ins WHEN NEW.source = 'withings' BEGIN SELECT RAISE(ABORT, 'reject'); END");
  Alcotest.(check bool) "transaction fails" true
    (Result.is_error (Store_sqlite.persist_withings_import store ~user ~connection
      ~rows:[ Weigh_in.{ external_id = "upstream-alice:one"; measured_at = now; weight_kg = 80.0 } ] ~cursor:(Some 99L)));
  Alcotest.(check (option int64)) "cursor retained" None (sync_cursor store)

let test_stale_batch_cannot_regress_value_or_cursor () =
  let store, user, connection, _ = setup [] in
  let current = Weigh_in.{ external_id = "upstream-alice:one"; measured_at = now; weight_kg = 80.0 } in
  Result.get_ok (Store_sqlite.persist_withings_import store ~user ~connection ~rows:[ current ] ~cursor:(Some 100L));
  let stale = Weigh_in.{ current with weight_kg = 70.0 } in
  Result.get_ok (Store_sqlite.persist_withings_import store ~user ~connection ~rows:[ stale ] ~cursor:(Some 99L));
  let weight = List.hd (Result.get_ok (Store_sqlite.query_weigh_ins store ~user ~from:None ~to_:None ~limit:10)) in
  Alcotest.(check (float 0.0001)) "value is not regressed" 80.0 weight.weight_kg;
  Alcotest.(check (option int64)) "cursor is not regressed" (Some 100L) (sync_cursor store)

let test_refresh_failure_requires_reauthorization () =
  let store, user, connection, client = setup [ measurement "one" ] in
  let module Fake = struct
    include (val client : Withings.S)
    let refresh ~refresh_token:_ = Error (Error.Invalid_input "Withings authorization failure")
  end in
  ignore (Withings_sync.sync (Withings_sync.make ~store ~client:(module Fake) ~token_key:key ~now:(fun () -> Ptime.add_span now (Ptime.Span.of_int_s 7200) |> Option.get)) ~user ~connection);
  let Withings_connection.Connected status = Result.get_ok (Store_sqlite.get_withings_status store ~user) in
  Alcotest.(check bool) "reauthorization" true status.requires_reauthorization

let test_transient_refresh_failure_does_not_require_reauthorization () =
  let store, user, connection, client = setup [ measurement "one" ] in
  let module Fake = struct
    include (val client : Withings.S)
    let refresh ~refresh_token:_ = Error (Error.Invalid_input "Withings request failed")
  end in
  Alcotest.(check bool) "transient refresh fails" true
    (Result.is_error (Withings_sync.sync (Withings_sync.make ~store ~client:(module Fake) ~token_key:key
      ~now:(fun () -> Ptime.add_span now (Ptime.Span.of_int_s 7200) |> Option.get)) ~user ~connection));
  let Withings_connection.Connected status = Result.get_ok (Store_sqlite.get_withings_status store ~user) in
  Alcotest.(check bool) "transient failure retains authorization" false status.requires_reauthorization

let () = Alcotest.run "withings sync"
  [ ("sync", [ Alcotest.test_case "normalization, duplicate and cursor" `Quick test_normalizes_deduplicates_and_advances_cursor;
                Alcotest.test_case "update and tombstone" `Quick test_upstream_change_preserves_timestamp_and_tombstone;
                Alcotest.test_case "failed transaction retains cursor" `Quick test_failed_transaction_retains_cursor;
                Alcotest.test_case "stale batch cannot regress" `Quick test_stale_batch_cannot_regress_value_or_cursor;
                Alcotest.test_case "permanent refresh failure" `Quick test_refresh_failure_requires_reauthorization;
                Alcotest.test_case "transient refresh failure" `Quick test_transient_refresh_failure_does_not_require_reauthorization ]) ]
