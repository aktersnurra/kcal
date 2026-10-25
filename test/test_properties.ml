open Hegel

(* Generalizes the hand-picked "patch one field" examples in test_meal_store.ml:
   for any subset of description/calories_kcal/protein_g included in a patch,
   update_meal must change exactly those fields and leave the rest as they were. *)
let%hegel_test meal_patch_touches_only_the_included_fields tc =
  Test_support.with_store (fun store user ->
      let meal = Test_support.create_meal store user in
      let include_description = draw tc (booleans ()) in
      let include_calories = draw tc (booleans ()) in
      let include_protein = draw tc (booleans ()) in
      let new_description = "item " ^ draw tc (text ()) in
      let new_calories = draw tc (integers ~min_value:0 ()) in
      let new_protein =
        draw tc (floats ~min_value:0.0 ~allow_infinity:false ())
      in
      let patch =
        Meal.
          {
            empty_patch with
            description =
              (if include_description then Some new_description else None);
            calories_kcal =
              (if include_calories then Some new_calories else None);
            protein_g = (if include_protein then Some new_protein else None);
          }
      in
      let updated =
        Result.get_ok (Test_support.update_meal_raw store ~user meal.id patch)
      in
      assert (
        updated.description
        = if include_description then new_description else meal.description);
      assert (
        updated.calories_kcal
        = if include_calories then new_calories else meal.calories_kcal);
      assert (
        updated.protein_g
        = if include_protein then new_protein else meal.protein_g);
      (* Fields never included in this patch must never move, no matter what else did. *)
      assert (Option.equal Float.equal updated.carbs_g meal.carbs_g);
      assert (Option.equal Float.equal updated.fat_g meal.fat_g);
      assert (Option.equal Float.equal updated.confidence meal.confidence);
      assert (
        Option.equal String.equal updated.estimate_source meal.estimate_source);
      assert (Option.equal String.equal updated.notes meal.notes))

let () =
  Alcotest.run "properties"
    [
      ( "meal patch",
        [
          Alcotest.test_case "meal patch touches only the included fields"
            `Quick meal_patch_touches_only_the_included_fields;
        ] );
    ]
