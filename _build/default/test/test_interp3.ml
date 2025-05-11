open Interp3
open OUnit2

let parser_tests =
  "parser test suite" >::: [
    "parse unit" >:: (fun _ ->
      let prog = Option.get (parse "let x = ()") in
      let expected = [{ is_rec = false; name = "x"; binding = Interp3.Unit }] in
      assert_equal expected prog
    );

    "parse bool true" >:: (fun _ ->
      let prog = Option.get (parse "let x = true") in
      let expected = [{ is_rec = false; name = "x"; binding = Interp3.Bool true }] in
      assert_equal expected prog
    );

    "parse int" >:: (fun _ ->
      let prog = Option.get (parse "let x = 42") in
      let expected = [{ is_rec = false; name = "x"; binding = Interp3.Int 42 }] in
      assert_equal expected prog
    );

    "parse float" >:: (fun _ ->
      let prog = Option.get (parse "let x = 3.14") in
      let expected = [{ is_rec = false; name = "x"; binding = Interp3.Float 3.14 }] in
      assert_equal expected prog
    );
  ]

let principle_type_tests =
  "principle_type test suite" >::: [
    "unify type variable with TInt" >:: (fun _ ->
      let ty = TVar "a" in
      let cs = [(TVar "a", TInt)] in
      let expected = Some (Forall (VarSet.empty, TInt)) in
      assert_equal expected (principle_type ty cs)
    );

    "empty constraints with type variable" >:: (fun _ ->
      let ty = TVar "a" in
      let cs = [] in
      let expected = Some (Forall (VarSet.add "a" VarSet.empty, TVar "a")) in
      assert_equal expected (principle_type ty cs)
    );

    "unify TList (TVar a) with TList TInt" >:: (fun _ ->
      let ty = TList (TVar "a") in
      let cs = [(TVar "a", TInt)] in
      let expected = Some (Forall (VarSet.empty, TList TInt)) in
      assert_equal expected (principle_type ty cs)
    );

    "unify TPair (TVar a, TVar b) with TPair (TInt, TBool)" >:: (fun _ ->
      let ty = TPair (TVar "a", TVar "b") in
      let cs = [(TVar "a", TInt); (TVar "b", TBool)] in
      let expected = Some (Forall (VarSet.empty, TPair (TInt, TBool))) in
      assert_equal expected (principle_type ty cs)
    );

    "unify TFun (TVar a, TVar b) with TFun (TInt, TBool)" >:: (fun _ ->
      let ty = TFun (TVar "a", TVar "b") in
      let cs = [(TVar "a", TInt); (TVar "b", TBool)] in
      let expected = Some (Forall (VarSet.empty, TFun (TInt, TBool))) in
      assert_equal expected (principle_type ty cs)
    );

    "unification failure TInt vs TBool" >:: (fun _ ->
      let ty = TInt in
      let cs = [(TInt, TBool)] in
      let expected = None in
      assert_equal expected (principle_type ty cs)
    );

    "complex type with partial unification" >:: (fun _ ->
      let ty = TFun (TVar "a", TList (TVar "b")) in
      let cs = [(TVar "a", TInt)] in
      let expected = Some (Forall (VarSet.add "b" VarSet.empty, TFun (TInt, TList (TVar "b")))) in
      assert_equal expected (principle_type ty cs)
    );

    "occurs check failure" >:: (fun _ ->
      let ty = TVar "a" in
      let cs = [(TVar "a", TList (TVar "a"))] in
      let expected = None in
      assert_equal expected (principle_type ty cs)
    );
  ]

let tests =
  "interp3 test suite" >::: [
    parser_tests;
    principle_type_tests;
  ]

let _ = run_test_tt_main tests