// Maybe Monad
let ( let* ) = (x: option('a), f: 'a => option('b)): option('b) =>
  switch (x) {
  | Some(x) => f(x)
  | None => None
  };

// Maybe Monad
let (let+) = (x: option('a), f: 'a => 'b): option('b) => {
  let* x = x;
  Some(f(x));
};

// Maybe Monad, but for lists (I guess?)
let (let*$) =
    (xs: list(option('a)), f: list('a) => option('b)): option('b) => {
  let rec helper: list(option('a)) => option(list('a)) =
    fun
    | [Some(hd), ...tl] => {
        let+ tl = helper(tl);
        [hd, ...tl];
      }
    | [None, ..._] => None
    | [] => Some([]);

  let* xs = helper(xs);
  f(xs);
};

// Maybe Monad, but for lists (I guess?)
let (let+$) = (xs: list(option('a)), f: list('a) => 'b): option('b) => {
  let*$ xs = xs;
  Some(f(xs));
};

// Remove all None elements from the list
let rec filter: list(option('a)) => list('a) =
  fun
  | [Some(hd), ...tl] => [hd, ...filter(tl)]
  | [None, ...tl] => filter(tl)
  | [] => [];

// Types
type typ =
  | Arrow(typ, typ)
  | Num
  | Hole
  | Product(list(typ));

// External Expressions
type exter_exp =
  | Var(string)
  | Fun(string, exter_exp)
  | Ap(exter_exp, exter_exp)
  | Num(int)
  | Add(exter_exp, exter_exp)
  | Ann(exter_exp, typ)
  | Tuple(list(exter_exp))
  | Proj(exter_exp, int)
  | EmptyHole
  | FilledHole(exter_exp)
  | Deferral;

// Internal Expressions
type inter_exp =
  | Var(string)
  | Fun(string, inter_exp)
  | Ap(inter_exp, inter_exp)
  | Num(int)
  | Add(inter_exp, inter_exp)
  | Ann(inter_exp, typ)
  | Tuple(list(inter_exp))
  | Proj(inter_exp, int)
  | EmptyHole
  | FilledHole(inter_exp);

module TypCtx = Map.Make(String);
type typctx = TypCtx.t(typ);

let matched_arrow_type: typ => option(typ) =
  fun
  | Hole => Some(Arrow(Hole, Hole))
  | Arrow(t1, t2) => Some(Arrow(t1, t2))
  | _ => None;

let matched_product_type = (n: int, t: typ): option(typ) => {
  let rec repeat = (n: int, x: 'a): list('a) =>
    if (n > 0) {
      [x, ...repeat(n - 1, x)];
    } else {
      [];
    };

  switch (t) {
  | Hole => Some(Product(repeat(n, Hole)))
  | Product(ts) =>
    if (List.compare_length_with(ts, n) == 0) {
      Some(Product(ts));
    } else {
      None;
    }
  | _ => None
  };
};

let rec consistent = (t1: typ, t2: typ): bool =>
  switch (t1, t2) {
  | (Hole, _)
  | (_, Hole) => true
  | (Arrow(t1, t2), Arrow(t1', t2')) =>
    consistent(t1, t2) && consistent(t1', t2')
  | (Product(ts), Product(ts')) =>
    try(List.for_all2(consistent, ts, ts')) {
    | Invalid_argument(_) => false
    }
  | (t1, t2) => t1 == t2
  };

let rec syn = (ctx: typctx, e: exter_exp): option((inter_exp, typ)) =>
  switch (e) {
  | Var(x) =>
    let+ t =
      try(Some(TypCtx.find(x, ctx))) {
      | Not_found => None
      };
    (Var(x), t);

  | Fun(_) => None

  | Ap(e1, e2) =>
    let* (e1, t1) = syn(ctx, e1);
    switch (matched_arrow_type(t1)) {
    | Some(Arrow(t2, t)) =>
      switch (ana(ctx, e2, t2)) {
      | Some(e2) => Some((Ap(e1, e2), t))
      | None =>
        // PARTIAL APPLICATION CODE BEGINS
        // Check to see if `e2` is a tuple with deferrals
        // If it is, this is an instance of partial application
        switch (e2, t2) {
        | (Tuple(es), Product(ts)) =>
          let* final_t = {
            let get_deferred_typ =
                (e: exter_exp, t: typ): option(option(typ)) =>
              switch (e, ana(ctx, e, t)) {
              | (Deferral, _) => Some(Some(t)) // Item was deferred, include it in `deferred_inputs` below
              | (_, Some(_)) => Some(None) // Item was not deferred, exclude it from `deferred_inputs` below
              | (_, None) => None // Entire `Ap` expression cannot synthesize
              };

            let* deferred_inputs =
              try(Some(List.map2(get_deferred_typ, es, ts))) {
              // Find the deferred inputs
              | Invalid_argument(_) => None
              };
            let+$ deferred_inputs = deferred_inputs; // Make sure the entire expression can synthesize
            let deferred_inputs = filter(deferred_inputs); // Remove the inputs that weren't deferred

            switch (deferred_inputs) {
            | [_, _, ..._] => Arrow(Product(deferred_inputs), t) // Multiple inputs deferred
            | [hd] => Arrow(hd, t) // Single input deferred (no `Product` necessary, since it's just one)
            | [] => t2 // Nothing was deferred, so just apply the function as normal
            };
          };

          let+ final_e = {
            let deferral_var_name = "~";

            // Replace the deferred inputs in the applied expression with the deferral variable
            let* es_deferred = {
              let+ (es_deferred_backwards, _) = {
                // Generate the expression to replace the Deferrals by index
                let deferral_replacement: int => inter_exp = {
                  // True if multiple inputs are deferred, false otherwise
                  // Used to determine whether the deferral variable should be a tuple
                  let multiple_deferrals =
                    es
                    |> List.filter((e: exter_exp) => (e == Deferral: bool))
                    |> List.length > 1;

                  if (multiple_deferrals) {
                    (
                      // Deferral variable is a tuple, so it needs to be projected
                      (index: int) => (
                        Proj(Var(deferral_var_name), index): inter_exp
                      )
                    );
                  } else {
                    (
                      // Deferral variable is not a tuple, so it can be used directly
                      (_: int) => (
                        Var(deferral_var_name): inter_exp
                      )
                    );
                  };
                };

                // The function used by List.fold_left2
                // Iterates through the applied expression, replacing each Deferral
                let f =
                    (
                      acc: option((list(inter_exp), int)),
                      e: exter_exp,
                      t: typ,
                    )
                    : option((list(inter_exp), int)) => {
                  let* (acc_list, index) = acc;
                  let+ (new_hd, new_index) =
                    switch (e) {
                    | Deferral =>
                      Some((deferral_replacement(index), index + 1))
                    | _ =>
                      let _ = ts;
                      let+ e = ana(ctx, e, t);
                      (e, index);
                    };

                  ([new_hd, ...acc_list], new_index);
                };

                try(List.fold_left2(f, Some(([], 0)), es, ts)) {
                | Invalid_argument(_) => None
                };
              };

              // List.fold_left reverses the order of the list, so reverse it again
              List.rev(es_deferred_backwards);
            };

            Some(
              Ann(
                Fun(deferral_var_name, Ap(e1, Tuple(es_deferred))),
                final_t,
              ),
            );
          };

          (final_e, final_t);
        | _ => None
        }
      // PARTIAL APPLICATION CODE ENDS
      }

    | _ => None
    };

  | Num(n) => Some((Num(n), Num))

  | Add(e1, e2) =>
    let num: typ = Num;
    let* e1 = ana(ctx, e1, num);
    let+ e2 = ana(ctx, e2, num);
    (Add(e1, e2), num);

  | Ann(e1, t1) =>
    let+ e1 = ana(ctx, e1, t1);
    (Ann(e1, t1), t1);

  | Tuple(es) =>
    let+$ es_ts = List.map(syn(ctx), es);
    let (es, ts) = List.split(es_ts);
    (Tuple(es), Product(ts));

  | Proj(e1, i) =>
    let* (e1, t1) = syn(ctx, e1);
    switch (t1) {
    | Product(ts) =>
      let+ t =
        try(List.nth_opt(ts, i)) {
        | Invalid_argument(_) => None
        };
      (Proj(e1, i), t);
    | _ => None
    };

  | EmptyHole => Some((EmptyHole, Hole))

  | FilledHole(e1) =>
    let+ (e1, _) = syn(ctx, e1);
    (FilledHole(e1), Hole);

  | Deferral => None
  }

and ana = (ctx: typctx, e: exter_exp, t: typ): option(inter_exp) =>
  switch (e) {
  | Fun(x, e1) =>
    switch (matched_arrow_type(t)) {
    | Some(Arrow(t1, t2)) =>
      let+ e1 = ana(TypCtx.add(x, t1, ctx), e1, t2);
      Fun(x, e1);
    | _ => None
    }

  | Tuple(es) =>
    switch (matched_product_type(List.length(es), t)) {
    | Some(Product(ts)) =>
      let* es =
        try(Some(List.map2(ana(ctx), es, ts))) {
        | Invalid_argument(_) => None
        };
      let+$ es = es;
      Tuple(es);
    | _ => None
    }

  | Var(_)
  | Ap(_)
  | Num(_)
  | Add(_)
  | Ann(_)
  | Proj(_)
  | EmptyHole
  | FilledHole(_)
  | Deferral =>
    let* (e, t') = syn(ctx, e);
    if (consistent(t, t')) {
      Some(e);
    } else {
      None;
    };
  };

// Printing Utilities

let rec unelaborate: inter_exp => exter_exp =
  fun
  | Var(x) => Var(x)
  | Fun(x, e) => Fun(x, unelaborate(e))
  | Ap(e1, e2) => Ap(unelaborate(e1), unelaborate(e2))
  | Num(n) => Num(n)
  | Add(e1, e2) => Add(unelaborate(e1), unelaborate(e2))
  | Ann(e, t) => Ann(unelaborate(e), t)
  | Tuple(es) => Tuple(List.map(unelaborate, es))
  | Proj(e, i) => Proj(unelaborate(e), i)
  | EmptyHole => EmptyHole
  | FilledHole(e) => FilledHole(unelaborate(e));

type associativity =
  | Left
  | Right
  | Atom;

let associativity_of_typ = (t: typ): associativity =>
  switch (t) {
  | Arrow(_) => Right
  | Num => Atom
  | Product(_) => Atom
  | Hole => Atom
  };

let associativity_of_exp = (e: exter_exp): associativity =>
  switch (e) {
  | Var(_) => Atom
  | Fun(_) => Atom
  | Ap(_) => Left
  | Num(_) => Atom
  | Add(_) => Left
  | Ann(_) => Atom
  | Tuple(_) => Atom
  | Proj(_) => Atom
  | EmptyHole => Atom
  | FilledHole(_) => Atom
  | Deferral => Atom
  };

let parenthesize = (s: string): string => "(" ++ s ++ ")";

let paren_if =
    (
      string_of: 'a => string,
      associativity_of: 'a => associativity,
      side: associativity,
      a: 'a,
    )
    : string => {
  let s = string_of(a);
  if (side == associativity_of(a)) {
    parenthesize(s);
  } else {
    s;
  };
};

let rec string_of_typ = (t: typ): string => {
  let paren_if = paren_if(string_of_typ, associativity_of_typ);

  switch (t) {
  | Arrow(t1, t2) => paren_if(Right, t1) ++ " -> " ++ paren_if(Left, t2)
  | Num => "Num"
  | Product(ts) =>
    let rec string_of_product = (ts: list(typ)): string =>
      switch (ts) {
      | [hd, ...[_, ..._] as tl] =>
        string_of_typ(hd) ++ ", " ++ string_of_product(tl)
      | [hd] => string_of_typ(hd)
      | [] => ""
      };
    ts |> string_of_product |> parenthesize;
  | Hole => "?"
  };
};

let rec string_of_exp = (e: exter_exp): string => {
  let paren_recurse = (e: exter_exp): string =>
    e |> string_of_exp |> parenthesize;
  let paren_if = paren_if(string_of_exp, associativity_of_exp);

  switch (e) {
  | Var(x) => x
  | Fun(x, e1) => parenthesize("fun " ++ x ++ " -> " ++ string_of_exp(e1))
  | Ap(e1, e2) => paren_if(Right, e1) ++ " " ++ paren_if(Left, e2)
  | Num(n) => string_of_int(n)
  | Add(e1, e2) => paren_if(Right, e1) ++ " + " ++ paren_if(Left, e2)
  | Ann(e1, t) =>
    parenthesize(paren_recurse(e1) ++ ": " ++ string_of_typ(t))
  | Tuple(es) =>
    let rec string_of_tuple = (ts: list(exter_exp)): string =>
      switch (ts) {
      | [hd, ...[_, ..._] as tl] =>
        string_of_exp(hd) ++ ", " ++ string_of_tuple(tl)
      | [hd] => string_of_exp(hd)
      | [] => ""
      };
    es |> string_of_tuple |> parenthesize;
  | Proj(e1, n) => paren_recurse(e1) ++ "." ++ string_of_int(n)
  | EmptyHole => "?"
  | FilledHole(e1) => "?(" ++ string_of_exp(e1) ++ ")"
  | Deferral => "~"
  };
};

// let string_syn = (e: exter_exp): string => {
//   switch (
//     {
//       let+ (_, t) = syn(TypCtx.empty, e);
//       t;
//     }
//   ) {
//   | Some(t) => string_of_typ(t)
//   | None => "Synthesis Failed"
//   };
// };

let example_exp = (e: exter_exp): exter_exp =>
  Ap(
    Ann(
      Fun(
        "x",
        Add(
          Add(Proj(Var("x"), 0), Proj(Var("x"), 1)),
          Proj(Var("x"), 2),
        ),
      ),
      Arrow(Product([Num, Num, Num]), Num),
    ),
    e,
  );

let test_expressions: list((exter_exp, exter_exp)) = [
  // Num(42),
  // Ann(Fun("x", Add(Var("x"), Var("x"))), Arrow(Num, Num)),
  // Ap(Ann(Fun("x", Add(Var("x"), Var("x"))), Arrow(Num, Num)), Num(42)),
  // Tuple([Num(1), Num(2), Num(3)]),
  // Ann(Fun("x", Num(42)), Arrow(Product([Num, Num]), Num)),
  // Ann(
  //   Tuple([Num(1), Fun("x", Var("x")), Num(3)]),
  //   Product([Num, Arrow(Num, Num), Num]),
  // ),
  (
    example_exp(Tuple([Num(0), Num(1), Num(2)])),
    example_exp(Tuple([Num(0), Num(1), Num(2)])),
  ),
  (
    example_exp(Tuple([Num(0), Deferral, Num(2)])),
    Ann(
      Fun("~", example_exp(Tuple([Num(0), Var("~"), Num(2)]))),
      Arrow(Num, Num),
    ),
  ),
  (
    example_exp(Tuple([Deferral, Num(1), Deferral])),
    Ann(
      Fun(
        "~",
        example_exp(
          Tuple([Proj(Var("~"), 0), Num(1), Proj(Var("~"), 1)]),
        ),
      ),
      Arrow(Product([Num, Num]), Num),
    ),
  ),
  (
    example_exp(Tuple([Deferral, Deferral, Deferral])),
    Ann(
      Fun(
        "~",
        example_exp(
          Tuple([
            Proj(Var("~"), 0),
            Proj(Var("~"), 1),
            Proj(Var("~"), 2),
          ]),
        ),
      ),
      Arrow(Product([Num, Num, Num]), Num),
    ),
  ),
  // Proj(
  //   Tuple([
  //     Num(1),
  //     Tuple([Num(2), Num(3)]),
  //     Tuple([Num(4), Num(5), Num(6)]),
  //   ]),
  //   2,
  // ),
  // Ap(
  //   Ap(
  //     Ann(
  //       Fun("f", Fun("x", Ap(Var("f"), Var("x")))),
  //       Arrow(
  //         Arrow(Product([Num, Num, Num]), Num),
  //         Arrow(Product([Num, Num, Num]), Num),
  //       ),
  //     ),
  //     Fun(
  //       "y",
  //       Add(
  //         Add(Proj(Var("y"), 0), Proj(Var("y"), 1)),
  //         Proj(Var("y"), 2),
  //       ),
  //     ),
  //   ),
  //   Tuple([Num(10), Num(20), Num(30)]),
  // ),
  // Ap(
  //   Ap(
  //     Ann(
  //       Fun("f", Fun("x", Ap(Var("f"), Var("x")))),
  //       Arrow(
  //         Arrow(Product([Num, Num, Num]), Num),
  //         Arrow(Product([Num, Num, Num]), Num),
  //       ),
  //     ),
  //     Fun(
  //       "y",
  //       Add(
  //         Add(Proj(Var("y"), 0), Proj(Var("y"), 1)),
  //         Proj(Var("y"), 2),
  //       ),
  //     ),
  //   ),
  //   Tuple([Deferral, Deferral, Num(30)]),
  // ),
];

// No Imperativeness Above This Line

let test_expression = ((e: exter_exp, expected_elaboration: exter_exp)): unit => {
  let synthesized = syn(TypCtx.empty, e);

  print_endline("Expression:           " ++ string_of_exp(e));

  print_endline(
    "Actual Elaboration:   "
    ++ (
      switch (synthesized) {
      | Some((e, _)) => string_of_exp(unelaborate(e))
      | None => "Elaboration Failed"
      }
    ),
  );

  print_endline(
    "Expected Elaboration: " ++ string_of_exp(expected_elaboration),
  );

  print_endline(
    "Synthesis:            "
    ++ (
      switch (synthesized) {
      | Some((_, t)) => string_of_typ(t)
      | None => "Synthesis Failed"
      }
    ),
  );

  print_endline("");
};

List.iter(test_expression, test_expressions);

// Arrow(Product([Num, Num]), Num) |> string_of_typ |> print_endline

// print_endline("---");

// Arrow(Num, Num) |> string_of_typ |> print_endline;
// Arrow(Num, Arrow(Num, Num)) |> string_of_typ |> print_endline;
// Arrow(Arrow(Num, Num), Arrow(Num, Num)) |> string_of_typ |> print_endline;
// Product([]) |> string_of_typ |> print_endline;
// Product([Num]) |> string_of_typ |> print_endline;
// Product([Num, Num]) |> string_of_typ |> print_endline;
// Product([Num, Num, Num]) |> string_of_typ |> print_endline;
// Product([Num, Arrow(Num, Num), Num]) |> string_of_typ |> print_endline;
