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
  | Proj(inter_exp, int);

module TypCtx = Map.Make(String);
type typctx = TypCtx.t(typ);

let matched_arrow_type = (t: option(typ)): option(typ) => t;
let matched_product_type = (t: option(typ)): option(typ) => t;
let consistent = (t1: typ, t2: typ): bool => t1 == t2;

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
    switch (matched_arrow_type(Some(t1))) {
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

  | Deferral => None
  }

and ana = (ctx: typctx, e: exter_exp, t: typ): option(inter_exp) =>
  switch (e) {
  | Fun(x, e1) =>
    switch (matched_arrow_type(Some(t))) {
    | Some(Arrow(t1, t2)) =>
      let+ e1 = ana(TypCtx.add(x, t1, ctx), e1, t2);
      Fun(x, e1);
    | _ => None
    }

  | Tuple(es) =>
    switch (matched_product_type(Some(t))) {
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
  | Deferral =>
    let* (e, t') = syn(ctx, e);
    if (consistent(t, t')) {
      Some(e);
    } else {
      None;
    };
  };

let rec old_syn = (ctx: typctx, e: exter_exp): option(typ) =>
  switch (e) {
  | Var(x) =>
    try(Some(TypCtx.find(x, ctx))) {
    | Not_found => None
    }

  | Fun(_) => None

  | Ap(e1, e2) =>
    switch (matched_arrow_type(old_syn(ctx, e1))) {
    | Some(Arrow(t1, t2)) =>
      if (old_ana(ctx, e2, t1)) {
        Some(t2);
      } else {
        // PARTIAL APPLICATION CODE BEGINS
        // Check to see if `e2` is a tuple with deferrals
        // If it is, this is an instance of partial application
        switch (e2, t1) {
        | (Tuple(es), Product(ts)) =>
          let get_deferred_typ = (e: exter_exp, t: typ): option(option(typ)) =>
            switch (e, old_ana(ctx, e, t)) {
            | (Deferral, _) => Some(Some(t)) // Item was deferred, include it in `deferred_inputs` below
            | (_, true) => Some(None) // Item was not deferred, exclude it from `deferred_inputs` below
            | (_, false) => None // Entire `Ap` expression cannot synthesize
            };

          let+$ deferred_inputs = List.map2(get_deferred_typ, es, ts); // Find the deferred inputs
          let deferred_inputs = filter(deferred_inputs); // Remove the inputs that weren't deferred

          switch (deferred_inputs) {
          | [_, _, ..._] => Arrow(Product(deferred_inputs), t2) // Multiple inputs deferred
          | [hd] => Arrow(hd, t2) // Single input deferred (no `Product` necessary, since it's just one)
          | [] => t2 // Nothing was deferred, so just apply the function as normal
          };
        | _ => None
        // PARTIAL APPLICATION CODE ENDS
        };
      }
    | _ => None
    }

  | Num(_) => Some(Num)

  | Add(e1, e2) =>
    if (old_ana(ctx, e1, Num) && old_ana(ctx, e2, Num)) {
      Some(Num);
    } else {
      None;
    }

  | Ann(e1, t) =>
    if (old_ana(ctx, e1, t)) {
      Some(t);
    } else {
      None;
    }

  | Tuple(es) =>
    let+$ ts = List.map(old_syn(ctx), es);
    Product(ts);

  | Proj(e1, i) =>
    switch (old_syn(ctx, e1)) {
    | Some(Product(ts)) =>
      try(List.nth_opt(ts, i)) {
      | Invalid_argument(_) => None
      }
    | _ => None
    }

  | Deferral => None
  }

and old_ana = (ctx: typctx, e: exter_exp, t: typ): bool =>
  switch (e) {
  | Fun(x, e1) =>
    switch (matched_arrow_type(Some(t))) {
    | Some(Arrow(t1, t2)) => old_ana(TypCtx.add(x, t1, ctx), e1, t2)
    | _ => false
    }

  | Tuple(es) =>
    switch (matched_product_type(Some(t))) {
    | Some(Product(ts)) =>
      try(List.for_all2(old_ana(ctx), es, ts)) {
      | Invalid_argument(_) => false
      }
    | _ => false
    }

  | _ =>
    switch (old_syn(ctx, e)) {
    | Some(t1) => consistent(t1, t)
    | None => false
    }
  };

let elaborate: exter_exp => option(inter_exp) = {
  let rec syn_elaborate = (ctx: typctx, e: exter_exp): option(inter_exp) =>
    switch (e) {
    | Var(x) => Some(Var(x))

    | Fun(_) => None

    | Ap(e1, Tuple(es) as e2) as e =>
      if (List.mem(Deferral, es)) {
        // PARTIAL APPLICATION CODE BEGINS
        let deferral_var_name = "~";

        // Elaborate the original function being applied
        let* e1 = syn_elaborate(ctx, e1);

        // Replace the deferred inputs in the applied expression with the deferral variable
        let* es_deferred = {
          // Generate the expression to replace the Deferrals by index
          let deferral_replacement: int => inter_exp = {
            // True if multiple inputs are deferred, false otherwise
            // Used to determine whether the deferral variable should be a tuple
            let multiple_deferrals =
              es
              |> List.filter((e: exter_exp) => e == Deferral)
              |> List.length > 1;

            (
              (index: int) =>
                if (multiple_deferrals) {
                  // Deferral variable is a tuple, so it needs to be projected
                  Proj(
                    Var(deferral_var_name),
                    index,
                  );
                } else {
                  // Deferral variable is not a tuple, so it can be used directly
                  Var(
                    deferral_var_name,
                  );
                }
            );
          };

          // The function used by List.fold_left
          // Iterates through the applied expression, replacing each Deferral
          let f =
              (acc: option((list(inter_exp), int)), e: exter_exp)
              : option((list(inter_exp), int)) => {
            let* (acc_list, index) = acc;
            let+ (new_hd, new_index) =
              switch (e) {
              | Deferral => Some((deferral_replacement(index), index + 1))
              | _ =>
                let+ e = syn_elaborate(ctx, e);
                (e, index);
              };

            ([new_hd, ...acc_list], new_index);
          };

          let+ (es_deferred_backwards, _) =
            es |> List.fold_left(f, Some(([], 0)));

          // List.fold_left reverses the order of the list, so reverse it again
          List.rev(es_deferred_backwards);
        };

        // We need to annotate the end result of this elaboration with its type since function literals can't synthesize
        let+ t = old_syn(ctx, e);

        Ann(Fun(deferral_var_name, Ap(e1, Tuple(es_deferred))), t);
        // PARTIAL APPLICATION CODE ENDS
      } else {
        let* e1 = syn_elaborate(ctx, e1);
        let+ e2 = syn_elaborate(ctx, e2);
        Ap(e1, e2);
      }

    | Ap(e1, e2) =>
      let* e1 = syn_elaborate(ctx, e1);
      let+ e2 = syn_elaborate(ctx, e2);
      Ap(e1, e2);

    | Num(n) => Some(Num(n))

    | Add(e1, e2) =>
      let* e1 = syn_elaborate(ctx, e1);
      let+ e2 = syn_elaborate(ctx, e2);
      Add(e1, e2);

    | Ann(e1, t) =>
      let+ e1 = ana_elaborate(ctx, e1, t);
      Ann(e1, t);

    | Tuple(es) =>
      let+$ es = List.map(syn_elaborate(ctx), es);
      Tuple(es);

    | Proj(e1, n) =>
      let+ e1 = syn_elaborate(ctx, e1);
      Proj(e1, n);

    | Deferral => None
    }

  and ana_elaborate = (ctx: typctx, e: exter_exp, t: typ): option(inter_exp) => {
    switch (e) {
    | Fun(x, e1) =>
      switch (matched_arrow_type(Some(t))) {
      | Some(Arrow(t1, _)) =>
        let+ e1 = syn_elaborate(TypCtx.add(x, t1, ctx), e1);
        Fun(x, e1);
      | _ => None
      }

    | Tuple(es) =>
      switch (matched_product_type(Some(t))) {
      | Some(Product(ts)) =>
        let* es =
          try(Some(List.map2(ana_elaborate(ctx), es, ts))) {
          | Invalid_argument(_) => None
          };
        let+$ es = es;
        Tuple(es);
      | _ => None
      }

    | _ => syn_elaborate(ctx, e)
    };
  };

  syn_elaborate(TypCtx.empty);
};
