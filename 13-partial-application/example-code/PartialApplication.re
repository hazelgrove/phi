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

module UsedCtx = Set.Make(String);

let matched_arrow_type = (t: option(typ)): option(typ) => t;
let matched_product_type = (t: option(typ)): option(typ) => t;
let consistent = (t1: typ, t2: typ): bool => t1 == t2;

let rec syn = (ctx: typctx, e: exter_exp): option(typ) =>
  switch (e) {
  | Var(x) =>
    try(Some(TypCtx.find(x, ctx))) {
    | Not_found => None
    }

  | Fun(_) => None

  | Ap(e1, e2) =>
    switch (matched_arrow_type(syn(ctx, e1))) {
    | Some(Arrow(t1, t2)) =>
      if (ana(ctx, e2, t1)) {
        Some(t2);
      } else {
        // PARTIAL APPLICATION CODE BEGINS
        // Check to see if `e2` is a tuple with deferrals
        // If it is, this is an instance of partial application
        switch (e2, t1) {
        | (Tuple(es), Product(ts)) =>
          let get_deferred_typ = (e: exter_exp, t: typ): option(option(typ)) =>
            switch (e, ana(ctx, e, t)) {
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
    if (ana(ctx, e1, Num) && ana(ctx, e2, Num)) {
      Some(Num);
    } else {
      None;
    }

  | Ann(e1, t) =>
    if (ana(ctx, e1, t)) {
      Some(t);
    } else {
      None;
    }

  | Tuple(es) =>
    let+$ ts = List.map(syn(ctx), es);
    Product(ts);

  | Proj(e1, i) =>
    switch (syn(ctx, e1)) {
    | Some(Product(ts)) =>
      try(List.nth_opt(ts, i)) {
      | Invalid_argument(_) => None
      }
    | _ => None
    }

  | Deferral => None
  }

and ana = (ctx: typctx, e: exter_exp, t: typ): bool =>
  switch (e) {
  | Fun(x, e1) =>
    switch (matched_arrow_type(Some(t))) {
    | Some(Arrow(t1, t2)) => ana(TypCtx.add(x, t1, ctx), e1, t2)
    | _ => false
    }

  | Tuple(es) =>
    switch (matched_product_type(Some(t))) {
    | Some(Product(ts)) =>
      try(List.for_all2(ana(ctx), es, ts)) {
      | Invalid_argument(_) => false
      }
    | _ => false
    }

  | _ =>
    switch (syn(ctx, e)) {
    | Some(t1) => consistent(t1, t)
    | None => false
    }
  };

// Unfinished (still produces incorrect results)
let rec elaborate: exter_exp => option(inter_exp) =
  fun
  | Var(x) => Some(Var(x))

  | Fun(x, e1) => {
      let+ e1 = elaborate(e1);
      Fun(x, e1);
    }

  | Ap(e1, Tuple(es) as e2) as e =>
    if (List.mem(Deferral, es)) {
      let ctx = TypCtx.empty; // TODO: Make this the actual context

      let deferral_var_name = "~";

      let* e1 = elaborate(e1);

      let* es_deferred = {
        let deferral_replacement = {
          let multiple_deferrals =
            es
            |> List.filter((e: exter_exp) => e == Deferral)
            |> List.length > 1;

          (
            (index: int) =>
              if (multiple_deferrals) {
                Proj(Var(deferral_var_name), index);
              } else {
                Var(deferral_var_name);
              }
          );
        };

        let f =
            (acc: option((list(inter_exp), int)), e: exter_exp)
            : option((list(inter_exp), int)) => {
          let* (acc_list, index) = acc;
          let+ (new_hd, new_index) =
            switch (e) {
            | Deferral => Some((deferral_replacement(index), index + 1))
            | _ =>
              let+ e = elaborate(e);
              (e, index);
            };

          ([new_hd, ...acc_list], new_index);
        };

        let+ (es_deferred_backwards, _) =
          es |> List.fold_left(f, Some(([], 0)));
        List.rev(es_deferred_backwards);
      };

      let+ t = syn(ctx, e);

      Ann(Fun(deferral_var_name, Ap(e1, Tuple(es_deferred))), t);
    } else {
      let* e1 = elaborate(e1);
      let+ e2 = elaborate(e2);
      Ap(e1, e2);
    }

  | Ap(e1, e2) => {
      let* e1 = elaborate(e1);
      let+ e2 = elaborate(e2);
      Ap(e1, e2);
    }

  | Num(n) => Some(Num(n))

  | Add(e1, e2) => {
      let* e1 = elaborate(e1);
      let+ e2 = elaborate(e2);
      Add(e1, e2);
    }

  | Ann(e1, t) => {
      let+ e1 = elaborate(e1);
      Ann(e1, t);
    }

  | Tuple(es) => {
      let+$ es = List.map(elaborate, es);
      Tuple(es);
    }

  | Proj(e1, n) => {
      let+ e1 = elaborate(e1);
      Proj(e1, n);
    }

  | Deferral => None;
