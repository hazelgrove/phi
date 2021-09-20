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

type exp =
  | Var(string)
  | Fun(string, exp)
  | Ap(exp, exp)
  | Num(int)
  | Add(exp, exp)
  | Ann(exp, typ)
  | Tuple(list(exp))
  | Proj(exp, int)
  | Deferral;

module TypCtx = Map.Make(String);
type typctx = TypCtx.t(typ);

module UsedCtx = Set.Make(String);

let matched_arrow_type = (t: option(typ)): option(typ) => t;
let matched_product_type = (t: option(typ)): option(typ) => t;
let consistent = (t1: typ, t2: typ): bool => t1 == t2;

let rec syn = (ctx: typctx, e: exp): option(typ) =>
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
          let get_deferred_typ = (e: exp, t: typ): option(option(typ)) =>
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

and ana = (ctx: typctx, e: exp, t: typ): bool =>
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
let rec elaborate: exp => option(exp) =
  fun
  | Var(x) => Some(Var(x))

  | Fun(x, e1) => {
      let+ e1 = elaborate(e1);
      Fun(x, e1);
    }

  | Ap(_, Tuple(es)) =>
    Some(
      if (List.mem(Deferral, es)) {
        Deferral;
      } else {
        Deferral;
      },
    )

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
