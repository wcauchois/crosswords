/* Based on http://2ality.com/2018/01/iterators-reasonml.html */
type t('a) = unit => option('a);

let ofList = (l: list('a)) : t('a) => {
  let current = ref(l);
  () =>
    switch current^ {
    | [] => None
    | [head, ...tail] =>
      current := tail;
      Some(head);
    };
};

let collect = (g: t('a)) : list('a) => {
  let rec aux = acc =>
    switch (g()) {
    | None => acc
    | Some(x) => aux([x, ...acc])
    };
  List.rev(aux([]));
};

let find = (p: 'a => bool, g: t('a)) : option('a) => {
  let rec aux = () =>
    switch (g()) {
    | Some(x) when p(x) => Some(x)
    | Some(_) => aux()
    | None => None
    };
  aux();
};

let map = (f: 'a => 'b, g: t('a)) : t('b) =>
  () =>
    switch (g()) {
    | Some(x) => Some(f(x))
    | None => None
    };

let takeWhile = (f: 'a => bool, g: t('a)) : t('a) =>
  () =>
    switch (g()) {
    | Some(x) when f(x) => Some(x)
    | _ => None
    };

let last = (g: t('a)) : option('a) => {
  let lastSeen: ref(option('a)) = ref(None);
  let rec aux = () =>
    switch (g()) {
    | Some(x) =>
      lastSeen := Some(x);
      aux();
    | None => lastSeen^
    };
  aux();
};

let rangeStep = (start: int, end_: int, step: int) : t(int) => {
  let i = ref(start);
  if (step == 0) {
    raise(Failure("Step must be nonzero"));
  };
  let cond: unit => bool =
    if (step > 0) {
      () => i^ < end_;
    } else {
      () => i^ > end_;
    };
  () =>
    if (cond()) {
      let ret = Some(i^);
      i := i^ + step;
      ret;
    } else {
      None;
    };
};

let range = (start: int, end_: int) => rangeStep(start, end_, 1);

/*
 ([0, 1], [2, 3]) => [(0, 2), (0, 3), (1, 2), (1, 3)]
 */
let cartesian = (g1: t('a), g2: t('b)) : t(('a, 'b)) => {
  let g2List = collect(g2);
  let g2CurrentList = ref(g2List);
  let g1CurrentVal = ref(g1());
  if (List.length(g2List) > 0) {
    () =>
      switch g1CurrentVal^ {
      | Some(x) =>
        switch g2CurrentList^ {
        | [] => raise(Failure("Should never happen"))
        | [head, ...tail] =>
          let ret = Some((x, head));
          if (List.length(tail) == 0) {
            g2CurrentList := g2List;
            g1CurrentVal := g1();
          } else {
            g2CurrentList := tail;
          };
          ret;
        }
      | None => None
      };
  } else {
    () => None;
  };
};