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

let range = (start: int, end_: int, ~step: option(int)=?) : t(int) => {
  let i = ref(start);
  let stepOrDefault = switch (step) {
  | Some(s) => s
  | None => 1
  };
  if (stepOrDefault == 0) {
    raise(Failure("Step must be nonzero"))
  };
  let cond: unit => bool = if (stepOrDefault > 0) {
    () => i^ < end_;
  } else {
    () => i^ > end_;
  };
  () => {
    if (cond()) {
      Some(i^)
    } else {
      None
    }
  }
};
