'use strict';

const getTag = (fa) => {
  if (Array.isArray(fa)) {
    return fa[0];
  } else {
    return fa;
  }
};

// Fold for ADT
const List_fold = (fa, matcher) => {
  const tag = getTag(fa);
  switch (tag) {
    case 'Nil': return matcher.onNil();
    case 'Cons': return matcher.onCons(fa[1], fa[2]);
    default: return matcher.onDefault();
  }
};

const sum = (list) => {
  return List_fold(list,
    {
      'onNil': () => {return 0;},
      'onCons': (x, xs) => {return x + sum(xs)}
    }
  );
};

// Fold for Record
const Person_fold = (fa, on) => {
  return on(fa.name, fa.age);
};

// Fold for Tuple
const Tuple2_fold = (fa, on) => {
  return on(fa[0], fa[1]);
};
