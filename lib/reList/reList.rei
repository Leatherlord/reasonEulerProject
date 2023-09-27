let length: list('a) => int;
let nth: (list('a), int) => 'a;
let map: ('a => 'b, list('a)) => list('b);
let map2: (('a, 'b) => 'c, list('a), list('b)) => list('c);
let foldLeft: (('a, 'b) => 'a, 'a, list('b)) => 'a;
let foldRight: (('a, 'b) => 'b, list('a), 'b) => 'b;
let hd: list('a) => 'a;
let tl: list('a) => list('a);
let ofSeq: Seq.t('a) => list('a);
