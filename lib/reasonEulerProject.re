let p13modular = data => {
  let generate = dataStr => {
    List.map(str => Z.of_string(str), String.split_on_char('\n', dataStr));
  };

  let count = zList => {
    List.fold_left(Z.(+), Z.zero, zList);
  };

  String.sub(Z.to_string(count(generate(data))), 0, 10);
};

let p18modular = data => {
  let generate = dataStr => {
    List.map(
      strList =>
        List.map(int_of_string, String.split_on_char(' ', strList ++ " 0")),
      String.split_on_char('\n', dataStr),
    );
  };

  let count = intListList => {
    let maxPairs = ys => List.map2(max, ys, List.tl(ys) @ [0]);
    let rec firstN = (lst, n) => {
      switch (lst) {
      | [] => []
      | [hd, ...tl] =>
        if (n == 1) {
          [hd];
        } else {
          [hd, ...firstN(tl, n - 1)];
        }
      };
    };
    let f = (s, rs) => {
      List.map2((+), maxPairs(firstN(rs, List.length(s))), s);
    };
    let zeroes = Seq.take(List.length(intListList) + 1, Seq.forever(_ => 0));
    List.hd(List.fold_right(f, intListList, List.of_seq(zeroes)));
  };

  count(generate(data));
};
