let p13monolithRecursive = dataStr => {
  let rec strListToZList = strList =>
    switch (strList) {
    | [] => []
    | [hd, ...tl] => [Z.of_string(hd), ...strListToZList(tl)]
    };

  let rec sumZ = zList =>
    switch (zList) {
    | [] => Z.zero
    | [hd, ...tl] => Z.(+)(hd, sumZ(tl))
    };

  let result =
    String.sub(
      Z.to_string(
        sumZ(strListToZList(String.split_on_char('\n', dataStr))),
      ),
      0,
      10,
    );
  result;
};

let p13monolithTailRecursive = dataStr => {
  let strListToZList = strList => {
    let rec strListToZListInner = (strList, acc) =>
      switch (strList) {
      | [] => acc
      | [hd, ...tl] => strListToZListInner(tl, [Z.of_string(hd), ...acc])
      };
    strListToZListInner(strList, []);
  };

  let sumZ = zList => {
    let rec sumZInner = (zList, acc) =>
      switch (zList) {
      | [] => acc
      | [hd, ...tl] => sumZInner(tl, Z.(+)(acc, hd))
      };

    sumZInner(zList, Z.zero);
  };

  let result =
    String.sub(
      Z.to_string(
        sumZ(strListToZList(String.split_on_char('\n', dataStr))),
      ),
      0,
      10,
    );
  result;
};

let p18monolithRecursive = dataStr => {
  let strToIntList = str => {
    let rec strListToIntList = strList =>
      switch (strList) {
      | [] => []
      | [hd, ...tl] => [int_of_string(hd), ...strListToIntList(tl)]
      };
    strListToIntList(String.split_on_char(' ', str));
  };

  let rec strListToIntListList = strList => {
    switch (strList) {
    | [] => []
    | [hd, ...tl] => [strToIntList(hd), ...strListToIntListList(tl)]
    };
  };

  let count = data => {
    let rec countInner = (data, level, position) =>
      if (level >= List.length(data)) {
        0;
      } else {
        let left = countInner(data, level + 1, position);
        let right = countInner(data, level + 1, position + 1);
        List.nth(List.nth(data, level), position) + max(left, right);
      };
    countInner(data, 0, 0);
  };

  count(strListToIntListList(String.split_on_char('\n', dataStr)));
};

let p18monolithTailRecursive = dataStr => {
  let strToIntList = str => {
    let strListToIntList = strList => {
      let rec strListToIntListInner = (strList, acc) => {
        switch (strList) {
        | [] => acc
        | [hd, ...tl] =>
          strListToIntListInner(tl, [int_of_string(hd), ...acc])
        };
      };
      strListToIntListInner(strList, []);
    };
    strListToIntList(String.split_on_char(' ', str));
  };

  let strListToIntListList = strList => {
    let rec strListToIntListListInner = (strList, acc) => {
      switch (strList) {
      | [] => acc
      | [hd, ...tl] =>
        strListToIntListListInner(tl, [strToIntList(hd), ...acc])
      };
    };
    strListToIntListListInner(strList, []);
  };

  let count = data => {
    let rec countInner = (data, level, position) =>
      if (level <= (-1)) {
        0;
      } else {
        let currList = List.nth(data, level);
        let left = countInner(data, level - 1, position);
        let right = countInner(data, level - 1, position + 1);
        List.nth(currList, List.length(currList) - position - 1)
        + max(left, right);
      };
    countInner(data, List.length(data) - 1, 0);
  };

  count(strListToIntListList(String.split_on_char('\n', dataStr)));
};

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
