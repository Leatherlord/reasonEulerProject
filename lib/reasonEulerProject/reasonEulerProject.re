open ReUtils;

let p13monolithRecursive = dataStr => {
  let rec strListToZList = strList =>
    switch (strList) {
    | [] => []
    | [hd, ...tl] => [ReZ.ofString(hd), ...strListToZList(tl)]
    };

  let rec sumZ = zList =>
    switch (zList) {
    | [] => ReZ.zero
    | [hd, ...tl] => ReZ.(+)(hd, sumZ(tl))
    };

  let result =
    ReString.sub(
      ReZ.toString(
        sumZ(strListToZList(ReString.splitOnChar('\n', dataStr))),
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
      | [hd, ...tl] => strListToZListInner(tl, [ReZ.ofString(hd), ...acc])
      };
    strListToZListInner(strList, []);
  };

  let sumZ = zList => {
    let rec sumZInner = (zList, acc) =>
      switch (zList) {
      | [] => acc
      | [hd, ...tl] => sumZInner(tl, ReZ.(+)(acc, hd))
      };

    sumZInner(zList, ReZ.zero);
  };

  let result =
    ReString.sub(
      ReZ.toString(
        sumZ(strListToZList(ReString.splitOnChar('\n', dataStr))),
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
      | [hd, ...tl] => [intOfString(hd), ...strListToIntList(tl)]
      };
    strListToIntList(ReString.splitOnChar(' ', str));
  };

  let rec strListToIntListList = strList => {
    switch (strList) {
    | [] => []
    | [hd, ...tl] => [strToIntList(hd), ...strListToIntListList(tl)]
    };
  };

  let count = data => {
    let rec countInner = (data, level, position) =>
      if (level >= ReList.length(data)) {
        0;
      } else {
        let left = countInner(data, level + 1, position);
        let right = countInner(data, level + 1, position + 1);
        ReList.nth(ReList.nth(data, level), position) + max(left, right);
      };
    countInner(data, 0, 0);
  };

  count(strListToIntListList(ReString.splitOnChar('\n', dataStr)));
};

let p18monolithTailRecursive = dataStr => {
  let strToIntList = str => {
    let strListToIntList = strList => {
      let rec strListToIntListInner = (strList, acc) => {
        switch (strList) {
        | [] => acc
        | [hd, ...tl] =>
          strListToIntListInner(tl, [intOfString(hd), ...acc])
        };
      };
      strListToIntListInner(strList, []);
    };
    strListToIntList(ReString.splitOnChar(' ', str));
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
        let currList = ReList.nth(data, level);
        let left = countInner(data, level - 1, position);
        let right = countInner(data, level - 1, position + 1);
        ReList.nth(currList, ReList.length(currList) - position - 1)
        + max(left, right);
      };
    countInner(data, ReList.length(data) - 1, 0);
  };

  count(strListToIntListList(ReString.splitOnChar('\n', dataStr)));
};

let p13modular = data => {
  let generate = dataStr => {
    ReList.map(
      str => ReZ.ofString(str),
      ReString.splitOnChar('\n', dataStr),
    );
  };

  let count = zList => {
    ReList.foldLeft(ReZ.(+), ReZ.zero, zList);
  };

  ReString.sub(ReZ.toString(count(generate(data))), 0, 10);
};

let p18modular = data => {
  let generate = dataStr => {
    ReList.map(
      strList =>
        ReList.map(intOfString, ReString.splitOnChar(' ', strList ++ " 0")),
      ReString.splitOnChar('\n', dataStr),
    );
  };

  let count = intListList => {
    let maxPairs = ys => ReList.map2(max, ys, ReList.tl(ys) @ [0]);
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
      ReList.map2((+), maxPairs(firstN(rs, ReList.length(s))), s);
    };
    let zeroes =
      Seq.take(ReList.length(intListList) + 1, Seq.forever(_ => 0));
    ReList.hd(ReList.foldRight(f, intListList, ReList.ofSeq(zeroes)));
  };

  count(generate(data));
};
