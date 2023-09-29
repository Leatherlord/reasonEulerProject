open ReUtils;

let p13monolithRecursive = dataStr => {
  let rec strListToZList =
    fun
    | [] => []
    | [hd, ...tl] => [hd |> ReZ.ofString, ...tl |> strListToZList];

  let rec sumZ =
    fun
    | [] => ReZ.zero
    | [hd, ...tl] => ReZ.(+)(hd, tl |> sumZ);

  dataStr
  |> ReString.splitOnChar('\n', _)
  |> strListToZList
  |> sumZ
  |> ReZ.toString
  |> ReString.sub(_, 0, 10);
};

let p13monolithTailRecursive = dataStr => {
  let strListToZList = strList => {
    let rec strListToZListInner =
      fun
      | ([], acc) => acc
      | ([hd, ...tl], acc) =>
        (tl, [hd |> ReZ.ofString, ...acc]) |> strListToZListInner;

    (strList, []) |> strListToZListInner;
  };

  let sumZ = zList => {
    let rec sumZInner =
      fun
      | ([], acc) => acc
      | ([hd, ...tl], acc) => (tl, ReZ.(+)(acc, hd)) |> sumZInner;

    (zList, ReZ.zero) |> sumZInner;
  };

  dataStr
  |> ReString.splitOnChar('\n')
  |> strListToZList
  |> sumZ
  |> ReZ.toString
  |> ReString.sub(_, 0, 10);
};

let p18monolithRecursive = dataStr => {
  let rec strListToIntListList = {
    let strToIntList = str => {
      let rec strListToIntList =
        fun
        | [] => []
        | [hd, ...tl] => [hd |> intOfString, ...tl |> strListToIntList];

      str |> ReString.splitOnChar(' ') |> strListToIntList;
    };

    fun
    | [] => []
    | [hd, ...tl] => [hd |> strToIntList, ...tl |> strListToIntListList];
  };

  let count = data => {
    let rec countInner =
      fun
      | (data, level, _) when level >= ReList.length(data) => 0
      | (data, level, position) => {
          let left = countInner((data, level + 1, position));
          let right = countInner((data, level + 1, position + 1));
          ReList.nth(ReList.nth(data, level), position) + max(left, right);
        };

    (data, 0, 0) |> countInner;
  };

  dataStr |> ReString.splitOnChar('\n') |> strListToIntListList |> count;
};

let p18monolithTailRecursive = dataStr => {
  let strToIntList = str => {
    let strListToIntList = strList => {
      let rec strListToIntListInner =
        fun
        | ([], acc) => acc
        | ([hd, ...tl], acc) =>
          (tl, [intOfString(hd), ...acc]) |> strListToIntListInner;

      (strList, []) |> strListToIntListInner;
    };
    str |> ReString.splitOnChar(' ') |> strListToIntList;
  };

  let strListToIntListList = strList => {
    let rec strListToIntListListInner =
      fun
      | ([], acc) => acc
      | ([hd, ...tl], acc) =>
        (tl, [hd |> strToIntList, ...acc]) |> strListToIntListListInner;

    (strList, []) |> strListToIntListListInner;
  };

  let count = data => {
    let rec countInner =
      fun
      | (_, level, _) when level <= (-1) => 0
      | (data, level, position) => {
          let currList = ReList.nth(data, level);
          let left = countInner((data, level - 1, position));
          let right = countInner((data, level - 1, position + 1));
          ReList.nth(currList, ReList.length(currList) - position - 1)
          + max(left, right);
        };

    (data, ReList.length(data) - 1, 0) |> countInner;
  };

  dataStr |> ReString.splitOnChar('\n') |> strListToIntListList |> count;
};

let p13modular = data => {
  let transform = (numbers: string) => {
    numbers
    |> ReString.splitOnChar('\n')
    |> ReList.map(str => str |> ReZ.ofString);
  };

  let sum = (numbers: list(Z.t)) => {
    numbers |> ReList.foldLeft(ReZ.(+), ReZ.zero);
  };

  data |> transform |> sum |> ReZ.toString |> ReString.sub(_, 0, 10);
};

let p18modular = data => {
  let transform = (triangle: string) => {
    triangle
    |> ReString.splitOnChar('\n')
    |> ReList.map(line =>
         " 0"
         |> (++)(line)
         |> ReString.splitOnChar(' ')
         |> ReList.map(intOfString)
       );
  };

  let count = intListList => {
    let maxPairs = (line: list(int)) => {
      [0] |> (@)(line |> ReList.tl) |> ReList.map2(max, line);
    };

    let rec firstN =
      fun
      | ([], _) => []
      | ([_, ..._], n) when n == 0 => []
      | ([hd, ..._], n) when n == 1 => [hd]
      | ([hd, ...tl], n) => [hd, ...firstN((tl, n - 1))];

    let folder = (line: list(int), results: list(int)) => {
      (results, ReList.length(line))
      |> firstN
      |> maxPairs
      |> ReList.map2((+), _, line);
    };

    let zeroes =
      intListList
      |> ReList.length
      |> (+)(1)
      |> Seq.take(_, Seq.forever(_ => 0))
      |> ReList.ofSeq;

    intListList |> ReList.foldRight(folder, _, zeroes) |> ReList.hd;
  };

  data |> transform |> count;
};
