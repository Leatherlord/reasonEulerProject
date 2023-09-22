let monolithRecursive = dataStr => {
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
  //   print_endline(result);
};

let monolithTailRecursive = dataStr => {
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
  //   print_endline(result);
};
