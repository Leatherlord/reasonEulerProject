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
      strList => List.map(int_of_string, String.split_on_char(' ', strList)),
      String.split_on_char('\n', dataStr),
    );
  };

  let count = intListList => {
    let rec countInner = (data, level, position) =>
      if (level >= List.length(data)) {
        0;
      } else {
        let left = countInner(data, level + 1, position);
        let right = countInner(data, level + 1, position + 1);
        List.nth(List.nth(data, level), position) + max(left, right);
      };
    countInner(intListList, 0, 0);
  };

  count(generate(data));
};
