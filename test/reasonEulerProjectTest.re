open OUnit2;
open ReasonEulerProject;

let testsMonolithRecursive =
  "test suite for monolith recursive"
  >::: [
    "single number"
    >:: (
      _ => assert_equal("1234567890", monolithRecursive("123456789012345"))
    ),
    "easy case"
    >:: (
      _ =>
        assert_equal(
          "2222222222",
          monolithRecursive("111111111111111
111111111111111"),
        )
    ),
    "should fail (input too short)"
    >:: (
      _ =>
        assert_raises(Invalid_argument("String.sub / Bytes.sub"), () =>
          monolithRecursive("12345")
        )
    ),
  ];

let testsMonolithTailRecursive =
  "test suite for monolith tail recursive"
  >::: [
    "single number"
    >:: (
      _ =>
        assert_equal("1234567890", monolithTailRecursive("123456789012345"))
    ),
    "easy case"
    >:: (
      _ =>
        assert_equal(
          "2222222222",
          monolithTailRecursive("111111111111111
111111111111111"),
        )
    ),
    "should fail (input too short)"
    >:: (
      _ =>
        assert_raises(Invalid_argument("String.sub / Bytes.sub"), () =>
          monolithTailRecursive("12345")
        )
    ),
  ];

run_test_tt_main(testsMonolithRecursive);
run_test_tt_main(testsMonolithTailRecursive);
