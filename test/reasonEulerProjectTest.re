open OUnit2;
open ReasonEulerProject;

let testsP13MonolithRecursive =
  "test suite for problem 13 monolith recursive"
  >::: [
    "single number"
    >:: (
      _ =>
        assert_equal("1234567890", p13monolithRecursive("123456789012345"))
    ),
    "easy case"
    >:: (
      _ =>
        assert_equal(
          "2222222222",
          p13monolithRecursive("111111111111111
111111111111111"),
        )
    ),
    "should fail (input too short)"
    >:: (
      _ =>
        assert_raises(Invalid_argument("String.sub / Bytes.sub"), () =>
          p13monolithRecursive("12345")
        )
    ),
  ];

let testsP13MonolithTailRecursive =
  "test suite for problem 13 monolith tail recursive"
  >::: [
    "single number"
    >:: (
      _ =>
        assert_equal(
          "1234567890",
          p13monolithTailRecursive("123456789012345"),
        )
    ),
    "easy case"
    >:: (
      _ =>
        assert_equal(
          "2222222222",
          p13monolithTailRecursive("111111111111111
111111111111111"),
        )
    ),
    "should fail (input too short)"
    >:: (
      _ =>
        assert_raises(Invalid_argument("String.sub / Bytes.sub"), () =>
          p13monolithTailRecursive("12345")
        )
    ),
  ];

run_test_tt_main(testsP13MonolithRecursive);
run_test_tt_main(testsP13MonolithTailRecursive);
