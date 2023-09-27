open OUnit2;
open ReasonEulerProject;

let testsP13Modular =
  "test suite for problem 13 modular"
  >::: [
    "single number"
    >:: (_ => assert_equal("1234567890", p13modular("123456789012345"))),
    "easy case"
    >:: (
      _ =>
        assert_equal(
          "2222222222",
          p13modular("111111111111111
111111111111111"),
        )
    ),
    "should fail (input too short)"
    >:: (
      _ =>
        assert_raises(Invalid_argument("String.sub / Bytes.sub"), () =>
          p13modular("12345")
        )
    ),
  ];

let testsP18Modular =
  "test suite for problem 18 modular"
  >::: [
    "single number" >:: (_ => assert_equal(24, p18modular("24"))),
    "easy case" >:: (_ => assert_equal(10, p18modular("1
2 3
4 5 6"))),
    "should fail (cannot cast empty string to int)"
    >:: (_ => assert_raises(Failure("int_of_string"), () => p18modular(""))),
  ];

run_test_tt_main(testsP13Modular);
run_test_tt_main(testsP18Modular);
