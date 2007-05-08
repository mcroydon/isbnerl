%% Test suite for isbn.erl
-module(isbn_test).
-include_lib("eunit/include/eunit.hrl").
-import(isbn, [check_digit_10/1, check_digit_10_string/1, validate_10/1, validate_10_string/1]).
-import(isbn, [check_digit_13/1, check_digit_13_string/1, validate_13/1, validate_13_string/1]).
-import(isbn, [convert_10_to_13/1, convert_10_to_13_string/1, convert_13_to_10/1, convert_13_to_10_string/1]).

check_digit_10_test_() -> [
    ?_assert(check_digit_10([1,9,3,4,3,5,6,0,0]) == 'X'),
    ?_assert(check_digit_10([0,5,9,6,0,0,7,9,7]) == 3),
    ?_assertException(throw, wrongLength, check_digit_10([1,2,3,4,5,6,7,8]))
].

check_digit_10_string_test_() -> [
    ?_assert(check_digit_10_string("1-59200-346") == 'X'),
    ?_assert(check_digit_10_string("020148567") == 2),
    ?_assertException(throw, wrongLength, check_digit_10_string("1-59200-34"))
].

validate_10_test_() -> [
    ?_assert(validate_10([0,6,7,2,3,2,7,5,6,2]) == true),
    ?_assert(validate_10([0,6,2,7,3,2,7,5,6,2]) == false),
    ?_assert(validate_10([1,5,9,2,0,0,3,4,6,'X']) == true),
    ?_assertException(throw, wrongLength, validate_10([1,2,3,4,5,6,7,8,9]))
].

validate_10_string_test_() -> [
    ?_assert(validate_10_string("1-59200-346-X") == true),
    ?_assert(validate_10_string("0596007833") == true),
    ?_assert(validate_10_string("0-569-00783-3") == false),
    ?_assertException(throw, wrongLength, validate_10_string("123456789"))
].

check_digit_13_test_() -> [
    ?_assert(check_digit_13([9,7,8,1,9,3,4,3,5,6,0,0]) == 5),
    ?_assert(check_digit_13([9,7,8,0,6,9,1,1,3,0,6,2]) == 0),
    ?_assertException(throw, wrongLength, check_digit_13([9,7,8,1,9,3,4,3,5,6,0]))
].
