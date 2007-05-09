%% Test suite for isbn.erl
-module(isbn_test).
-include_lib("eunit/include/eunit.hrl").
-import(isbn, [check_digit_10/1, check_digit_10_string/1, validate_10/1, validate_10_string/1]).
-import(isbn, [check_digit_13/1, check_digit_13_string/1, validate_13/1, validate_13_string/1]).
-import(isbn, [convert_10_to_13/1, convert_10_to_13_string/1, convert_13_to_10/1, convert_13_to_10_string/1]).

check_digit_10_test_() -> [
    {"X", ?_assert(check_digit_10([1,9,3,4,3,5,6,0,0]) == 'X')},
    {"number", ?_assert(check_digit_10([0,5,9,6,0,0,7,9,7]) == 3)},
    {"wrongLength", ?_assertException(throw, wrongLength, check_digit_10([1,2,3,4,5,6,7,8]))}
].

check_digit_10_string_test_() -> [
    {"X", ?_assert(check_digit_10_string("1-59200-346") == 'X')},
    {"number", ?_assert(check_digit_10_string("020148567") == 2)},
    {"wrongLength", ?_assertException(throw, wrongLength, check_digit_10_string("1-59200-34"))}
].

validate_10_test_() -> [
    {"true", ?_assert(validate_10([0,6,7,2,3,2,7,5,6,2]) == true)},
    {"false", ?_assert(validate_10([0,6,2,7,3,2,7,5,6,2]) == false)},
    {"true X", ?_assert(validate_10([1,5,9,2,0,0,3,4,6,'X']) == true)},
    {"wrongLength", ?_assertException(throw, wrongLength, validate_10([1,2,3,4,5,6,7,8,9]))}
].

validate_10_string_test_() -> [
    {"true with hyphens", ?_assert(validate_10_string("1-59200-346-X") == true)},
    {"true without hyphens", ?_assert(validate_10_string("0596007833") == true)},
    {"false with hyphens", ?_assert(validate_10_string("0-569-00783-3") == false)},
    {"false without hyphens", ?_assert(validate_10_string("0569007833") == false)},
    {"wrongLength", ?_assertException(throw, wrongLength, validate_10_string("123456789"))}
].

check_digit_13_test_() -> [
    {"number", ?_assert(check_digit_13([9,7,8,1,9,3,4,3,5,6,0,0]) == 5)},
    {"zero", ?_assert(check_digit_13([9,7,8,0,6,9,1,1,3,0,6,2]) == 0)},
    {"wrongLength", ?_assertException(throw, wrongLength, check_digit_13([9,7,8,1,9,3,4,3,5,6,0]))}
].

check_digit_13_string_test_() -> [
    {"zero", ?_assert(check_digit_13_string("978-1-59059-803") == 0)},
    {"number", ?_assert(check_digit_13_string("978-059610012") == 4)},
    {"wrongLength", ?_assertException(throw, wrongLength, check_digit_10_string("978-05961001"))}
].

validate_13_test_() -> [
    {"true", ?_assert(validate_13([9,7,8,0,8,1,4,7,4,2,8,1,5]) == true)},
    {"false", ?_assert(validate_13([9,7,8,0,8,1,4,7,2,4,8,1,5]) == false)},
    {"true 0", ?_assert(validate_13([9,7,8,1,5,9,0,5,9,8,0,3,0]) == true)},
    {"wrongLength", ?_assertException(throw, wrongLength, validate_13([1,2,3,4,5,6,7,8,9]))}
].

validate_13_string_test_() -> [
    {"true with hyphens", ?_assert(validate_13_string("978-0300111507") == true)},
    {"true without hyphens", ?_assert(validate_13_string("9780596007867") == true)},
    {"false with hyphens", ?_assert(validate_13_string("978-0301111507") == false)},
    {"false without hyphens", ?_assert(validate_13_string("9780996007867") == false)},
    {"wrongLength", ?_assertException(throw, wrongLength, validate_13_string("123456789"))}
].

convert_10_to_13_test_() -> [
    {"successful", ?_assert(convert_10_to_13([0,5,9,6,0,0,7,8,6,8]) == [9,7,8,0,5,9,6,0,0,7,8,6,7])},
    {"wrongLength", ?_assertException(throw, wrongLength, convert_10_to_13([1,2,3,4,5,6,7,8,9]))}
].

convert_10_to_13_string_test_() -> [
    {"successful", ?_assert(convert_10_to_13_string("0545010225") == "9780545010221")},
    {"wrongLength", ?_assertException(throw, wrongLength, convert_10_to_13_string("05450102253"))}
].

convert_13_to_10_test_() -> [
    {"successful regular", ?_assert(convert_13_to_10([9,7,8,0,5,9,6,0,0,7,8,6,7]) == [0,5,9,6,0,0,7,8,6,8])},
    {"successful 0", ?_assert(convert_13_to_10([9,7,8,1,5,9,0,5,9,8,0,3,0]) == [1,5,9,0,5,9,8,0,3,2])},
    {"successful X", ?_assert(convert_13_to_10([9,7,8,0,3,4,5,4,4,7,5,9,3]) == [0,3,4,5,4,4,7,5,9,'X'])},
    {"invalidIsbn10", ?_assertException(throw, invalidIsbn10, convert_13_to_10([9,7,6,1,5,9,0,5,9,8,0,3,0]))},
    {"wrongLength", ?_assertException(throw, wrongLength, convert_13_to_10([1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]))}
].

convert_13_to_10_string_test_() -> [
    {"successful X", ?_assert(convert_13_to_10_string("978-1934356005") == "193435600X")},
    {"successful 0", ?_assert(convert_13_to_10_string("978-1590598030") == "1590598032")},
    {"successful regular", ?_assert(convert_13_to_10_string("978-0439588638") == "0439588634")},
    {"invalidIsbn10", ?_assertException(throw, invalidIsbn10, convert_13_to_10_string("979-0439588638"))},
    {"wrongLength", ?_assertException(throw, wrongLength, convert_13_to_10_string("978-193435600"))}
].
