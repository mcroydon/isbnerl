%% @author Matt Croydon <matt@ooiio.com> [http://postneo.com]
%% @copyright 2007 Matt Croydon
%% @version 0.0.5
%% @doc ISBN utilities for Erlang including ISBN-10 and ISBN-13 check digit
%% generators and validators.  Also included is a  ISBN-10 to
%% ISBN-13 conversion, and ISBN-13 to ISBN-10 when applicable.

-module(isbn).
-export([check_digit_10/1, validate_10/1, check_digit_13/1, validate_13/1, convert_10_to_13/1, convert_13_to_10/1]).

%% @doc Given a 9 digit list, calculates the ISBN-10 check digit and returns it.
%% @spec check_digit_10(List) -> integer()
%% @throws atom()
check_digit_10(Isbn) when length(Isbn) /= 9 ->
    throw(wrongLength);

check_digit_10(Isbn) -> 
    check_digit_10(Isbn, 0).

check_digit_10([H|T], Total) ->
    check_digit_10(T, Total + (H * (length(T) + 2)));

check_digit_10([], Total) when 11 - (Total rem 11) =:= 10 ->
    'X';

check_digit_10([], Total) ->
    11 - (Total rem 11).

%% @doc Given a 10 digit ISBN, generates check digit and checks to see
%% that is matches the last integer passed in.  Returns true or
%% false, depending on if they match or not. 
%% @spec validate_10(List) -> boolean()
%% @throws atom()
validate_10(Isbn) when length(Isbn) /= 10 ->
    throw(wrongLength);

validate_10(Isbn) ->
    {MainList, CheckDigitList} = lists:split(9, Isbn),
    GivenCheckDigit = hd(CheckDigitList),
    case check_digit_10(MainList) of
	GivenCheckDigit -> true;
	_ -> false
    end.

%% @doc Given a 12 digit list, calculates the ISBN-13 check digit and 
%% returns it.
%% @spec check_digit_13(List) -> integer()
%% @throws atom()
check_digit_13(Isbn) when length(Isbn) /= 12 ->
    throw(wrongLength);

check_digit_13(Isbn) ->
    check_digit_13(Isbn, 0, 0, 1).

check_digit_13([H|T], OddTotal, EvenTotal, Count) ->
    case (Count rem 2) of
	1 -> check_digit_13(T, OddTotal + (H * 1), EvenTotal, Count + 1);
	0 -> check_digit_13(T, OddTotal, EvenTotal + (H * 3), Count + 1)
    end;

check_digit_13([], OddTotal, EvenTotal, _Count) ->
    Total = OddTotal + EvenTotal,
    Remainder = Total rem 10,
    Result = 10 - Remainder,
    case Result of
	10 -> 0;
	_ -> Result
    end.

%% @doc Given a 13 digit ISBN as a list of integers, generates the check
%% digit and returns true if it matches the check digit passed in,
%% false otherwise.
%% @spec validate_13(List) -> boolean()
%% @throws atom()
validate_13(Isbn) when length(Isbn) /= 13 ->
    throw(wrongLength);

validate_13(Isbn) ->
    {MainList, CheckDigitList} = lists:split(12, Isbn),
    GivenCheckDigit = hd(CheckDigitList),
    case check_digit_13(MainList) of
        GivenCheckDigit -> true;
        _ -> false
    end.

%% @doc Given an ISBN-10, converts it to ISBN-13, checks validity, and
%% returns the ISBN-13 or throws an error.
%% @spec convert_10_to_13(List) -> List
%% @throws atom()
convert_10_to_13(Isbn) when length(Isbn) /= 10 ->
    throw(wrongLength);

convert_10_to_13(Isbn) ->
    {Isbn9, _Tail} = lists:split(9, Isbn),
    Isbn12 = lists:append([9,7,8], Isbn9),
    CheckDigit = check_digit_13(Isbn12),
    Isbn13 = lists:append(Isbn12, [CheckDigit]),
    IsValid = validate_13(Isbn13),
    case IsValid of
	false -> throw(invalidIsbn13);
	true  -> Isbn13
    end.

%% @doc Given an ISBN-13, returns the correct ISBN-10 by
%% dropping the first three digits and check digit then
%% recalculating the new ISBN-10 check digit.
%%
%% If the given ISBN-13 does not begin with [9,7,8],
%% an error is thrown because only 978- ISBN-13's can
%% be translated in to ISBN-10.
%% @spec convert_13_to_10(List) -> List
%% @throws atom()
convert_13_to_10(Isbn) when length(Isbn) /= 13 ->
    throw(wrongLength);

convert_13_to_10(Isbn) ->
    case lists:prefix([9,7,8], Isbn) of
        false ->
            throw(invalidIsbn10);
        true ->
            do_convert_13_to_10(Isbn)
    end.
    
do_convert_13_to_10(Isbn) ->
    {_Prefix, Isbn10} = lists:split(3, Isbn),
    {Isbn9, _OldCheckDigit} = lists:split(9, Isbn10),
    CheckDigit = check_digit_10(Isbn9),
    FinalIsbn = lists:append(Isbn9, [CheckDigit]),
    FinalIsbn.
