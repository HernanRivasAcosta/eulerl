-module(utils).

-export([fibs/1, primes/1, factors/1, ufactors/1, multiply/1, is_palindrome/1]).

%%==============================================================================
%% Utils
%%==============================================================================
% A list of the first N fibonacci numbers (guaranteed to be sorted)
fibs(N) -> fibs(N, [1, 1]).
fibs(0, L) -> lists:reverse(L);
fibs(N, [Fn, Fn1 | _T] = L) -> fibs(N - 1, [Fn + Fn1 | L]).

% A list of the first N primes (guaranteed to be sorted)
primes(N) ->
  primes_naive(N).

% The prime factors of N (guaranteed to be sorted)
factors(N) ->
  factors(2, N, []).
factors(I, N, L) when I =< N ->
  case N rem I of
    0 -> factors(I, N div I, [I | L]);
    _ -> factors(I + 1, N, L)
  end;
factors(_I, _N, L) ->
  lists:reverse(L).

% The unique prime factors of N (guaranteed to be sorted)
ufactors(N) ->
  lists:usort(factors(N)).

% Returns true if a given number is a palindrome (101, 434, 456654, etc)
is_palindrome(N) ->
  is_palindrome_naive(N).

% Multiplies all numbers in a list (similar to lists:sum/1)
multiply(L) ->
  multiply(L, 1).
multiply([], R) ->
  R;
multiply([H | T], R) ->
  multiply(T, H * R).

%%==============================================================================
%% Internal functions
%%==============================================================================
is_palindrome_naive(N) ->
  Str = integer_to_list(N),
  L = length(Str) div 2,
  lists:sublist(Str, L) =:= lists:sublist(lists:reverse(Str), L).

primes_naive(N) ->
  primes_naive(N, 2, []).
primes_naive(0, _I, L) ->
  lists:reverse(L);
primes_naive(N, I, L) ->
  case lists:all(fun(X) -> I rem X =/= 0 end, L) of
    true -> primes_naive(N - 1, I + 1, [I | L]);
    _Any -> primes_naive(N, I + 1, L)
  end.