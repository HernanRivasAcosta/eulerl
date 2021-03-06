% If we list all the natural numbers below 10 that are multiples of 3 or 5, we
% get 3, 5, 6 and 9. The sum of these multiples is 23.
%
% Find the sum of all the multiples of 3 or 5 below 1000.
euler1() ->
  23 = euler1_naive(9),
  23 = euler1_constant_time(9),
  Naive = euler1_naive(999),
  ConstantTime = euler1_constant_time(999),
  Naive = ConstantTime.
euler1_naive(N) ->
  lists:sum([X || X <- lists:seq(0, N),
                  X rem 3 =:= 0 orelse X rem 5 =:= 0]).
euler1_constant_time(N) ->
  N3  = (N div 3),
  N5  = (N div 5),
  N15 = (N div 15),
  3  * N3  * (N3  + 1) div 2 +
  5  * N5  * (N5  + 1) div 2 -
  15 * N15 * (N15 + 1) div 2.

% Each new term in the Fibonacci sequence is generated by adding the previous
% two terms. By starting with 1 and 2, the first 10 terms will be:
% 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
%
% By considering the terms in the Fibonacci sequence whose values do not exceed
% four million, find the sum of the even-valued terms.
euler2() ->
  Naive = euler2_naive(),
  Better = euler2_better(),
  Naive = Better.
euler2_naive() ->
  euler2_naive(1, 1, 0).
euler2_naive(Fn, Fn1, Sum) ->
  case Fn + Fn1 of
    Fn2 when Fn2 =< 4000000 ->
      NewSum = case Fn2 rem 2 of
                 0 -> Sum + Fn2;
                 _ -> Sum
               end,
      euler2_naive(Fn1, Fn2, NewSum);
    _Fn2 ->
      Sum
  end.
euler2_better() ->
  euler2_better(0, 2, 2).
euler2_better(Efn, Efn1, Sum) ->
  case Efn1 * 4 + Efn of
    Efn2 when Efn2 =< 4000000 ->
      euler2_better(Efn1, Efn2, Sum + Efn2);
    _Efn2 ->
      Sum
  end.

% The prime factors of 13195 are 5, 7, 13 and 29.
%
% What is the largest prime factor of the number 600851475143 ?
euler3() ->
  29 = euler3(13195),
  euler3(600851475143).
euler3(N) ->
  euler3(2, N, 0).
euler3(CurrentDiv, N, Max) when CurrentDiv =< N ->
  case N rem CurrentDiv of
    0 ->
      euler3(CurrentDiv + 1, N div CurrentDiv, CurrentDiv);
    _ ->
      euler3(CurrentDiv + 1, N, Max)
  end;
euler3(_CurrentDiv, _N, Max) ->
  Max.

% A palindromic number reads the same both ways. The largest palindrome made
% from the product of two 2-digit numbers is 9009 = 91 × 99.
%
% Find the largest palindrome made from the product of two 3-digit numbers.
euler4() ->
  9009 = euler4(99, 99),
  euler4(999, 999).
euler4(0, _N2) ->
  not_found;
euler4(N1, 0) ->
  N = N1 - 1,
  euler4(N, N);
euler4(N1, N2) ->
  R = N1 * N2,
  case utils:is_palindrome(N1 * N2) of
    true -> R;
    _Any -> euler4(N1, N2 - 1)
  end.

% 2520 is the smallest number that can be divided by each of the numbers from 1
% to 10 without any remainder.
%
% What is the smallest positive number that is evenly divisible by all of the
% numbers from 1 to 20?
euler5() ->
  2520 = euler5(10),
  euler5(20).
euler5(N) ->
  euler5(N, 2, []).
euler5(N, I, L) when I > N ->
  utils:multiply(L);
euler5(N, I, L) ->
  euler5(N, I + 1, L ++ (utils:factors(I) -- L)).

% The sum of the squares of the first ten natural numbers is,
% 1^2 + 2^2 + ... + 10^2 = 385
% The square of the sum of the first ten natural numbers is,
% (1 + 2 + ... + 10)^2 = 552 = 3025
% Hence the difference between the sum of the squares of the first ten natural
% numbers and the square of the sum is 3025 − 385 = 2640.
%
% Find the difference between the sum of the squares of the first one hundred
% natural numbers and the square of the sum.
euler6() ->
  2640 = euler6(10),
  euler6(100).
euler6(N) ->
  N * (N + 1) div 2 * N * (N + 1) div 2 - N * (N + 1) * (2 * N + 1) div 6.

% By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see
% that the 6th prime is 13.
%
% What is the 10 001st prime number?
euler7() ->
  13 = lists:last(utils:primes(6)),
  lists:last(utils:primes(10001)).

% The four adjacent digits in the 1000-digit number that have the greatest
% product are 9 × 9 × 8 × 9 = 5832.
% 73167176531330624919225119674426574742355349194934
% 96983520312774506326239578318016984801869478851843
% 85861560789112949495459501737958331952853208805511
% 12540698747158523863050715693290963295227443043557
% 66896648950445244523161731856403098711121722383113
% 62229893423380308135336276614282806444486645238749
% 30358907296290491560440772390713810515859307960866
% 70172427121883998797908792274921901699720888093776
% 65727333001053367881220235421809751254540594752243
% 52584907711670556013604839586446706324415722155397
% 53697817977846174064955149290862569321978468622482
% 83972241375657056057490261407972968652414535100474
% 82166370484403199890008895243450658541227588666881
% 16427171479924442928230863465674813919123162824586
% 17866458359124566529476545682848912883142607690042
% 24219022671055626321111109370544217506941658960408
% 07198403850962455444362981230987879927244284909188
% 84580156166097919133875499200524063689912560717606
% 05886116467109405077541002256983155200055935729725
% 71636269561882670428252483600823257530420752963450
%
% Find the thirteen adjacent digits in the 1000-digit number that have the
% greatest product. What is the value of this product?
euler8() ->
  5832 = euler8(4),
  euler8(13).
euler8(N) ->
  L = [7, 3, 1, 6, 7, 1, 7, 6, 5, 3, 1, 3, 3, 0, 6, 2, 4, 9, 1, 9, 2, 2, 5, 1,
       1, 9, 6, 7, 4, 4, 2, 6, 5, 7, 4, 7, 4, 2, 3, 5, 5, 3, 4, 9, 1, 9, 4, 9,
       3, 4, 9, 6, 9, 8, 3, 5, 2, 0, 3, 1, 2, 7, 7, 4, 5, 0, 6, 3, 2, 6, 2, 3,
       9, 5, 7, 8, 3, 1, 8, 0, 1, 6, 9, 8, 4, 8, 0, 1, 8, 6, 9, 4, 7, 8, 8, 5,
       1, 8, 4, 3, 8, 5, 8, 6, 1, 5, 6, 0, 7, 8, 9, 1, 1, 2, 9, 4, 9, 4, 9, 5,
       4, 5, 9, 5, 0, 1, 7, 3, 7, 9, 5, 8, 3, 3, 1, 9, 5, 2, 8, 5, 3, 2, 0, 8,
       8, 0, 5, 5, 1, 1, 1, 2, 5, 4, 0, 6, 9, 8, 7, 4, 7, 1, 5, 8, 5, 2, 3, 8,
       6, 3, 0, 5, 0, 7, 1, 5, 6, 9, 3, 2, 9, 0, 9, 6, 3, 2, 9, 5, 2, 2, 7, 4,
       4, 3, 0, 4, 3, 5, 5, 7, 6, 6, 8, 9, 6, 6, 4, 8, 9, 5, 0, 4, 4, 5, 2, 4,
       4, 5, 2, 3, 1, 6, 1, 7, 3, 1, 8, 5, 6, 4, 0, 3, 0, 9, 8, 7, 1, 1, 1, 2,
       1, 7, 2, 2, 3, 8, 3, 1, 1, 3, 6, 2, 2, 2, 9, 8, 9, 3, 4, 2, 3, 3, 8, 0,
       3, 0, 8, 1, 3, 5, 3, 3, 6, 2, 7, 6, 6, 1, 4, 2, 8, 2, 8, 0, 6, 4, 4, 4,
       4, 8, 6, 6, 4, 5, 2, 3, 8, 7, 4, 9, 3, 0, 3, 5, 8, 9, 0, 7, 2, 9, 6, 2,
       9, 0, 4, 9, 1, 5, 6, 0, 4, 4, 0, 7, 7, 2, 3, 9, 0, 7, 1, 3, 8, 1, 0, 5,
       1, 5, 8, 5, 9, 3, 0, 7, 9, 6, 0, 8, 6, 6, 7, 0, 1, 7, 2, 4, 2, 7, 1, 2,
       1, 8, 8, 3, 9, 9, 8, 7, 9, 7, 9, 0, 8, 7, 9, 2, 2, 7, 4, 9, 2, 1, 9, 0,
       1, 6, 9, 9, 7, 2, 0, 8, 8, 8, 0, 9, 3, 7, 7, 6, 6, 5, 7, 2, 7, 3, 3, 3,
       0, 0, 1, 0, 5, 3, 3, 6, 7, 8, 8, 1, 2, 2, 0, 2, 3, 5, 4, 2, 1, 8, 0, 9,
       7, 5, 1, 2, 5, 4, 5, 4, 0, 5, 9, 4, 7, 5, 2, 2, 4, 3, 5, 2, 5, 8, 4, 9,
       0, 7, 7, 1, 1, 6, 7, 0, 5, 5, 6, 0, 1, 3, 6, 0, 4, 8, 3, 9, 5, 8, 6, 4,
       4, 6, 7, 0, 6, 3, 2, 4, 4, 1, 5, 7, 2, 2, 1, 5, 5, 3, 9, 7, 5, 3, 6, 9,
       7, 8, 1, 7, 9, 7, 7, 8, 4, 6, 1, 7, 4, 0, 6, 4, 9, 5, 5, 1, 4, 9, 2, 9,
       0, 8, 6, 2, 5, 6, 9, 3, 2, 1, 9, 7, 8, 4, 6, 8, 6, 2, 2, 4, 8, 2, 8, 3,
       9, 7, 2, 2, 4, 1, 3, 7, 5, 6, 5, 7, 0, 5, 6, 0, 5, 7, 4, 9, 0, 2, 6, 1,
       4, 0, 7, 9, 7, 2, 9, 6, 8, 6, 5, 2, 4, 1, 4, 5, 3, 5, 1, 0, 0, 4, 7, 4,
       8, 2, 1, 6, 6, 3, 7, 0, 4, 8, 4, 4, 0, 3, 1, 9, 9, 8, 9, 0, 0, 0, 8, 8,
       9, 5, 2, 4, 3, 4, 5, 0, 6, 5, 8, 5, 4, 1, 2, 2, 7, 5, 8, 8, 6, 6, 6, 8,
       8, 1, 1, 6, 4, 2, 7, 1, 7, 1, 4, 7, 9, 9, 2, 4, 4, 4, 2, 9, 2, 8, 2, 3,
       0, 8, 6, 3, 4, 6, 5, 6, 7, 4, 8, 1, 3, 9, 1, 9, 1, 2, 3, 1, 6, 2, 8, 2,
       4, 5, 8, 6, 1, 7, 8, 6, 6, 4, 5, 8, 3, 5, 9, 1, 2, 4, 5, 6, 6, 5, 2, 9,
       4, 7, 6, 5, 4, 5, 6, 8, 2, 8, 4, 8, 9, 1, 2, 8, 8, 3, 1, 4, 2, 6, 0, 7,
       6, 9, 0, 0, 4, 2, 2, 4, 2, 1, 9, 0, 2, 2, 6, 7, 1, 0, 5, 5, 6, 2, 6, 3,
       2, 1, 1, 1, 1, 1, 0, 9, 3, 7, 0, 5, 4, 4, 2, 1, 7, 5, 0, 6, 9, 4, 1, 6,
       5, 8, 9, 6, 0, 4, 0, 8, 0, 7, 1, 9, 8, 4, 0, 3, 8, 5, 0, 9, 6, 2, 4, 5,
       5, 4, 4, 4, 3, 6, 2, 9, 8, 1, 2, 3, 0, 9, 8, 7, 8, 7, 9, 9, 2, 7, 2, 4,
       4, 2, 8, 4, 9, 0, 9, 1, 8, 8, 8, 4, 5, 8, 0, 1, 5, 6, 1, 6, 6, 0, 9, 7,
       9, 1, 9, 1, 3, 3, 8, 7, 5, 4, 9, 9, 2, 0, 0, 5, 2, 4, 0, 6, 3, 6, 8, 9,
       9, 1, 2, 5, 6, 0, 7, 1, 7, 6, 0, 6, 0, 5, 8, 8, 6, 1, 1, 6, 4, 6, 7, 1,
       0, 9, 4, 0, 5, 0, 7, 7, 5, 4, 1, 0, 0, 2, 2, 5, 6, 9, 8, 3, 1, 5, 5, 2,
       0, 0, 0, 5, 5, 9, 3, 5, 7, 2, 9, 7, 2, 5, 7, 1, 6, 3, 6, 2, 6, 9, 5, 6,
       1, 8, 8, 2, 6, 7, 0, 4, 2, 8, 2, 5, 2, 4, 8, 3, 6, 0, 0, 8, 2, 3, 2, 5,
       7, 5, 3, 0, 4, 2, 0, 7, 5, 2, 9, 6, 3, 4, 5, 0],
  euler8(lists:sublist(L, N), lists:sublist(L, N, length(L))).
euler8(CurrSeq, ItemsLeft) ->
  Product = utils:multiply(CurrSeq),
  case lists:member(0, CurrSeq) of
    true ->
      LastZero = string:str(lists:reverse(CurrSeq), [0]),
      euler8(CurrSeq, Product, LastZero, ItemsLeft, 0);
    _Any ->
      euler8(CurrSeq, Product, 0, ItemsLeft, Product)
  end.
euler8(_CurrSeq, _CurrProduct, _LastZero, [], BestProduct) ->
  BestProduct;
euler8([H1 | T1], CurrProduct, LastZero, [H2 | T2], BestProduct) ->
  NewSeq = T1 ++ [H2],
  NewProduct = (CurrProduct div lists:max([1, H1])) * lists:max([1, H2]),
  NewLastZero = case H2 of
                  0 -> length(T1) + 1;
                  _ -> LastZero - 1
                end,
  case (NewProduct > BestProduct) andalso (NewLastZero =< 0) of
    true ->
      euler8(NewSeq, NewProduct, NewLastZero, T2, NewProduct);
    false ->
      euler8(NewSeq, NewProduct, NewLastZero, T2, BestProduct)
  end.

% A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
% a^2 + b^2 = c^2
% For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
% 
% There exists exactly one Pythagorean triplet for which a + b + c = 1000.
% Find the product abc.
euler9() ->
  60 = euler9(12),
  euler9(1000).
euler9(N) ->
  euler9(N, N div 3, N div 2, N).
euler9(_N, 0, _B, _C) ->
  not_found;
euler9(N, A, 0, C) ->
  euler9(N, A - 1, N div 2, C);
euler9(N, A, B, 0) ->
  euler9(N, A, B - 1, N);
euler9(N, A, B, C) ->
  case (A + B + C =:= N) andalso (A * A + B * B =:= C * C) of
    true -> A * B * C;
    _Any -> euler9(N, A, B, C - 1)
  end.