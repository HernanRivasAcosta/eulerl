# Eulerl
Solving the Euler Project problems in Erlang.

## Usage
Just use rebar

```
$ rebar compile
==> euler (compile)
$ rebar shell
==> euler (shell)
Erlang R16B03-1 (erts-5.10.4) [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V5.10.4  (abort with ^G)
1> euler:euler1().
233168
```

## Code structure
Under `solutions/` you can find files each containing 10 solutions, this are included on `euler.erl` which while ugly makes it easy to use and write.
