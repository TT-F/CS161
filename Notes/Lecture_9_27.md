# Lecture 1

## Date: 9/27

* some information and background knowledge about AI.
* LISP

### LISP 

```LISP
>(quote (+ 3 1))
>(+ 3 1)

>(+ 3 1)
> 4

>(setq X 3)
>3

>(setq x (+ 1 2 3))
>(+ 1 2 3)

>setq x (+ 1 2 3)
>6

>(setq x (a b c))
>(a b c)

>(car x)
>a

>(cdr x)
>(b c)

>(car (cdr x))
>b

>(cadr x)
>b

>(cdr (cdr x))
>(c)

>(car (cdr (cdr x)))
>c

>(caddr x)
>c

>(setq x '(a (b c) d e))
>(caadr x)
>b

>(cadadr x)
>c

>(setq x '(a b))
>(cddr x)
> NIL

>(cons 'a '(b c))
>(a b c)

>(cons 'a NIL)
>(a)
```

if you set `a` first: `>(setq a 3)`, you don't need `'` before `a`. 

```LISP
>(setq a 3)
>(cons a NIL)
>(3)

>(list 'a 'b 'c)
>(a b c)
```