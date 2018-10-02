# Lecture 2 
## 10/2 

## Lisp Expression 

    * accessors: car (first) cdr (rest)
    *Constructors: cons, list  
* Boolean Expression 
* Conditions 
* Functions

```Lisp
  NIL ()
  t 
  (> 1 3)
  (<= 3 7)

  (numberp)
  (symblep)
  (listp)
  (atom)
```

## Branching
```Lisp 
(cond   (bexp arg1 .... argn)
        (bexp arg1 .... argn)
        ...
        )

>(setq x 5)
>(cond  ((= x 0), 0)
        ((< x 0), `neg)
        ((> x 0), `pos)
        )

>(defin square (x)
    (* x x))
>(square 3)
>9 

>(define abs(x)
    (cond   (= x 0) 0)
            ((> x 0) x)
            (t (- x))
    )

>(defun odd? (x)
        (cond   ((= x 0) NIL)
                ((= x 1) t)
                (t (odd? (- x 2)))))

>(defun fact(n)
        (cond   ((= n 0) 1)
                (t (* n (fact (- n 1))))))

>(defun power(B p)
        (cond   ((= p 0) 1)
                ((evenp p) (square (power B (/ p 2))))
                (t (* B (power B (- p 1))))))

>(defun sum-list(L)
        (cond ((NULL L) 0)
        (t (+   (first L)
                (sum-list (rest L))))))

>(defun member? (x L)
        (cond   ((NULL L) NIL)
                ((equal x (fist L) t)
                (t (member? x (rest L))))))

>(defun nth (n L)
        (cond   ((= n 0) (first L))
                (t (nth (- n 1) (rest L)))))

>(defun Last (L)
        (cond   ((NULL (rest L)) (fist L))
                (t (last (rest L)))))


>(member 'b '(a b c d))
>t

>(defun remove(x L)
        (cond   ((null L) NIL)
                ((equal x (first L)) (rest L))
                (t (cons (first L)
                        (remove x ( rest L))))))

>(defun append(L1 L2)
        (cond ((NULL L1) L2)
                (t (cons (first L1) (append (rest L1) L2)))))

>(defun reverse (L)
        (cond   ((null L) NIL)
                (t (append (reverse (rest L))
                        (list (first L))))))

>(cons L1 L2)
>((a b) b c d)

>(append L1 L2)
>(a b b c d)

>(list L1 L2)
>((a b) (b c d))

>(defun foo (x y)
        (let*   ((a 3)
                (b 7))
        (+ x y a b)))

>(setq x 5)
>(+ (let ((x 3))
        (+ x (* x 10)))
        x)
>38 

```
