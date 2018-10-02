# Lecture 2 
## 10/2 

## Lisp Expression 

    * accessors: car (first) cdr (rest)
    *Constructors: cons, list  
* Boolean Expression 
* Conditions 
* Functions


* NIL ()
* t 
* (> 1 3)
* (<= 3 7)

* (numberp)
* (symblep)
* (listp)
* (atom)

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
                (t (* B ()))))
```


