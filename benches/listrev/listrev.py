import sys
sys.setrecursionlimit(50_000)

def ll_range(n, acc):
    if n == 0:
        return acc
    else:
        return ll_range(n - 1, (n, acc))

def ll_reverse(l, acc):
    match l:
        case (): 
            return acc
        case (hd, tl):
            return ll_reverse(tl, (hd, acc))

for _ in range(500):
    ls = ll_range(10_000, ())
    rev = ll_reverse(ls, ())
