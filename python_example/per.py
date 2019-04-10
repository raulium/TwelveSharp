from itertools import combinations_with_replacement as cwr


def per(n, step=0): 
    sn = str(n)
    if len(sn) == 1:
        return step
    else:
        result = 1
        sub = [int(i) for i in sn]
        for s in sub:
            result *= s
        return per(result, step=step+1)


def seq_gen(n):
    p = [2, 3]
    q = [6, 7, 8, 9]
    r = []

    if n > 1:
        for i in p:
            r += [(i,) + j for j in cwr(q, n-1)]
        r += [i for i in cwr(q, n)]
        r = [int(''.join(str(k) for k in i)) for i in r]

    else:
        r = [2, 3, 6, 7, 8, 9]
    return r


def main():
    p = 0
    for i in range(0, 100):
        for j in seq_gen(i):
            x = per(j)
            if x >= p:
                p = x
                print "{num}:\t{per}".format(num=j, per=x)


if __name__ == '__main__':
    main()