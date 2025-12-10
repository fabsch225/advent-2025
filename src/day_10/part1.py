import sys
import re

def gf2_gauss(A, b):
    n = len(A)
    m = len(A[0])

    row = 0
    pivot_cols = []

    for col in range(m):
        pivot = None
        for r in range(row, n):
            if A[r][col] == 1:
                pivot = r
                break
        if pivot is None:
            continue

        A[row], A[pivot] = A[pivot], A[row]
        b[row], b[pivot] = b[pivot], b[row]
        pivot_cols.append(col)

        for r in range(n):
            if r != row and A[r][col] == 1:
                for c in range(col, m):
                    A[r][c] ^= A[row][c]
                b[r] ^= b[row]

        row += 1

    return row, pivot_cols, A, b

def solve_min_presses(A, b):
    n = len(A)
    m = len(A[0])

    A = [row[:] for row in A]
    b = b[:]

    rank, pivots, Ared, bred = gf2_gauss(A, b)
    pivset = set(pivots)

    for r in range(rank, n):
        if all(Ared[r][c] == 0 for c in range(m)) and bred[r] == 1:
            return None

    free_vars = [j for j in range(m) if j not in pivset]

    x0 = [0]*m
    for r,pv in enumerate(pivots):
        s = bred[r]
        for c in range(pv+1, m):
            if Ared[r][c] == 1:
                s ^= x0[c]
        x0[pv] = s

    basis = []
    for fv in free_vars:
        v = [0]*m
        v[fv] = 1
        for r,pv in enumerate(pivots):
            s = 0
            for c in range(pv+1, m):
                if Ared[r][c] == 1:
                    s ^= v[c]
            v[pv] = s
        basis.append(v)

    best = None
    k = len(basis)
    for mask in range(1<<k):
        x = x0[:]
        for i in range(k):
            if mask & (1<<i):
                x = [x[j] ^ basis[i][j] for j in range(m)]
        presses = sum(x)
        if best is None or presses < best:
            best = presses

    return best

def parse_line(line):
    pattern = re.search(r"\[([.#]+)\]", line).group(1)
    target = [1 if c == "#" else 0 for c in pattern]
    nlights = len(target)

    btns = re.findall(r"\(([\d,]+)\)", line)
    btns = [list(map(int, part.split(","))) for part in btns]

    m = len(btns)
    A = [[0]*m for _ in range(nlights)]
    for j, lst in enumerate(btns):
        for l in lst:
            A[l][j] ^= 1

    return A, target

def main():
    total = 0
    for line in sys.stdin:
        line = line.strip()
        if not line:
            continue
        A, target = parse_line(line)
        res = solve_min_presses(A, target)
        if res is None:
            print("NO SOLUTION for:", line)
            return
        total += res
    print(total)

if __name__ == "__main__":
    main()
