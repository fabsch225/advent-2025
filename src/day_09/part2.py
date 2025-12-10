import sys
from functools import cmp_to_key

def read_points():
    pts = []
    for line in sys.stdin:
        s = line.strip()
        if not s:
            continue
        x,y = s.split(',')
        pts.append((int(x), int(y)))
    return pts

def calc_area(p1, p2):
    (x1,y1),(x2,y2) = p1,p2
    return (abs(x1-x2)+1) * (abs(y1-y2)+1)

def make_rect(p1, p2):
    (x1,y1),(x2,y2) = p1,p2
    return (min(x1,x2), max(x1,x2), min(y1,y2), max(y1,y2))

def intersects_boundary(edges, rect):
    xL,xR,yB,yT = rect
    for (ax,ay),(bx,by) in edges:
        if ax == bx:
            ey1, ey2 = min(ay,by), max(ay,by)
            if (ax > xL and ax < xR) and (max(yB, ey1) < min(yT, ey2)):
                return True
        elif ay == by:
            ex1, ex2 = min(ax,bx), max(ax,bx)
            if (ay > yB and ay < yT) and (max(xL, ex1) < min(xR, ex2)):
                return True
        else:
            pass
    return False

def is_inside_or_on_boundary(edges, px2, py2):
    on_boundary = False
    crossings = 0

    for (ax,ay),(bx,by) in edges:
        if on_boundary:
            return True

        ax2, ay2 = ax*2, ay*2
        bx2, by2 = bx*2, by*2

        if ax2 == bx2 and ax2 == px2:
            if py2 >= min(ay2, by2) and py2 <= max(ay2, by2):
                return True
        elif ay2 == by2 and ay2 == py2:
            if px2 >= min(ax2, bx2) and px2 <= max(ax2, bx2):
                return True

        if ax2 == bx2:
            yMin, yMax = min(ay2, by2), max(ay2, by2)
            if (px2 < ax2) and (py2 >= yMin) and (py2 < yMax):
                crossings += 1

    return (crossings % 2 == 1)

def rect_is_valid(edges, p1, p2):
    rect = make_rect(p1, p2)
    xL,xR,yB,yT = rect
    cx2 = xL + xR
    cy2 = yB + yT

    if intersects_boundary(edges, rect):
        return False
    return is_inside_or_on_boundary(edges, cx2, cy2)

def solve(points):
    n = len(points)
    edges = [ (points[i], points[(i+1)%n]) for i in range(n) ]

    pairs = []
    for i in range(n):
        for j in range(i+1, n):
            p1 = points[i]; p2 = points[j]
            area = calc_area(p1,p2)
            pairs.append(( -area, p1, p2))

    pairs.sort()

    for neg_area, p1, p2 in pairs:
        if rect_is_valid(edges, p1, p2):
            return -neg_area
    return 0

if __name__ == "__main__":
    pts = read_points()
    ans = solve(pts)
    print(ans)
