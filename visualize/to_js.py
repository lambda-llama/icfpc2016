#!/usr/bin/env python
# Usage python to_js.py < problem.txt
# Result will be printed to stdout

import re
import sys
import numpy as np
from scipy.spatial import ConvexHull
import functools


def parse_vertex(s):
    if "/" in s:
        return float(s.split("/")[0]) / int(s.split("/")[1])
    else:
        return float(s)


def parse(lines):
    polygons_number = int(lines[0])
    polygons = []
    print("Polygons nubmer", polygons_number, file=sys.stderr)
    cursor = 1
    for p in range(polygons_number):
        polygon = []
        vertices = int(lines[cursor])
        print("Polygon {0} Vertices {1}".format(p, vertices), file=sys.stderr)
        cursor += 1
        for v in range(vertices):
            line = lines[cursor]
            cursor += 1
            coords = re.split("[ ,]", line)
            print("Vertex {0} Coords: {1}".format(v, coords), file=sys.stderr)
            x = parse_vertex(coords[0])
            y = parse_vertex(coords[1])
            polygon.append([x, y])
        polygons.append(polygon)

    skeletons_number = int(lines[cursor])
    skeletons = []
    print("Skeletons", skeletons_number, file=sys.stderr)
    cursor += 1

    for s in range(skeletons_number):
        line = lines[cursor]
        cursor += 1
        coords = re.split("[ ,]", line)
        print("Vertex {0} Coords: {1}".format(s, coords), file=sys.stderr)
        x1 = parse_vertex(coords[0])
        y1 = parse_vertex(coords[1])
        x2 = parse_vertex(coords[2])
        y2 = parse_vertex(coords[3])
        skeletons.append([[x1, y1], [x2, y2]])
    max_x = max(map(lambda polygon: max(map(lambda vertex: vertex[0], polygon)), polygons))
    min_x = min(map(lambda polygon: min(map(lambda vertex: vertex[0], polygon)), polygons))
    max_y = max(map(lambda polygon: max(map(lambda vertex: vertex[1], polygon)), polygons))
    min_y = min(map(lambda polygon: min(map(lambda vertex: vertex[1], polygon)), polygons))
    print("X in [{0}, {1}], Y in [{2}, {3}]".format(min_x, min_y, max_x, max_y), file=sys.stderr)
    rescaled_polygons = [[
                             [v[0] - min_x, v[1] - min_y]
                             for v in p] for p in polygons]
    rescaled_skeletons = [[
                              [e[0][0] - min_x, e[0][1] - min_y],
                              [e[1][0] - min_x, e[1][1] - min_y]]
                          for e in skeletons]

    return [rescaled_polygons, [], rescaled_skeletons]


def square(convex):
    s = 0
    ps = len(convex)
    for i in range(0, ps):
        s += convex[i][0] * convex[(i + 1) % ps][1] - convex[i][1] * convex[(i + 1) % ps][0]
    return abs(s / 2)


def check_boundary(edge, point1, point2):
    """return true if points are on the different semi planes"""
    xe = edge[1][0] - edge[0][0]
    ye = edge[1][1] - edge[0][1]
    x1 = point1[0] - edge[0][0]
    y1 = point1[1] - edge[0][1]
    x2 = point2[0] - edge[0][0]
    y2 = point2[1] - edge[0][1]
    cp1 = x1 * ye - y1 * xe
    cp2 = x2 * ye - y2 * xe
    # print("Xe", xe, "Ye", ye)
    # print("X1", x1, "Y1", y1)
    # print("X2", x2, "Y2", y2)
    result = cp1 * cp2 < 0
    # print("Boundary check", result)
    return result


def fold(convex):
    """
    Basic IDEA:
    All the loaded problems are already shifted to fit 1x1 square.
    LET Working set = 4 square points
    Iteratively for each edge in target convex check if any of the points from Working set is in another semi plane.
    If there is one, let fold, update Working set.
    Iterate while we can. As a result we get number of lines.
    """
    print("Convex", convex)
    convex_center = [sum(map(lambda v: v[0], convex)) / len(convex), sum(map(lambda v: v[1], convex)) / len(convex)]
    print("Convex center", convex_center)
    print("Square", square(convex))
    ws = [[0, 0], [0, 1], [1, 1], [1, 0]]
    i = 0
    size = len(convex)
    delta = square(ws) - square(convex)

    while abs(delta) > 0.1:
        i += 1
        if i > 50:
            break

        print("Iteration", i)
        print("WS", ws)
        print("Delta", delta)
        edge = (convex[i % size], convex[(i + 1) % size])
        print("Try edge", i % size, edge)

        ws_len = len(ws)
        i_to_process = [v for v in range(ws_len) if check_boundary(edge, ws[v], convex_center)]
        if len(i_to_process) == 0:
            continue

        points_else = [ws[i] for i in range(ws_len) if i not in i_to_process]
        print("Points to stay", points_else)

        points_to_mirror = [ws[i] for i in i_to_process]
        print("Points to mirror", points_to_mirror)

        mirrored = [mirror(v[0], v[1], edge[0][0], edge[0][1], edge[1][0], edge[1][1]) for v in points_to_mirror]
        print("Mirrored", mirrored)

        i_min = min(i_to_process)
        i_max = max(i_to_process)
        ws_prev_edge = [ws[(i_min - 1) % ws_len], ws[i_min]]
        ws_next_edge = [ws[(i_max + 1) % ws_len], ws[i_max]]
        print("Prev edge to intersect", ws_prev_edge)
        print("Next edge to intersect", ws_next_edge)
        try:
            point_intersection = [line_intersection(edge, ws_prev_edge), line_intersection(edge, ws_next_edge)]
        except:
            pass
        print("Intersections", point_intersection)

        ws_new = np.array(points_else + mirrored + point_intersection)
        hull = ConvexHull(ws_new)
        ws = remove_duplicates(ws_new[hull.vertices].tolist())
        delta = square(ws) - square(convex)
    print("Result found in", i, "iterations", "delta", delta, "approximation", ws)


def remove_duplicates(ws_new):
    ws = []
    p = [-1000, 1000]
    for i in range(len(ws_new)):
        if abs(p[0] - ws_new[i][0]) > 1e-3 or abs(p[1] - ws_new[i][1]) > 1e-3:
            ws.append(ws_new[i])
            p = ws_new[i]
    return ws


def mirror(p_x, p_y, x0, y0, x1, y1):
    dx = (x1 - x0)
    dy = (y1 - y0)
    a = (dx * dx - dy * dy) / (dx * dx + dy * dy)
    b = 2 * dx * dy / (dx * dx + dy * dy)
    x = a * (p_x - x0) + b * (p_y - y0) + x0
    y = b * (p_x - x0) - a * (p_y - y0) + y0
    return [x, y]


def line_intersection(line1, line2):
    dx = (line1[0][0] - line1[1][0], line2[0][0] - line2[1][0])
    dy = (line1[0][1] - line1[1][1], line2[0][1] - line2[1][1])  # Typo was here

    def det(a, b):
        return a[0] * b[1] - a[1] * b[0]

    div = det(dx, dy)
    if div == 0:
        raise Exception('lines do not intersect')

    d = (det(*line1), det(*line2))
    x = det(d, dx) / div
    y = det(d, dy) / div
    return [x, y]


if __name__ == "__main__":
    lines = sys.stdin.readlines()
    print("Lines\n", "".join(lines), file=sys.stderr)
    silhouette = parse(lines)
    print(str(silhouette))
    (polygons, _, _) = silhouette
    points = np.array([[v[0], v[1]] for p in polygons for v in p])
    hull = ConvexHull(polygons[0])
    convex_points = points[hull.vertices]
    fold(convex_points)
