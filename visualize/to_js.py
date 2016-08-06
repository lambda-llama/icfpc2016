#!/usr/bin/env python
# Usage python to_js.py < problem.txt
# Result will be printed to stdout

import re
import sys
import numpy as np
from scipy.spatial import ConvexHull

import matplotlib.pyplot as plt
from matplotlib.collections import LineCollection
from math import sqrt


def parse_vertex(s):
    if "/" in s:
        return float(s.split("/")[0]) / int(s.split("/")[1])
    else:
        return float(s)


def length(edge):
    p0 = edge[0]
    p1 = edge[1]
    print("Edge p0", p0, "p1", p1)
    return sqrt((p1[0] - p0[0]) * (p1[0] - p0[0]) + (p1[1] - p0[1]) * (p1[1] - p0[1]))


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


def check_boundary(edge, point1, point2):
    """return true if points are on the same half planes"""
    xe = edge[1][0] - edge[0][0]
    ye = edge[1][1] - edge[0][1]
    x1 = point1[0] - edge[0][0]
    y1 = point1[1] - edge[0][1]
    x2 = point2[0] - edge[0][0]
    y2 = point2[1] - edge[0][1]
    cp1 = x1 * ye - y1 * xe
    cp2 = x2 * ye - y2 * xe
    result = cp1 * cp2 >= 0
    return result


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


def fold(convex):
    """
    Basic IDEA:
    All the loaded problems are already shifted to fit 1x1 square.
    LET Working set = 4 square points
    Iteratively for each edge in target convex check if any of the edges from Working set is in another semi plane.
    """
    print("Convex", convex)
    convex_center = [sum(map(lambda v: v[0], convex)) / len(convex), sum(map(lambda v: v[1], convex)) / len(convex)]
    print("Convex center", convex_center)

    # WSP coordinates of the points
    points = [[0, 0], [0, 1], [1, 1], [1, 0]]
    # Points origins
    origins = [0, 1, 2, 3]
    # WS edges contain edges
    edges = [[i, (i + 1) % len(points)] for i in range(len(points))]

    iteration = 0
    no_progress = 0
    while True:
        iteration += 1
        if iteration > 10:
            print("Iteration limit.", iteration - 1, "STOP")
            break

        print("ITERATION", iteration, "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
        print("Points", len(points), points)
        print("Origins", len(origins), origins)
        print("WS edges", len(edges), edges)

        # Edge to try folding on
        fold_edge = (convex[iteration % len(convex)], convex[(iteration + 1) % len(convex)])
        print("Edge", iteration % len(convex), fold_edge)

        edges_new = []
        fold_points = []
        n_points = len(points)
        for e in edges:
            p0 = points[e[0]]
            p1 = points[e[1]]
            check_p0 = check_boundary(fold_edge, convex_center, p0)
            check_p1 = check_boundary(fold_edge, convex_center, p1)

            if check_p0 and check_p1:
                # This edge shouldn't be touched
                edges_new.append(e)
                continue
            if not check_p0 and not check_p1:
                # This edge should be mirrored
                p0_mirror = mirror(p0[0], p0[1], fold_edge[0][0], fold_edge[0][1], fold_edge[1][0], fold_edge[1][1])
                p1_mirror = mirror(p1[0], p1[1], fold_edge[0][0], fold_edge[0][1], fold_edge[1][0], fold_edge[1][1])
                # Add new edges and set origins
                points.append(p0_mirror)
                origins.append([e[0], "m", iteration % len(convex)])
                points.append(p1_mirror)
                origins.append([e[1], "m", iteration % len(convex)])
                # New edge ids
                edges_new.append([len(points) - 2, len(points) - 1])
                print("Edge", e, "mirrored to", edges_new[len(edges_new) - 1])

            # Simmerty trick
            if check_p0 and not check_p1:
                t = p0
                p0 = p1
                p1 = t
                t = e[0]
                e[0] = e[1]
                e[1] = t
                check_p0 = check_boundary(fold_edge, convex_center, p0)
                check_p1 = check_boundary(fold_edge, convex_center, p1)

            if not check_p0 and check_p1:
                # p1 should be left intact, p0 should be mirrored + new fold point added
                p0_mirror = mirror(p0[0], p0[1], fold_edge[0][0], fold_edge[0][1], fold_edge[1][0], fold_edge[1][1])
                p_fold = line_intersection(fold_edge, [p0, p1])
                p_fold_coeff = length([p0, p_fold]) / length([p0, p1])
                points.append(p0_mirror)
                origins.append([e[0], "m", iteration % len(convex)])
                points.append(p_fold)
                fold_points.append(len(points) - 1)
                origins.append([e[0], e[1], "f", iteration % len(convex), p_fold_coeff])
                # New edges ids
                edges_new.append([len(points) - 2, len(points) - 1])
                edges_new.append([e[1], len(points) - 1])
                print("Edge", e, "mirrored to",
                      edges_new[len(edges_new) - 1], "and",
                      edges_new[len(edges_new) - 2])

        print("Fold points", fold_points)
        # Add edges combined of new folded points
        for i in fold_points:
            for j in fold_points:
                if i == j:
                    pass
                edges_new.append([i, j])

        if n_points == len(points):
            print("Edge", iteration % len(convex), "nothing changed")
            no_progress += 1
        else:
            no_progress = 0
        # Update edges and check for progress
        edges = edges_new
        if no_progress == len(convex):
            print("No progress for", no_progress, "steps. STOP.")
            break
        visualize(iteration, edges, points)

    print("Points", len(points), points)
    print("Origins", len(origins), origins)
    print("WS edges", len(edges), edges)
    print("RESULT found in", iteration, "iterations")

    # Backtrace
    # backtrace(points, origins, edges)


def visualize(iteration, edges, points):
    coords = [[points[v[0]], points[v[1]]] for v in edges]
    fig = plt.figure()
    ax = fig.add_subplot(111)
    ax.add_collection(LineCollection(coords))
    ax.autoscale()
    ax.margins(0.1)
    plt.xlim((-1.5, 1.5))
    plt.ylim((-1.5, 1.5))
    plt.savefig("out/viz_{0}.png".format(iteration))


def backtrace(points, origins, edges):
    """Perform backtrace"""

    def is_fold_point(p):
        return hasattr(origins[p], "__len__") and 'f' in origins[p]

    def is_mirror_point(p):
        return hasattr(origins[p], "__len__") and 'm' in origins[p]

    def is_fold_edge(e):
        return is_fold_point(e[0]) or is_fold_point(e[1])

    print("Fold points", [i for i in range(len(points)) if is_fold_point(i)])
    print("Mirror points", [i for i in range(len(points)) if is_mirror_point(i)])
    print("Fold edges", [i for i in range(len(edges)) if is_fold_edge(edges[i])])

    def point_prev(p):
        if is_mirror_point(p):
            return origins[p][0]
        else:
            return origins[p]

    print()
    print("Start edges", edges)
    iteration = 1
    result = []
    while True:
        print()
        print("Backtrace iteration", iteration)
        iteration += 1
        edges_prev = []
        changed = False
        for e in edges:
            p0 = e[0]
            p1 = e[1]
            if is_mirror_point(p0) and is_mirror_point(p1):
                # Ignore this
                pass
            if is_fold_point(p0) and is_fold_point(p1):
                # Check if any of the points were mirror ones, then replicate it, else put to result

                if is_fold_point(origins[p0][0]) or is_mirror_point(origins[p0][1]) or \
                        is_mirror_point(origins[p1][0]) or is_mirror_point(origins[p1][1]):
                    pass

            if not is_fold_edge(e):
                edges_prev.append([point_prev(p0), point_prev(p1)])
            else:
                changed = True
        edges = edges_prev
        print("Edges", edges)
        if not changed:
            break

    print("Final edges", edges)


if __name__ == "__main__":
    lines = sys.stdin.readlines()
    print("Lines\n", "".join(lines), file=sys.stderr)
    silhouette = parse(lines)
    print(str(silhouette))
    (polygons, _, _) = silhouette
    silhouette_points = np.array([[v[0], v[1]] for p in polygons for v in p])
    hull = ConvexHull(polygons[0])
    fold(silhouette_points[hull.vertices].tolist())
