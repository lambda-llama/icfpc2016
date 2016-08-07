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
    dy = (line1[0][1] - line1[1][1], line2[0][1] - line2[1][1])

    def det(a, b):
        return a[0] * b[1] - a[1] * b[0]

    div = det(dx, dy)
    if div == 0:
        raise Exception('lines do not intersect')

    d = (det(*line1), det(*line2))
    x = det(d, dx) / div
    y = det(d, dy) / div
    return [x, y]


class Point:
    counter = 0

    def __init__(self, x, y, z):
        self.id = Point.counter
        Point.counter += 1
        self.x = x
        self.y = y
        self.z = z

    def coords(self):
        return [self.x, self.y]

    def z(self):
        return self.z

    def __repr__(self):
        return 'P{0}[{1}, {2}]'.format(self.id, self.x, self.y)


class Mirror(Point):
    def __init__(self, x, y, z, source):
        super().__init__(x, y, z)
        self.source = source

    def __repr__(self):
        return 'M{0}[{1}, {2}]'.format(self.id, self.x, self.y)


class Fold(Point):
    def __init__(self, x, y, z, start, end, coefficient):
        super().__init__(x, y, z)
        self.start = start
        self.end = end
        self.coefficient = coefficient

    def __repr__(self):
        return 'F{0}[{1}, {2}]'.format(self.id, self.x, self.y)


class Edge:
    def __init__(self, start, end):
        self.start = start
        self.end = end

    def start(self):
        return self.start

    def end(self):
        return self.end

    def __repr__(self):
        return '({0}-{1})'.format(self.start, self.end)


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

    p00 = Point(0, 0, 0)
    p01 = Point(0, 1, 0)
    p11 = Point(1, 1, 0)
    p10 = Point(1, 0, 0)
    edges = [Edge(p00, p01), Edge(p01, p11), Edge(p11, p10), Edge(p10, p00)]

    iteration = 0
    no_progress = 0
    while True:
        iteration += 1
        if iteration > 2:
            print("Iteration limit.", iteration - 1, "STOP")
            break
        print()
        print("ITERATION", iteration, "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
        print("Edges", len(edges), edges)

        # Edge to try folding on
        fold_edge = (convex[iteration % len(convex)], convex[(iteration + 1) % len(convex)])
        print("FOLD edge", iteration % len(convex), fold_edge)

        edges_new = []
        fold_points = []
        n_points = Point.counter
        for e in edges:
            p0 = e.start
            p1 = e.end
            p0_inside = check_boundary(fold_edge, convex_center, p0.coords())
            p1_inside = check_boundary(fold_edge, convex_center, p1.coords())

            if p0_inside and p1_inside:
                # This edge shouldn't be touched
                edges_new.append(e)
                continue
            if not p0_inside and not p1_inside:
                # This edge should be mirrored
                p0_mirror_coords = mirror(p0.x, p0.y, fold_edge[0][0], fold_edge[0][1], fold_edge[1][0],
                                          fold_edge[1][1])
                p1_mirror_coords = mirror(p1.x, p1.y, fold_edge[0][0], fold_edge[0][1], fold_edge[1][0],
                                          fold_edge[1][1])
                # Add new edges and set origin
                p0_mirror = Mirror(p0_mirror_coords[0], p0_mirror_coords[1], iteration, p0)
                p1_mirror = Mirror(p1_mirror_coords[0], p1_mirror_coords[1], iteration, p1)
                # New edge ids
                edges_new.append(Edge(p0_mirror, p1_mirror))
                print("Edge", e, "mirrored to", edges_new[len(edges_new) - 1])

            # Simmerty trick
            if p0_inside and not p1_inside:
                t = p0
                p0 = p1
                p1 = t
                p0_inside = False
                p1_inside = True

            if not p0_inside and p1_inside:
                # p1 should be left intact, p0 should be mirrored + new fold point added
                p0_mirror_coords = mirror(p0.x, p0.y,
                                          fold_edge[0][0], fold_edge[0][1],
                                          fold_edge[1][0], fold_edge[1][1])
                p0_mirror = Mirror(p0_mirror_coords[0], p0_mirror_coords[1], iteration, p0)
                fold_coords = line_intersection(fold_edge, [p0.coords(), p1.coords()])
                if fold_coords == p1.coords():
                    fold_point = p1
                else:
                    coefficient = length([p0.coords(), fold_coords]) / length([p0.coords(), p1.coords()])
                    fold_point = Fold(fold_coords[0], fold_coords[1], iteration, p0, p1, coefficient)
                fold_points.append(fold_point)
                # New edges
                edges_new.append(Edge(fold_point, p0_mirror))
                if fold_point != p1:
                    edges_new.append(Edge(fold_point, p1))
                    print("Edge", e, "mirrored to", edges_new[len(edges_new) - 1], "and", edges_new[len(edges_new) - 2])
                else:
                    print("Edge", e, "mirrored to", edges_new[len(edges_new) - 1])

        print("New fold points", fold_points)
        # Add edges combined of new folded points
        for i in fold_points:
            for j in fold_points:
                if i != j:
                    edges_new.append(Edge(i, j))

        if n_points == Point.counter:
            print("Edge", iteration % len(convex), "nothing changed")
            no_progress += 1
        else:
            no_progress = 0
        # Update edges and check for progress
        edges = edges_new
        if no_progress == len(convex):
            print("No progress for", no_progress, "steps. STOP.")
            break
        visualize("fold_{}".format(iteration), edges)

    print()
    print("Edges", len(edges), edges)
    print("RESULT found in", iteration, "iterations")

    # Backtrace
    backtrace(edges, iteration)


def visualize(iteration, edges):
    # print("Visualize", iteration, edges)
    coords = [[e.start.coords(), e.end.coords()] for e in edges]
    fig = plt.figure()
    ax = fig.add_subplot(111)
    ax.add_collection(LineCollection(coords))
    ax.margins(0.1)
    plt.xlim((-0.5, 1.5))
    plt.ylim((-0.5, 1.5))
    plt.savefig("out/{0}.png".format(iteration))


def is_fold_point(p):
    return type(p) is Fold


def is_mirror_point(p):
    return type(p) is Mirror


def backtrace(edges, iterations):
    print("Backtrace", edges)
    iteration = iterations
    while iteration > 0:
        print()
        iteration -= 1
        print("Backtrace iteration", iteration)
        edges_prev = []

        for e in [e for e in edges if e.start.z == iteration or e.end.z == iteration]:
            p0 = e.start
            p1 = e.end
            # We deal only with fold edges
            if is_fold_point(p0) or is_fold_point(p1):
                print("Fold edge", e)
                p0_duplicated = duplicate_fold_point(p0, iteration - 1)
                p1_duplicated = duplicate_fold_point(p1, iteration - 1)
                if p0_duplicated != p0 or p1_duplicated != p1:
                    edges_prev.append(Edge(p0_duplicated, p1_duplicated))
                    print("Duplicated edge to prev generation", edges_prev[len(edges_prev) - 1])
                else:
                    edges_prev.append(e)
                    print("Edge to prev generation", e)
        edges = edges_prev
        visualize("unfold_{}".format(iteration), edges)
    print("Edges", edges)


def multiply(k, p):
    return p[0] * k, p[1] * k


def plus(p0, p1):
    return p0[0] + p1[0], p0[1] + p1[1]


def minus(p0, p1):
    return p0[0] - p1[0], p0[1] - p1[1]


def duplicate_fold_point(p, iteration):
    if not is_fold_point(p):
        return p
    if is_mirror_point(p.end):
        x, y = minus(p.coords(), multiply(p.coefficient / (1 - p.coefficient), minus(p.end.coords(), p.coords())))
        return Fold(x, y, iteration, p.start, p.end.source, p.coefficient)
    return p


if __name__ == "__main__":
    lines = sys.stdin.readlines()
    print("Lines\n", "".join(lines), file=sys.stderr)
    silhouette = parse(lines)
    print(str(silhouette))
    (polygons, _, _) = silhouette
    silhouette_points = np.array([[v[0], v[1]] for p in polygons for v in p])
    hull = ConvexHull(polygons[0])
    fold(silhouette_points[hull.vertices].tolist())
