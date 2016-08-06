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


class Point:
    def __init__(self, id, x, y):
        self.id = id
        self.x = x
        self.y = y

    def coords(self):
        return [self.x, self.y]

    def __repr__(self):
        return 'Point{0}[{1}, {2}]'.format(self.id, self.x, self.y)


class Mirror(Point):
    def __init__(self, id, x, y, source):
        super().__init__(id, x, y)
        self.source = source

    def __repr__(self):
        return 'Mirror{0}[{1}, {2}] {3}'.format(self.id, self.x, self.y, self.source)


class Fold(Point):
    def __init__(self, id, x, y, mirror, left, coefficient):
        super().__init__(id, x, y)
        self.mirror = mirror
        self.left = left
        self.coefficient = coefficient

    def __repr__(self):
        return 'Fold{0}[{1}, {2}] {3}_{4}_{5}'.format(self.id, self.x, self.y, self.mirror, self.left, self.coefficient)


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

    points = [Point(0, 0, 0), Point(1, 0, 1), Point(2, 1, 1), Point(3, 1, 0)]

    # WS edges contain edges
    edges = [[i, (i + 1) % len(points)] for i in range(len(points))]

    iteration = 0
    no_progress = 0
    while True:
        iteration += 1
        if iteration > 2:
            print("Iteration limit.", iteration - 1, "STOP")
            break

        print("ITERATION", iteration, "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
        print("Points", len(points), points)
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
            p0_inside = check_boundary(fold_edge, convex_center, p0.coords())
            p1_inside = check_boundary(fold_edge, convex_center, p1.coords())

            if p0_inside and p1_inside:
                # This edge shouldn't be touched
                edges_new.append(e)
                continue
            if not p0_inside and not p1_inside:
                # This edge should be mirrored
                p0_mirror = mirror(p0.x, p0.y, fold_edge[0][0], fold_edge[0][1], fold_edge[1][0], fold_edge[1][1])
                p1_mirror = mirror(p1.x, p1.y, fold_edge[0][0], fold_edge[0][1], fold_edge[1][0], fold_edge[1][1])
                # Add new edges and set origin
                points.append(Mirror(len(points), p0_mirror[0], p0_mirror[1], e[0]))
                points.append(Mirror(len(points), p1_mirror[0], p1_mirror[1], e[0]))
                # New edge ids
                edges_new.append([len(points) - 2, len(points) - 1])
                print("Edge", e, "mirrored to", edges_new[len(edges_new) - 1])

            # Simmerty trick
            if p0_inside and not p1_inside:
                t = p0
                p0 = p1
                p1 = t
                t = e[0]
                e[0] = e[1]
                e[1] = t
                p0_inside = check_boundary(fold_edge, convex_center, p0.coords())
                p1_inside = check_boundary(fold_edge, convex_center, p1.coords())

            if not p0_inside and p1_inside:
                # p1 should be left intact, p0 should be mirrored + new fold point added
                p0_mirror = mirror(p0.x, p0.y, fold_edge[0][0], fold_edge[0][1], fold_edge[1][0], fold_edge[1][1])
                p_fold = line_intersection(fold_edge, [p0.coords(), p1.coords()])
                p_fold_coeff = length([p0.coords(), p_fold]) / length([p0.coords(), p1.coords()])
                points.append(Mirror(len(points), p0_mirror[0], p0_mirror[1], e[0]))
                points.append(Fold(len(points), p_fold[0], p_fold[1], e[0], e[1], p_fold_coeff))
                fold_points.append(points[len(points) - 1])
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
                if i != j:
                    edges_new.append([i.id, j.id])

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
    print("WS edges", len(edges), edges)
    print("RESULT found in", iteration, "iterations")

    # Backtrace
    # backtrace(points, edges)


def visualize(iteration, edges, points):
    print("Visualize", iteration, edges, points)
    coords = [[points[e[0]].coords(), points[e[1]].coords()] for e in edges]

    fig = plt.figure()
    ax = fig.add_subplot(111)
    ax.add_collection(LineCollection(coords))
    ax.autoscale()
    ax.margins(0.1)
    plt.xlim((-1.5, 1.5))
    plt.ylim((-1.5, 1.5))
    plt.savefig("out/viz_{0}.png".format(iteration))


def backtrace(points, edges):
    print("Backtrace", points, edges)

    def is_fold_point(p):
        return type(points[p]) is Fold

    def is_mirror_point(p):
        return type(points[p]) is Mirror

    print("Fold points", [i for i in range(len(points)) if is_fold_point(i)])
    print("Mirror points", [i for i in range(len(points)) if is_mirror_point(i)])
    print("Start edges", edges)
    iteration = 1
    while True:
        print()
        print("Backtrace iteration", iteration)
        iteration += 1
        edges_prev = []
        edges_number = len(edges)
        for e in edges:
            p0 = e[0]
            p1 = e[1]
            # We deal only with fold edges
            if is_fold_point(p0) and is_fold_point(p1):
                fp0 = points[p0]
                fp1 = points[p1]
                print("Fold edge processing", e, fp0, fp1)

                # Add original edge
                edges_prev.append(e)
                print("Added edge to prev generation", e)

                # Compute duplicated edge
                p0_duplicated = p0
                p1_duplicated = p1

                if is_fold_point(fp0.mirror):
                    points.append(Fold(len(points), p0, 666, 666, fp0.mirror, fp0.left))
                    p0_duplicated = len(points) - 1

                if is_fold_point(fp1.mirror):
                    points.append(Fold(len(points), p1, 666, 666, fp1.mirror, fp1.left))
                    p1_duplicated = len(points) - 1

                if p0_duplicated != p0 or p1_duplicated != p1:
                    edges_prev.append([p0_duplicated, p1_duplicated])

        if edges_number != len(edges_prev):
            edges = edges_prev
            print("Edges", edges)
        else:
            break


if __name__ == "__main__":
    lines = sys.stdin.readlines()
    print("Lines\n", "".join(lines), file=sys.stderr)
    silhouette = parse(lines)
    print(str(silhouette))
    (polygons, _, _) = silhouette
    silhouette_points = np.array([[v[0], v[1]] for p in polygons for v in p])
    hull = ConvexHull(polygons[0])
    fold(silhouette_points[hull.vertices].tolist())
