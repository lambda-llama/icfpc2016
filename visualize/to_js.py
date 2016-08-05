#!/usr/bin/env python
# Usage python to_js.py < problem.txt
# Result will be printed to stdout

import re
import sys


def parse_vertex(s):
    if "/" in s:
        return float(s.split("/")[0]) / int(s.split("/")[1])
    else:
        return float(s)


def to_js(lines):
    result = "[\n"
    polygons = int(lines[0])
    print("Polygons", polygons, file=sys.stderr)
    cursor = 1
    result += "["
    for p in range(polygons):
        if p > 0:
            result += "\n, "
        result += "["
        vertices = int(lines[cursor])
        print("Polygon {0} Vertices {1}".format(p, vertices), file=sys.stderr)
        cursor += 1

        for v in range(vertices):
            if v > 0:
                result += ", "
            line = lines[cursor]
            cursor += 1
            coords = re.split("[ ,]", line)
            print("Vertex {0} Coords: {1}".format(v, coords), file=sys.stderr)
            x = parse_vertex(coords[0])
            y = parse_vertex(coords[1])
            result += "[{0}, {1}]".format(x, y)
        result += "]"

    result += "],\n[],\n["  # Splitter between polygons and skeleton

    skeletons = int(lines[cursor])
    print("Skeletons", skeletons, file=sys.stderr)
    cursor += 1

    for s in range(skeletons):
        if s > 0:
            result += ", "
        line = lines[cursor]
        cursor += 1
        coords = re.split("[ ,]", line)
        print("Vertex {0} Coords: {1}".format(s, coords), file=sys.stderr)
        x1 = parse_vertex(coords[0])
        y1 = parse_vertex(coords[1])
        x2 = parse_vertex(coords[2])
        y2 = parse_vertex(coords[3])
        result += "[[ {0}, {1}], [{2}, {3}]]".format(x1, y1, x2, y2)
    result += "]\n]"

    return result


lines = sys.stdin.readlines()
print("Lines\n", "".join(lines), file=sys.stderr)
print(to_js(lines))
