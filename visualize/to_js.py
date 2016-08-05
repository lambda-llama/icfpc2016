import re

problem50 = """1
9
25/29,49/58
135/203,61/58
211/290,164/145
35/58,259/232
15/29,35/29
99/232,65/58
44/145,331/290
21/58,215/203
9/58,25/29
15
59/116,99/116 35/58,259/232
59/116,99/116 15/29,35/29
59/116,99/116 211/290,164/145
159/232,197/232 45/58,217/232
59/116,99/116 99/232,65/58
77/232,199/232 119/232,239/232
9/58,25/29 15/29,35/29
59/116,99/116 44/145,331/290
77/232,199/232 57/232,55/58
159/232,197/232 119/232,239/232
25/29,49/58 15/29,35/29
25/29,49/58 9/58,25/29
45/58,217/232 57/232,55/58
35/58,259/232 211/290,164/145
99/232,65/58 44/145,331/290
"""

problem1 = """1
4
0,0
1,0
1,1
0,1
4
0,0 1,0
0,0 0,1
1,0 1,1
0,1 1,1
"""


def parse_vertex(s):
    if "/" in s:
        return float(s.split("/")[0]) / int(s.split("/")[1])
    else:
        return float(s)


def to_js(lines):
    result = "[\n"
    polygons = int(lines[0])
    print("Polygons", polygons)
    cursor = 1
    for p in range(polygons):
        if p > 0:
            result += "\n, "
        result += "["
        vertices = int(lines[cursor])
        print("Vertices", vertices)
        cursor += 1

        for v in range(vertices):
            if v > 0:
                result += ", "
            line = lines[cursor]
            cursor += 1
            coords = re.split("[ ,]", line)
            print("Coords: ", coords)
            x = parse_vertex(coords[0])
            y = parse_vertex(coords[1])
            result += "[{0}, {1}]".format(x, y)

        result += "],\n[],\n["  # Splitter between polygons and skeleton

        skeletons = int(lines[cursor])
        print("Skeletons", skeletons)
        cursor += 1

        for s in range(skeletons):
            if s > 0:
                result += ", "
            line = lines[cursor]
            cursor += 1
            coords = re.split("[ ,]", line)
            print("Coords: ", coords)
            x1 = parse_vertex(coords[0])
            y1 = parse_vertex(coords[1])
            x2 = parse_vertex(coords[2])
            y2 = parse_vertex(coords[3])
            result += "[[ {0}, {1}], [{2}, {3}]]".format(x1, y1, x2, y2)
        result += "]\n]"

        return result


print("Result\n", to_js(problem50.splitlines()))
