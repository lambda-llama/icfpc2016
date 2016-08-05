from itertools import chain

import matplotlib.pyplot as plt
from matplotlib.patches import PathPatch
from matplotlib.path import Path


def plot_poly(ax, points):
    codes = [Path.MOVETO]
    codes.extend([Path.LINETO] * (len(points) - 2))
    codes.append(Path.CLOSEPOLY)

    path = Path(points, codes)
    patch = PathPatch(path, lw=2)
    ax.add_patch(patch)


def plot_edge(ax, edge):
    xs, ys = zip(*edge)
    plt.plot(xs, ys, c="red", lw=2)


if __name__ == "__main__":
    facets = [
        [((1, 0), (1/2, 1/2)), ((1/2, 1/2), (0, 0)), ((0, 0), (1, 0))],
        [((0, 1/2), (0, 0)), ((0, 0), (1/2, 1/2)), ((1/2, 1/2), (0, 1/2))]
    ]
    edge = ((1, 0), (1/2, 1/2))

    fig = plt.figure()
    ax = fig.add_subplot(111)

    for facet in facets:
        first, *pairs, last = list(chain(*facet))
        plot_poly(ax, [first, *pairs[::2], last])

    plot_edge(ax, edge)

    plt.grid()
    plt.show()
