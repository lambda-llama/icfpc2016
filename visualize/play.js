function fold(l, z, f) {
    for (var i = 0; i < l.length; i++) z = f(z, l[i]);
    return z;
}

function push(l, x) {
    l = l.concat();
    l.push(x);
    return l;
}

var problems = {
    sample: [
        [
            [[0, 0], [1, 0], [.5, .5], [0, .5]] // Polygon 1
        ],
        [], // delimiter
        [
            [[.5, .5], [0, 0]],
            [[0, 0], [1, 0]], [[1, 0], [.5, .5]],
            [[.5, .5], [0, .5]],
            [[0, .5], [0, 0]] // edges
        ]
    ],
    problem1: [
        [[0.0, 0.0], [1.0, 0.0], [1.0, 1.0], [0.0, 1.0]],
        [],
        [[[0.0, 0.0], [1.0, 0.0]], [[0.0, 0.0], [0.0, 1.0]], [[1.0, 0.0], [1.0, 1.0]], [[0.0, 1.0], [1.0, 1.0]]]
    ],
    problem50: [
        [[-0.2888878685571777, -0.3053597229074664], [-0.17886960198780746, -0.11415065364456083], [0.09226433380942664, 0.0], [0.02992445194111142, 0.30873084353832303], [-1.0507810648965166, 0.3756217072497429], [-1.155651037515134, 0.1933602475548775], [-0.9159908438079517, 0.055464073597302155], [-0.83091689060389, -0.1466060418999827], [-0.4724420761531618, -0.19974592634921814]],
        [],
        [[[-0.2888878685571777, -0.3053597229074664], [-1.155651037515134, 0.1933602475548775]], [[-0.4056836786131572, -0.20964211260355142], [-0.6884214871367693, 0.025056198989353226]], [[-0.83091689060389, -0.1466060418999827], [-1.0507810648965166, 0.3756217072497429]], [[-0.4056836786131572, -0.20964211260355142], [-0.83091689060389, -0.1466060418999827]], [[-0.4056836786131572, -0.20964211260355142], [0.09226433380942664, 0.0]], [[0.0, 0.0], [0.09226433380942664, 0.0]], [[0.0, 0.0], [0.0, 0.3105830339872893]], [[-0.018047081263651413, 0.1653549726760543], [0.0, 0.3105830339872893]], [[0.09226433380942664, 0.0], [0.02992445194111142, 0.30873084353832303]], [[-0.018047081263651413, 0.1653549726760543], [0.02992445194111142, 0.30873084353832303]], [[-0.2888878685571777, -0.3053597229074664], [-0.018047081263651413, 0.1653549726760543]], [[0.02992445194111142, 0.30873084353832303], [-1.0507810648965166, 0.3756217072497429]], [[-0.83091689060389, -0.1466060418999827], [-0.6884214871367693, 0.025056198989353226]], [[-1.155651037515134, 0.1933602475548775], [-1.0507810648965166, 0.3756217072497429]]]
    ],
        convex1: [[[[0, 0], [1, 0], [1, 1], [0, 1]]], [], [[[0, 1], [0, 0]], [[1, 0], [1, 1]], [[0, 0], [1, 0]], [[1, 1], [0, 1]]]],
     convex2: [[[[0, 0], [1, 0], [1, 1], [0, 1]]], [], [[[0, 1], [0, 0]], [[1, 0], [1, 1]], [[0, 0], [1, 0]], [[1, 1], [0, 1]]]],
     donut: [[[[0, 1 / 8], [1 / 8, 0], [.25, 0], [3 / 8, 1 / 8], [3 / 8, .25], [.25, 3 / 8], [1 / 8, 3 / 8], [0, .25]]], [[[1 / 8, 1 / 8], [1 / 8, .25], [.25, .25], [.25, 1 / 8]]], [[[3 / 8, 1 / 8], [0, 1 / 8]], [[1 / 8, 3 / 8], [1 / 8, 0]], [[1 / 8, 0], [.25, 0]], [[.25, 3 / 8], [1 / 8, 3 / 8]], [[.25, 0], [3 / 8, 1 / 8]], [[3 / 8, .25], [.25, 3 / 8]], [[0, .25], [3 / 8, .25]], [[3 / 8, .25], [3 / 8, 1 / 8]], [[.25, 0], [.25, 3 / 8]], [[1 / 8, 3 / 8], [0, .25]], [[0, .25], [0, 1 / 8]], [[1 / 8, 0], [0, 1 / 8]]]],

     crane: [[[[.16, .72], [1831 / 6745, 4749 / 6745], [49 / 195, 224 / 585], [.35, .35], [224 / 585, 49 / 195], [.84, .28], [448 / 895, 294 / 895], [168 / 295, 441 / 1180], [.7, .7], [441 / 1180, 168 / 295], [294 / 895, 448 / 895], [.3, .7], [277 / 1015, 738 / 1015]]], [], [[[5603 / 14650, 2777 / 7325], [39221 / 111815, 38878 / 111815]], [[112 / 265, 147 / 530], [5603 / 14650, 2777 / 7325]], [[14 / 41, 147 / 410], [.7, .7]], [[.7, .7], [168 / 295, 441 / 1180]], [[.35, .35], [.84, .28]], [[49 / 195, 224 / 585], [.35, .35]], [[147 / 415, 28 / 83], [21 / 58, 10 / 29]], [[39221 / 111815, 38878 / 111815], [.84, .28]], [[.35, .35], [.3, .7]], [[5603 / 14650, 2777 / 7325], [168 / 295, 441 / 1180]], [[.5, 21 / 64], [.5, .5]], [[21 / 58, 10 / 29], [116 / 273, 29 / 104]], [[2777 / 7325, 5603 / 14650], [38878 / 111815, 39221 / 111815]], [[21 / 64, .5], [2777 / 7325, 5603 / 14650]], [[112 / 265, 147 / 530], [147 / 530, 112 / 265]], [[224 / 585, 49 / 195], [.84, .28]], [[28 / 83, 147 / 415], [188 / 635, 447 / 635]], [[116 / 273, 29 / 104], [5603 / 14650, 2777 / 7325]], [[277 / 1015, 738 / 1015], [.3, .7]], [[147 / 410, 14 / 41], [.7, .7]], [[50378 / 168335, 117957 / 168335], [38878 / 111815, 39221 / 111815]], [[5603 / 14650, 2777 / 7325], [.5, 21 / 64]], [[.84, .28], [147 / 415, 28 / 83]], [[10 / 29, 21 / 58], [28 / 83, 147 / 415]], [[.7, .7], [441 / 1180, 168 / 295]], [[168 / 295, 441 / 1180], [224 / 585, 49 / 195]], [[.16, .72], [188 / 635, 447 / 635]], [[.5, .5], [21 / 64, .5]], [[49 / 195, 224 / 585], [441 / 1180, 168 / 295]], [[2777 / 7325, 5603 / 14650], [147 / 530, 112 / 265]], [[441 / 1180, 168 / 295], [2777 / 7325, 5603 / 14650]], [[50378 / 168335, 117957 / 168335], [.16, .72]], [[29 / 104, 116 / 273], [2777 / 7325, 5603 / 14650]], [[224 / 585, 49 / 195], [.35, .35]], [[29 / 104, 116 / 273], [10 / 29, 21 / 58]], [[.16, .72], [277 / 1015, 738 / 1015]], [[.3, .7], [.16, .72]], [[.5, .5], [.35, .35]], [[49 / 195, 224 / 585], [277 / 1015, 738 / 1015]]]]
};

/**
 * @param {Object} t
 * @param {Object} n
 * @param {number} dataAndEvents
 * @return {undefined}
 */
function Z(t, n, dataAndEvents) {
    if (t) {
        var point = F(n, t);
        var x = W(R(t, n), [2, 0]);
        var l;
        var b;
        /** @type {number} */
        x = point[0] * x[0] + point[1] * x[1];
        if (point[0]) {
            /** @type {Array} */
            l = [x / point[0], 0];
            /** @type {Array} */
            b = [(x - point[1]) / point[0], 1];
        } else {
            if (!point[1]) {
                return;
            }
            /** @type {Array} */
            l = [0, x / point[1]];
            /** @type {Array} */
            b = [1, x / point[1]];
        }
        if (L(F(n, t), N(F(l, t)))[1] >= 0) {
            /** @type {Array} */
            x = l;
            /** @type {Array} */
            l = b;
            /** @type {Array} */
            b = x;
        }
        x = fold(I.concat().reverse(), [], function (log, dataAndEvents) {
            var matched = C(dataAndEvents[1], [[], []], function (kv, a, dataAndEvents) {
                var err = push(kv[0], a);
                var typePattern = F(b, l);
                var n = W(F(dataAndEvents[1], a[1]), typePattern);
                return n[1] != 0 && (n = W(F(a[1], l), typePattern)[1] / -n[1], 0 <= n && n < 1) ? (n = R(a[1], L(F(dataAndEvents[1], a[1]), [n, 0])), n = [R(L(F(dataAndEvents[0], a[0]), W(F(n, a[1]), F(dataAndEvents[1], a[1]))), a[0]), n], [push(kv[1], n), push(err, n)]) : [err, kv[1]];
            });
            matched = matched[1].length ? (L(F(b, l), N(F(matched[0][0][1], l)))[1] >= 0 && (matched = matched.reverse()), U(matched, function (e) {
                return [dataAndEvents[0], C(e, [], function (err, nil, dataAndEvents) {
                    return nil[1][0] == dataAndEvents[1][0] && (nil[1][1] == dataAndEvents[1][1] && (nil[0][0] == dataAndEvents[0][0] && nil[0][1] == dataAndEvents[0][1])) ? err : push(err, nil);
                })];
            })) : L(F(b, l), N(F(dataAndEvents[1][0][1], l)))[1] >= 0 ? [0, dataAndEvents] : [dataAndEvents];
            if (matched[0]) {
                log.unshift(matched[0]);
            }
            if (matched[1]) {
                var y = F(b, l);
                y = W(y, N(y));
                log = push(log, [!matched[1][0], U(matched[1][1], function (dataAndEvents) {
                    return [dataAndEvents[0], R(L(N(F(dataAndEvents[1], l)), y), l)];
                })]);
            }
            return log;
        });
        if (dataAndEvents) {
            B(x);
            var _ctx = $("#dst")[0].getContext("2d");
            O([t, n], function (dataAndEvents) {
                /** @type {string} */
                _ctx.fillStyle = "#000000";
                _ctx.fillRect(dataAndEvents[0] - 1 / 128, dataAndEvents[1] - 1 / 128, 1 / 64, 1 / 64);
            });
        } else {
            G.push(I);
            I = x;
            /** @type {number} */
            E = 0;
            B(I);
        }
    }
}
/**
 * @param {Object} x
 * @return {undefined}
 */
function B(x) {
    Q(x, 1);
    Q(x, 0);
}
/**
 * @param {Array} l
 * @param {Array} c
 * @return {?}
 */
function F(l, c) {
    return [l[0] - c[0], l[1] - c[1]];
}
/**
 * @param {string} d
 * @return {?}
 */
function T(d) {
    return $("#origami #" + d + " li.active").data(d + "-id");
}
/**
 * @param {Array} protos
 * @return {undefined}
 */
function Y(protos) {
    if (protos.length == 0) {
        $("#flip").click(function () {
            I = E ? G.pop() : (G.push(I), U(I, function (dataAndEvents) {
                return [!dataAndEvents[0], U(dataAndEvents[1], function (data) {
                    return [data[0], S(data[1])];
                })];
            }).reverse());
            /** @type {boolean} */
            E = !E;
            B(I);
        });
        $("#undo").click(function () {
            if (G.length >= 1) {
                I = G.pop();
            }
            B(I);
        });
        $.map(problems, function (deepDataAndEvents, dataAndEvents) {
            $("#origami #silhouette-list").append('<li data-silhouette-id="' + dataAndEvents + '"><a href="#">' + dataAndEvents + "</a></li>");
        });
        O([0, 1], function (match) {
            /** @type {string} */
            var target = "#origami #" + (match ? "silhouette" : "texture");
            var canvas = $(match ? "#dst" : "#src");
            /** @type {number} */
            var h = canvas.height() / 2 + 128;
            /** @type {number} */
            var delta = canvas.width() / 2 - 128;
            /** @type {number} */
            var color = 0;
            $(target + " a").click(function (validClass) {
                validClass.preventDefault();
                /** @type {string} */
                validClass = "active";
                $(target + " li").removeClass(validClass);
                $(this).parent().addClass(validClass);
                B(I);
            });
            if (match) {
                /**
                 * @param {Object} e
                 * @return {?}
                 */
                var move = function (e) {
                    var x = e.offsetX;
                    var y = e.offsetY;
                    var touches = e.originalEvent.changedTouches;
                    if (touches) {
                        var nodeOfs = $("#dst")[0].getBoundingClientRect();
                        /** @type {number} */
                        x = touches[0].pageX - nodeOfs.left;
                        /** @type {number} */
                        y = touches[0].pageY - nodeOfs.top;
                    }
                    return [(x - delta) / 256, (h - y) / 256];
                };
                var onMouseMove;
                canvas.mousedown(onMouseMove = function (e) {
                    $("#navi").remove();
                    color = move(e);
                    return false;
                });
                canvas.bind("touchstart", onMouseMove);
                canvas.mousemove(onMouseMove = function (e) {
                    Z(color, move(e), 1);
                    return false;
                });
                canvas.bind("touchmove", onMouseMove);
                canvas.mouseup(onMouseMove = function (e) {
                    Z(color, move(e));
                    /** @type {number} */
                    color = 0;
                    return false;
                });
                canvas.bind("touchend", onMouseMove);
            }
            canvas[0].getContext("2d").setTransform(256, 0, 0, -256, delta, h);
        });
        B(I);
    } else {
        /** @type {Image} */
        var im = new Image;
        J.push(im);
        /**
         * @return {undefined}
         */
        im.onload = function () {
            Y(protos);
        };
        /** @type {string} */
        im.src = "/texture" + protos.shift() + ".png";
    }
}
/**
 * @param {Array} j
 * @return {?}
 */
function S(j) {
    return [1 - j[0], j[1]];
}
/**
 * @param {Array} a
 * @param {Array} b
 * @return {?}
 */
function R(a, b) {
    return [a[0] + b[0], a[1] + b[1]];
}
/**
 * @param {Array} l
 * @param {?} value
 * @return {undefined}
 */
function Q(l, value) {
    var context = $(value ? "#dst" : "#src")[0].getContext("2d");
    context.clearRect(-1, -1, 3, 3);
    context.beginPath();
    /** @type {number} */
    context.lineWidth = 0.001;
    /** @type {string} */
    context.strokeStyle = "#000000";
    fold(Array.apply(null, {
        length: 40
    }), -10, function (dataAndEvents, d) {
        context.moveTo(-1, d = dataAndEvents / 10);
        context.lineTo(3, d);
        context.moveTo(d, -1);
        context.lineTo(d, 3);
        return dataAndEvents + 1;
    });
    context.stroke();
    O(l, function (objectTypes) {
        context.beginPath();
        var ab = objectTypes[1];
        O(ab, function (tagMap) {
            context.lineTo(tagMap[value][0], tagMap[value][1]);
        });
        context.closePath();
        var texture = T("texture");
        if (texture) {
            if (value && objectTypes[0]) {
                context.save();
                context.clip();
                context.translate(ab[0][1][0], ab[0][1][1]);
                l = S(ab[0][0]);
                ab = W(F(S(ab[1][0]), l), F(ab[1][1], ab[0][1]));
                context.rotate(-Math.atan2(ab[1], ab[0]));
                context.translate(-l[0], -l[1]);
                context.drawImage(J[texture - 1], 0, 0, 512, 512, 0, 0, 1, 1);
                context.restore();
            } else {
                /** @type {string} */
                context.fillStyle = "#cccccc";
                context.fill();
            }
        }
        /** @type {number} */
        context.lineWidth = 0.003;
        /** @type {string} */
        context.strokeStyle = "#000000";
        context.stroke();
    });
    if (value) {
        var i = T("silhouette");
        if (i = i ? problems[i] : null) {
            context.beginPath();
            O([i[0], i[1]], function (inplace) {
                O(inplace, function (inplace) {
                    O(inplace, function (pt) {
                        context.lineTo(pt[0], pt[1]);
                    });
                    context.closePath();
                });
            });
            /** @type {string} */
            context.fillStyle = "rgba(255,192,192,.8)";
            context.fill();
            context.beginPath();
            O(i[2], function (dataAndEvents) {
                context.moveTo(dataAndEvents[0][0], dataAndEvents[0][1]);
                context.lineTo(dataAndEvents[1][0], dataAndEvents[1][1]);
            });
            /** @type {number} */
            context.lineWidth = 0.005;
            /** @type {string} */
            context.strokeStyle = "rgba(255,128,128,.8)";
            context.stroke();
        }
    }
}
/**
 * @param {?} n
 * @param {Array} m
 * @param {Function} set
 * @return {?}
 */
function C(n, m, set) {
    m = fold(n, [m], function (m, val) {
        return [m[1] ? set(m[0], m[1], val) : m[0], val];
    });
    return set(m[0], m[1], n[0]);
}
/**
 * @param {Array} a
 * @param {Array} b
 * @return {?}
 */
function L(a, b) {
    return [a[0] * b[0] - a[1] * b[1], a[0] * b[1] + a[1] * b[0]];
}
/**
 * @param {Array} v11
 * @param {Array} args
 * @return {?}
 */
function W(v11, args) {
    /** @type {number} */
    var d = args[0] * args[0] + args[1] * args[1];
    return [(args[0] * v11[0] + args[1] * v11[1]) / d, (args[0] * v11[1] - args[1] * v11[0]) / d];
}
/**
 * @param {?} results
 * @param {Function} f
 * @return {?}
 */
function U(results, f) {
    return fold(results, [], function (err, x) {
        return push(err, f(x));
    });
}
/**
 * @param {Array} v11
 * @return {?}
 */
function N(v11) {
    return [v11[0], -v11[1]];
}
/**
 * @param {Array} data
 * @param {Function} cb
 * @return {undefined}
 */
function O(data, cb) {
    fold(data, 0, function (dataAndEvents, outErr) {
        cb(outErr);
    });
}
/** @type {Array} */
var I = [[false, [[[0, 0], [0, 0]], [[1, 0], [1, 0]], [[1, 1], [1, 1]], [[0, 1], [0, 1]]]]];
/** @type {Array} */
var G = [];
/** @type {number} */
var E = 0;
/** @type {Array} */
var J = [];
$(function () {
    Y([0, 1, 2, 3]);
});
