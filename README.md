# icfpc2016

λ-llama submission for [ICFP contest 2016](http://icfpc2016.blogspot.com)

## Compiling the code

```
$ opam install core_kernel ppx_deriving oasis
$ ocaml setup.ml -all
```

## Running the visualizer

```bash
$ cd visualize
$ python -m http.server
```

## Running not finished solution based on convex hull
```
$ cd visualize
$ rm out/* && python to_js.py < problems/problem101.txt
```
Afterwards you can see correct folding sequence and not-finished-yet unfolding procedure.