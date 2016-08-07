import os.path
import subprocess
import sys
import time

from api import submit_solution


if __name__ == '__main__':
    try:
        [prob_fst, prob_last] = map(int, sys.argv[1:])
    except ValueError:
        sys.exit("usage: %prog ID_START ID_END")

    for i in range(prob_fst, prob_last):
        print("problem {:04d}:".format(i), end=" ")
        if not os.path.exists("problems/problem{}.txt".format(i)):
            print("SKIP")
            continue

        solution_path = "solutions/solution{}.txt".format(i)
        if os.path.exists(solution_path):
            print("Skip soln already exists")
            continue
        try:
            subprocess.check_call(["./solver_bf.native", str(i)],
                                  stdout=subprocess.PIPE,
                                  stderr=subprocess.PIPE,
                                  timeout=5)
        except subprocess.TimeoutExpired:
            print("FAILED")
            continue

        response = submit_solution(i, solution_path).json()
        if response["ok"]:
            print("OK " + solution_path)
        else:
            os.remove(solution_path)
            print("ERROR " + response["error"])
        time.sleep(5)
