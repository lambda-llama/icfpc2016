import os.path
import subprocess
import sys
import time

from api import submit_solution


if __name__ == '__main__':
    try:
        solver = sys.argv[1]
        [prob_fst, prob_last] = map(int, sys.argv[2:])
    except ValueError:
        sys.exit("usage: %prog SOLVER ID_START ID_END")

    for i in range(prob_fst, prob_last + 1):
        print("problem {:04d}:".format(i), end=" ")
        if not os.path.exists("problems/problem{}.txt".format(i)):
            print("SKIP problem missing")
            continue

        solution_path = "solutions/solution{}.txt".format(i)
        if os.path.exists(solution_path):
            print("SKIP solution exists")
            continue

        try:
            subprocess.check_call([solver, str(i)],
                                  stdout=subprocess.PIPE,
                                  stderr=subprocess.PIPE,
                                  timeout=5)
        except (subprocess.TimeoutExpired,
                subprocess.CalledProcessError):
            print("FAILED")
            continue

        response = submit_solution(i, solution_path).json()
        if response["ok"]:
            print("OK {} resemblance: {}".format(
                solution_path, response["resemblance"]))
        else:
            os.remove(solution_path)
            print("ERROR " + response["error"])
        time.sleep(5)
