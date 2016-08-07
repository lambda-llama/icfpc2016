import os.path
import subprocess
import time

from api import submit_solution


if __name__ == '__main__':
    prob_fst = 103
    prob_last = 710

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

        print("OK " + solution_path)
        print(submit_solution(i, solution_path).text)
        time.sleep(5)
