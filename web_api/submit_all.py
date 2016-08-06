import time
from api import submit_problem


if __name__ == '__main__':
    prob_fst = 1
    prob_last = 101

    for i in range(prob_fst, prob_last):
        print(submit_problem(i, "solution1.txt").text)
        time.sleep(2)
