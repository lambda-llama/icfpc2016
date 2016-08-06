from submitter import submit
import time


if __name__ == '__main__':
    prob_fst = 1
    prob_last = 101
    for i in range(prob_fst, prob_last):
        print(submit(str(i), "solution1.txt").text)
        time.sleep(2)
