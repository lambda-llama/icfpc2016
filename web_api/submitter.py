#! /usr/bin/python3
import sys
import requests

from api import submit_solution

if __name__ == '__main__':
    _, problem_id = sys.argv
    path = "solutions/solution{}.txt".format(problem_id)
    print(submit_solution(problem_id, path).text)
