#! /usr/bin/python3
import sys
import requests


if __name__ == '__main__':
    _, problem_id, file = sys.argv
    print(problem_id, file)
    headers = {
        "X-API-Key": "179-c628d31b740634928666a1515aeb12a2"
    }
    api_url = "http://2016sv.icfpcontest.org/api/solution/submit"
    with open(file) as f:
        files = {
            "problem_id": problem_id,
            "solution_spec": f
        }
        resp = requests.post(
            api_url, headers=headers, files=files
        )
    print(resp.text)
