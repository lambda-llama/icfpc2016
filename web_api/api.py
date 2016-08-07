import requests


"""
REQ_BLOB = ("curl --compressed -L -H Expect: -H 'X-API-Key: 179-c628d31b740634928666a1515aeb12a2' "
            "'http://2016sv.icfpcontest.org/api/blob/[hash]'")

REQ_SNAPSHOTS = ("curl --compressed -L -H Expect: -H 'X-API-Key: 179-c628d31b740634928666a1515aeb12a2' ",
                 "'http://2016sv.icfpcontest.org/api/snapshot/list'")

REQ_SUBMIT_PROBLEM = ("curl --compressed -L -H Expect: -H 'X-API-Key: 179-c628d31b740634928666a1515aeb12a2' "
                      "-F 'solution_spec=@[solution_file]' -F 'publish_time=[publish_time]' "
                      "'http://2016sv.icfpcontest.org/api/problem/submit'")

REQ_SUBMIT_SOLUTION = ("curl --compressed -L -H Expect: -H 'X-API-Key: 179-c628d31b740634928666a1515aeb12a2' "
                       "-F 'problem_id=[problem_id]' -F 'solution_spec=@[solution_file]' "
                       "'http://2016sv.icfpcontest.org/api/solution/submit'")
"""

HEADERS = {"X-API-Key": "179-c628d31b740634928666a1515aeb12a2"}


def request_blob(hash):
    api_url = "http://2016sv.icfpcontest.org/api/blob/{}".format(hash)
    resp = requests.get(api_url, headers=HEADERS)
    return resp


def request_snapshots():
    api_url = "http://2016sv.icfpcontest.org/api/snapshot/list"
    resp = requests.get(api_url, headers=HEADERS)
    return resp


def submit_problem(solution_file, publish_time):
    api_url = "http://2016sv.icfpcontest.org/api/problem/submit"

    with open(solution_file) as f:
        files = {
            "solution_spec": f,
            "publish_time": str(publish_time)
        }
        resp = requests.post(api_url, headers=HEADERS, files=files)
    return resp


def submit_solution(problem_id, solution_file):
    api_url = "http://2016sv.icfpcontest.org/api/solution/submit"

    with open(solution_file) as f:
        files = {
            "problem_id": str(problem_id),
            "solution_spec": f
        }
        resp = requests.post(api_url, headers=HEADERS, files=files)
    return resp

