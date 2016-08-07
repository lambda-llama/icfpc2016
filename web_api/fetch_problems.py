import os
import time
from glob import glob

from api import request_snapshots, request_blob


MAX_TO_DOWNLOAD = 200


if __name__ == '__main__':
    tmp_snaps = request_snapshots().json()

    last_snap = tmp_snaps['snapshots'][-1]
    last_snap_hash = last_snap['snapshot_hash']
    last_snap_time = last_snap['snapshot_time']
    print('Latest snapshot:\n hash:{}\n time:{}'.format(
        last_snap_hash, last_snap_time))

    # To avoid "429: Too Many Requests"
    time.sleep(1)
    
    tmp_blob = request_blob(last_snap_hash).json()

    problems_curr_d = {e['problem_id']: (e['publish_time'],
                                         e['problem_spec_hash'])
                     for e in tmp_blob['problems']}
    
    problems_old = set([int(os.path.basename(e)
                            .replace('problem', '')
                            .replace('.txt', ''))
                        for e in glob('../problems/*.txt')])
    # TODO: enhance matching via using `spec_hash` and `publish_time`
    problems_todo = set(problems_curr_d.keys()).difference(problems_old)
    print('New problems found: {}'.format(problems_todo))

    for idx, problem_todo in enumerate(problems_todo):
        time.sleep(1)
        
        tmp_filename = '../problems/problem{}.txt'.format(problem_todo)
        tmp_problem = request_blob(problems_curr_d[problem_todo][1]).text
        with open(tmp_filename, 'w') as f:
            f.writelines(tmp_problem)

        tmp_filename = '../problems/problem{}.meta'.format(problem_todo)
        meta = problems_curr_d[problem_todo]
        with open(tmp_filename, 'w') as f:
            f.writelines([str(meta[0]), '\n', meta[1], '\n'])
        
        print('Problem {} has been downloaded.'.format(problem_todo))

        if idx == MAX_TO_DOWNLOAD:
            print('Artificial download limit exceeded.')
            quit()
