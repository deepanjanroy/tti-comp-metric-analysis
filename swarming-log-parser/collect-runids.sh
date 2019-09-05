#!/bin/bash -x

run_id="$1"
run_id_output="swarming-logs-runids.txt"

python client-py/swarming.py query -S chrome-swarming.appspot.com "tasks/list?tags=${run_id}" --limit=5 \
    | jq --raw-output '.items[]' > $run_id_output


echo "Wrote run ids to $run_id_output"
