#!/bin/bash -x
# TODO: Convert this into a python script

run_id="$1"
run_id_output="swarming-logs-runids.txt"
logs_dir="logs-outputs"

echo "Run id is $run_id"

python client-py/swarming.py query -S chrome-swarming.appspot.com "tasks/list?tags=${run_id}" --limit=1000 | jq --raw-output '.items[] | select(.state == "COMPLETED" and .failure == false) | .run_id' > $run_id_output

# TODO: Don't clobber currently existing directory.
mkdir -p $logs_dir
total=$(wc -l ${run_id_output})
collected="0";
while read id; do
  echo "Collecting log ${collected} of ${total}"
  python client-py/swarming.py collect -S chrome-swarming.appspot.com ${id} > ${logs_dir}/${id}
  collected=$((${collected} + 1))
done < ${run_id_output}
