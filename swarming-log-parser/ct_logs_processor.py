import ast
import csv
import re
import sys
import traceback
import argparse
from collections import defaultdict
from pprint import pprint


TABLE_MERGER_COMMAND = 'python /b/s/w/ir/share/ct-master/py/csv_pivot_table_merger.py'
LOG_PREFIX_PATTERN = '^I.............................exec\.go\:82] exec\.go\:82'
FOR_ROWS_PREFIX = "^For rows: "

global_runs_stats = {
  'with_tti': 0,
  'without_tti': 0,
  'more_than_one_value': defaultdict(int)
}


def get_ct_rows(filename):
  output = []
  found_merge_command = False
  inside_row_output = False
  rows_buffer = []

  with open(filename) as f:
    for line in f:
      line = line.strip()
      # Iterate until you find the merge command.
      if TABLE_MERGER_COMMAND in line:
        found_merge_command = True
      if not found_merge_command: continue

      if (re.match(FOR_ROWS_PREFIX, line)):
        # The next few lines is our row output
        inside_row_output = True
        # Strip the "For rows: " prefix
        line = re.sub(FOR_ROWS_PREFIX, "", line)
        rows_buffer.append(line)
        continue

      if inside_row_output:
        if re.match("^Avg row is ", line):
          # We've seen all of current row output. Process the buffer.
          whole_buffer = ''.join(rows_buffer)
          output.append(ast.literal_eval(whole_buffer))
          rows_buffer = []
          inside_row_output = False
        elif re.match(LOG_PREFIX_PATTERN, line):
          # We continue to see row output. Keep buffering output.
          # All the lines we care about start with LOG_PREFIX_PATTERN
          # except the first line.

          # Strip the prefix
          line = re.sub(LOG_PREFIX_PATTERN, "", line).strip()
          if len(line) == 0: continue
          rows_buffer.append(line)
        else:
          # This is a line from some other program. Don't buffer it.
          pass

  return output

def get_metrics_per_run(ct_output_rows):
  # First build the url -> run_index -> dict of metrics
  url_to_run_index = {}
  for ct_output_row in ct_output_rows:
    if len(ct_output_row) == 0: continue
    for histogram in ct_output_row:
      metric_name = histogram['name']
      if metric_name not in SELECTED_METRICS:
        continue

      if histogram['avg'] == '': continue
      if histogram['count'] < 1: continue

      url = histogram['stories'].split("(")[0].strip()
      run_index = int(histogram['storysetRepeats'])
      if histogram['count'] > '1':
        # Track this case.
        global_runs_stats['more_than_one_value'][metric_name] += 1

      metric_value = histogram['avg']

      # Stories can be like "https://google.com (#12)".
      # Strip the story number at the end.
      if url not in url_to_run_index:
        url_to_run_index[url] = defaultdict(dict)

      metrics_dict = url_to_run_index[url][run_index]
      metrics_dict[metric_name] = float(metric_value)

      trace_url = histogram['traceUrls']
      if 'trace_url' in metrics_dict:
        try:
          assert metrics_dict['trace_url'] == trace_url
        except:
          print "Expected", metrics_dict['trace_url']
          print "but found"
          print trace_url
          print "Metric", metric_name
          print "Storyset repeat: ", run_index
          print "Current dict:"
          pprint(metrics_dict)
          raise
      else:
        metrics_dict['trace_url'] = trace_url
  return url_to_run_index



# Including all the metrics makes the dataset unnecessarily large. Include
# only selected metrics.
SELECTED_METRICS = [
  'trace_url',
  'SumOfQueuingTimeGT50_p1.0-navStart-Interactive',
  'SumOfQueuingTimeGT50_p1.0-navStart-TtiPlus10s',
  'SumOfQueuingTimeGT50_p1.0-FCP-Interactive',
  'SumOfQueuingTimeGT50_p1.0-FCP-TtiPlus10s',
  'SumOfQueuingTimeGT50_p1.5-navStart-Interactive',
  'SumOfQueuingTimeGT50_p1.5-navStart-TtiPlus10s',
  'SumOfQueuingTimeGT50_p1.5-FCP-Interactive',
  'SumOfQueuingTimeGT50_p1.5-FCP-TtiPlus10s',
  'SumOfQueuingTimeGT50_p2.0-navStart-Interactive',
  'SumOfQueuingTimeGT50_p2.0-navStart-TtiPlus10s',
  'SumOfQueuingTimeGT50_p2.0-FCP-Interactive',
  'SumOfQueuingTimeGT50_p2.0-FCP-TtiPlus10s',
  'navigationsReachedTTI',  # We should probably filter this out earlier.
  'timeToInteractive',
  'timeToFirstContentfulPaint',
  'DCL',
  'timeToOnload',
  'TTI-50',
  'mainFrameCumulativeLayoutShift',
  'largestTextPaint',
  'largestImagePaint',
]


CSV_FIELDNAMES = ['url', 'sort_by_fcp_index'] + SELECTED_METRICS

def get_csv_rows(ct_logs_filename):
  metrics_per_run = get_metrics_per_run(get_ct_rows(ct_logs_filename))
  csv_rows = []
  broken_once = False

  for url, run_index_to_metrics in metrics_per_run.items():
    all_runs = run_index_to_metrics.items()
    for i, run in all_runs:
      if 'navigationsReachedTTI' not in run:
        print ct_logs_filename
        print url
        print run
        print i
        raise Exception("FATAL ERROR: navigationsReachedTTI metric not found.")
    runs_with_tti = [r[1] for r in all_runs if int(r[1]['navigationsReachedTTI']) == 1]
    global_runs_stats['with_tti'] += len(runs_with_tti)
    global_runs_stats['without_tti'] += (len(all_runs) - len(runs_with_tti))

    sorted_runs = sorted(runs_with_tti, key=lambda x: x['timeToFirstContentfulPaint'])
    for i, metrics in enumerate(sorted_runs):
      metrics['url'] = url
      metrics['sort_by_fcp_index'] = i
      csv_rows.append(metrics)
  return csv_rows

def write_csv(out_filename, csv_rows):
  with open(out_filename, 'w') as f:
    writer = csv.DictWriter(f, CSV_FIELDNAMES)
    writer.writeheader()
    for row in csv_rows:
      writer.writerow(row)

def transform_to_csv(ct_logs_filename, out_filename):
  write_csv(out_filename, get_csv_rows(ct_logs_filename))

# main
# argparser = argparse.ArgumentParser()
# argparser.add_argument("input", help="Path to swarming log file")
# argparser.add_argument("output", help="Path to output csv file")
# args = argparser.parse_args()
# metrics_per_run = get_metrics_per_run(get_ct_rows(args.input))
# csv_rows = get_csv_rows(metrics_per_run)
# write_csv(args.output, csv_rows)
# pprint(url_to_metrics_dict.items()[0][1].keys())


# write_csv(args.output, url_to_metrics_dict)
# pprint({k: dict(v) for k, v in dict(url_to_metrics_dict).items()})
