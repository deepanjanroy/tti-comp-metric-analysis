import csv
import sys
from pprint import pprint

# If a column name has his suffix, this suffix will be deleted.
SUFFIX_TO_DROP = ' (ms)'

# List all the renamings of input columns.
RENAMES = {
  "expectedFirstInputDelay": "ExpectedFID",
  "timeToFirstContentfulPaint": "FCP",
  "timeToOnload": "Load Event",
  "navigationsReachedTTI": "# of navigations",
}

# Copies of a column with new name. Format: [(source_col, new_col)]
COPIES = [
  ("TTI-50", "TTI")
]

def genTtiEvaluationMetrics(window_size):
  ret = []
  prefix = "TTI-" + str(window_size)
  ret.append(prefix)
  ret.append(prefix + '-LongestTaskDuration')
  ret.append(prefix + '-LongestTaskTs')
  ret.append(prefix + '-LastLongTaskDuration')
  ret.append(prefix + '-LastLongTaskTs')
  return ret

def genEndpointPairs(endpoints):
  ret = []
  for i in xrange(len(endpoints)):
    for j in xrange(i + 1, len(endpoints)):
      ret.append((endpoints[i], endpoints[j]))
  return ret

def genSumTaskMetric(prefix, start, end):
  return "{0}-{1}-{2}".format(prefix, start, end)

FIRST_COLUMNS = ["page_name", "traceUrls"]

def genLowPriorityOutputColumns():
  cols = []
  all_endpoints = ["navStart", "FCP", "Interactive", "TtiPlus10s"]
  cols.extend([genSumTaskMetric('SumOfQueuingTimeGT50_p1.0', *pair)
               for pair in genEndpointPairs(all_endpoints)])
  cols.extend([genSumTaskMetric('SumOfQueuingTimeGT50_p1.5', *pair)
               for pair in genEndpointPairs(all_endpoints)])
  cols.extend([genSumTaskMetric('SumOfQueuingTimeGT50_p2.0', *pair)
               for pair in genEndpointPairs(all_endpoints)])
  cols.extend([genSumTaskMetric('SumOfLongTasks', *pair)
               for pair in genEndpointPairs(all_endpoints)])

  cols.extend([
    "timeToFirstPaint",
    "timeToFirstMeaningfulPaint",
    "timeToFirstCpuIdle",
    "Load Event",
    "LongestTaskInTraceDuration",
    "LongestTaskInTraceTs",
  ])

  for n in (50, 100, 200):
    cols.extend(genTtiEvaluationMetrics(n))

  return cols

# These columns will show up first
def genHighPriorityOutputColumns():
  cols = []
  cols.extend(FIRST_COLUMNS)
  cols.extend([
    "TTI",
    "# of navigations",
    "FCP",
    "DCL",
  ])

  endpoint_pairs = [
    ("navStart", "Interactive"),
    ("navStart", "TtiPlus10s"),
    ("FCP", "Interactive"),
    ("FCP", "TtiPlus10s"),
  ]

  cols.extend([genSumTaskMetric('SumOfQueuingTimeGT50_p1.0', *pair)
               for pair in endpoint_pairs])
  cols.extend([genSumTaskMetric('SumOfQueuingTimeGT50_p1.5', *pair)
               for pair in endpoint_pairs])
  cols.extend([genSumTaskMetric('SumOfQueuingTimeGT50_p2.0', *pair)
               for pair in endpoint_pairs])
  cols.extend([genSumTaskMetric('SumOfLongTasks', *pair)
               for pair in endpoint_pairs])
  cols.extend(['ProbFidMoreThan-{0}'.format(n) for n in range(50, 501, 50)])
  cols.extend([
    "ExpectedFID",
  ])
  return cols

def diff_list(base_list, new_list):
  new_list_set = set(new_list)
  return [x for x in base_list if x not in new_list_set]

def genOutputColumns():
  high_pri_columns = genHighPriorityOutputColumns()
  all_columns = high_pri_columns + genLowPriorityOutputColumns()
  low_pri_columns = diff_list(all_columns, high_pri_columns)
  return high_pri_columns + low_pri_columns

def drop_suffix(string):
  if string.endswith(SUFFIX_TO_DROP):
    return string[:-len(SUFFIX_TO_DROP)]
  return string

# Returns a list of row dicts.
def read_input(input_file):
  with open(input_file) as f:
    reader = csv.DictReader(f);
    rows = []
    for row in reader:
      column_names = row.keys()
      for col in column_names:
        row[drop_suffix(col)] = row.pop(col)
      rows.append(row)
  return rows

# TODO: There are a few too many for row in rows. These functions should
# perhaps work on a single column.
def rename_columns(rows):
  for row in rows:
    for old_name, new_name in RENAMES.items():
      if old_name in row:
        row[new_name] = row[old_name]
        del row[old_name]


# In Cluster Telemetry CSV, page names are often of the form
# http://example.com (#42). This gets rid of the story run number.
def clean_page_name(rows):
  for row in rows:
    page_name = row['page_name']
    row['page_name'] = page_name.split(" ")[0]


def copy_columns(rows):
  for row in rows:
    for source_col, new_col in COPIES:
      if source_col in row:
        row[new_col] = row[source_col]

# Deletes columns that are not in output_columns.
def prune_columns(rows, output_columns):
  for row in rows:
    current_columns = row.keys()
    for col in current_columns:
      if col not in output_columns:
        del row[col]

def write_output(rows, output_file, output_columns):
  with open(output_file, 'w') as f:
    writer = csv.DictWriter(f, output_columns)
    writer.writeheader()
    writer.writerows(rows)

def check_output_columns_exist(row, output_columns):
  input_columns = row.keys()
  for col in output_columns:
    if col not in input_columns:
      raise Exception("Output column {0} does not exist in input".format(col))

def has_single_navigation(row):
  return float(row['# of navigations']) == 1.0


def filter_output_rows(rows):
  return filter(has_single_navigation, rows);

"""
Usage:
$SCRIPT_NAME <input> <output>
"""
def main():
  output_columns = genOutputColumns()
  rows = read_input(sys.argv[1])
  input_length = len(rows);
  rename_columns(rows)
  copy_columns(rows)
  prune_columns(rows, output_columns)
  clean_page_name(rows)
  check_output_columns_exist(rows[0], output_columns)
  rows = filter_output_rows(rows);
  write_output(rows, sys.argv[2], output_columns)
  print "{0} total columns written.".format(len(output_columns))
  print "{0} total rows written.".format(len(rows))
  print "{0} total rows discarded.".format(input_length - len(rows))

if __name__ == '__main__':
  main()
