import argparse
import ct_logs_processor
import os
from pprint import pprint
import sys

input_logs_dir = 'logs-outputs'
output_csv_dir = 'csv-outputs'

def process_all_input_logs():
  try:
    os.makedirs(output_csv_dir, 0o755)
  except os.error:
    print "Output directory {0} already exists. Please delete it first.".format(output_csv_dir)
    sys.exit(1)

  all_files = os.listdir(input_logs_dir)
  for i, filename in enumerate(all_files):
    input_filepath = os.path.join(input_logs_dir, filename)
    output_filepath = os.path.join(output_csv_dir, filename + '.csv')
    print "Processing file", i, "of", len(all_files), ":", input_filepath, "->", output_filepath
    try:
      ct_logs_processor.transform_to_csv(input_filepath, output_filepath)
    except:
      print "Failed to process", input_filepath

# Use this for one file.
# ct_logs_processor.transform_to_csv('logs-outputs/45220ac7cfbf7411', 'out')

# Use this for all input files at once.
process_all_input_logs()


pprint(ct_logs_processor.global_runs_stats)
