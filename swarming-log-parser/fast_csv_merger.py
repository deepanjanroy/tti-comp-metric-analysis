import argparse

argparser = argparse.ArgumentParser()
argparser.add_argument("--output", help="Path to output csv file", required=True)
argparser.add_argument("--input",
                       help="List of paths to input csv files",
                       required=True,
                       nargs='+')
args = argparser.parse_args()
print 'input', args.input
print 'output', args.output

header = None
with open(args.output, 'w') as outfile:
  for input_file in args.input:
    with open(input_file) as infile:
      current_header = infile.readline()
      if header is None:
        header = current_header
        outfile.write(header)
      else:
        assert (current_header == header)
      for line in infile:
        outfile.write(line)

print "Wrote to", args.output
