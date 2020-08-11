#!/usr/bin/env bash

DATA_BUCKET=${DATA_BUCKET:-data.larc.micca.report}

# Create data directory for data
mkdir -p site_data

# Do dumb sync of all data
aws s3 sync s3://${DATA_BUCKET}/data site_data/

# Construct YMD_STRING
# YMD_STRING='2020-03-01'
# Query for the list of objects.  Backticks required to surround date?
#
# aws s3api list-objects --bucket "${DATA_BUCKET}"\
#   --query 'Contents[?LastModified>=`'#{YMD_STRING}'`][].{Key: Key}'
# 
# Pipe to jq to extract relevant info

