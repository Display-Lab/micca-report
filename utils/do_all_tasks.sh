#!/usr/bin/env bash

DATA_BUCKET=${DATA_BUCKET:-data.larc.micca.report}

# Create data directory for data
mkdir -p site_data

# Do dumb sync of all data from bucket to local
aws s3 sync s3://${DATA_BUCKET}/data site_data/

# Generate reports
Rscript --default-packages=miccareport -e 'miccareport::main()'

#debug with dry run
# Do dumb sync of local reports to bucket
aws s3 sync --dryrun site_reports/ s3://${DATA_BUCKET}/data
