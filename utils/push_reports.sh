#!/usr/bin/env bash

DATA_BUCKET=${DATA_BUCKET:-data.larc.micca.report}

# Do dumb sync of all reports
aws s3 sync site_reports/ s3://${DATA_BUCKET}/reports 
