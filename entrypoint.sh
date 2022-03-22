#!/bin/bash
echo "archive.nyu.edu:">auth.yml
echo "  headers:">>auth.yml
echo "   rest-dspace-token: changeme >> auth.yml
exec primo-endpoint -a auth.yml "$@"
