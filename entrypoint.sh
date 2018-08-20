#!/bin/bash
echo "archive.nyu.edu:">auth.yml
echo "  headers:">>auth.yml
echo "   rest-dspace-token: $TOKEN_FDA" >> auth.yml
exec primo-endpoint "$@" 
