<<<<<<< HEAD
echo "archive.nyu.edu:\n  headers:\n    rest-dspace-token: $TOKEN_FDA">auth.yml
exec primo-endpoint "$@" 
=======
#!/bin/bash
echo "archive.nyu.edu:">auth.yml
echo "  headers:">>auth.yml
echo "   rest-dspace-token: $TOKEN_FDA" >> auth.yml
exec primo-endpoint -a auth.yml "$@"
>>>>>>> b6b0d9119664e9de079e86cfc9cb9d4faa645799
