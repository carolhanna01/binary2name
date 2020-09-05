#! /bin/bash

# the next program must exit with error

if ./test_groups; then
  false; 
else
  if ./test_groups -a; then
    if ./test_groups -a -b; then
      false;
    else
      if ./test_groups --optc -d; then
        false;
      else
        true;
      fi
    fi
  else
    false;
  fi
fi
