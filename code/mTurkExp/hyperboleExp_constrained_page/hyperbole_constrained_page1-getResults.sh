#!/usr/bin/env sh
pushd /Applications/aws-mturk-clt-1.3.0/bin
./getResults.sh $1 $2 $3 $4 $5 $6 $7 $8 $9 -successfile /Users/justinek/Documents/Grad_school/Research/Hyperbole/hyperbole_github/code/mTurkExp/hyperboleExp_constrained_page//hyperbole_constrained_page1.success -outputfile /Users/justinek/Documents/Grad_school/Research/Hyperbole/hyperbole_github/code/mTurkExp/hyperboleExp_constrained_page//hyperbole_constrained_page1.results
popd