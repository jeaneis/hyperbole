#!/usr/bin/env sh
pushd /Applications/aws-mturk-clt-1.3.0/bin
./loadHITs.sh $1 $2 $3 $4 $5 $6 $7 $8 $9 -label /Users/justinek/Documents/Grad_school/Research/Hyperbole/hyperbole_github/code/mTurkExp/HyperboleThreeDomains/Launch//hyperboleThreeDomains -input /Users/justinek/Documents/Grad_school/Research/Hyperbole/hyperbole_github/code/mTurkExp/HyperboleThreeDomains/Launch//hyperboleThreeDomains.input -question /Users/justinek/Documents/Grad_school/Research/Hyperbole/hyperbole_github/code/mTurkExp/HyperboleThreeDomains/Launch//hyperboleThreeDomains.question -properties /Users/justinek/Documents/Grad_school/Research/Hyperbole/hyperbole_github/code/mTurkExp/HyperboleThreeDomains/Launch//hyperboleThreeDomains.properties -maxhits 1
popd