#!/usr/bin/env sh
pushd /Applications/aws-mturk-clt-1.3.0/bin
./loadHITs.sh $1 $2 $3 $4 $5 $6 $7 $8 $9 -label /Users/justinek/Dropbox/Hyperbole/HyperboleV2/mTurk_priors/affectPriorsExp/affect2 -input /Users/justinek/Dropbox/Hyperbole/HyperboleV2/mTurk_priors/affectPriorsExp/affect2.input -question /Users/justinek/Dropbox/Hyperbole/HyperboleV2/mTurk_priors/affectPriorsExp/affect2.question -properties /Users/justinek/Dropbox/Hyperbole/HyperboleV2/mTurk_priors/affectPriorsExp/affect2.properties -maxhits 1
popd