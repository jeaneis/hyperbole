useScrapedPriors = false;
num_utterances = 10;
utterance_cost_ratio = 1.2;
utterance_costs = [ones(num_utterances / 2, 1) * utterance_cost_ratio; ones(num_utterances/2,1)]';


% Electric Kettle
[l,m,u,a] = exaggeration_Oct2013_v1('kettle_constrainedPriors.csv', 'test_kettle_affectPriors.csv', ...
    utterance_costs, useScrapedPriors);
displayResults(l,m,u,a,'kettle_constrained.csv');

% Laptop
[l,m,u,a] = exaggeration_Oct2013_v1('laptop_constrainedPriors.csv', 'test_laptop_affectPriors.csv', ...
    utterance_costs, useScrapedPriors);
displayResults(l,m,u,a,'laptop_constrained.csv');

% Watch
[l,m,u,a] = exaggeration_Oct2013_v1('watch_constrainedPriors.csv', 'test_watch_affectPriors.csv', ...
    utterance_costs, useScrapedPriors);
displayResults(l,m,u,a,'watch_constrained.csv');

