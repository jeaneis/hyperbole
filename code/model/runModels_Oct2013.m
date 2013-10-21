useScrapedPriors = false;
hardness = 1;
num_utterances = 10;
utterance_cost_ratio = 1.5;
highVariance = 1;
lowVariance = 0.5;


utterance_costs = [ones(num_utterances / 2, 1) * utterance_cost_ratio; ones(num_utterances/2,1)]';
% Electric Kettle
[l,m,u,a] = exaggeration_Oct2013_v1('kettle_constrainedPriors.csv', 'kettle_affect-10182013.csv', ...
    utterance_costs, hardness, highVariance, lowVariance);
displayResults(l,m,u,a,'kettle_constrained.csv');

% Laptop
[l,m,u,a] = exaggeration_Oct2013_v1('laptop_constrainedPriors.csv', 'laptop_affect-10182013.csv', ...
    utterance_costs, hardness, highVariance, lowVariance);
displayResults(l,m,u,a,'laptop_constrained.csv');

% Watch
[l,m,u,a] = exaggeration_Oct2013_v1('watch_constrainedPriors.csv', 'watch_affect-10182013.csv', ...
    utterance_costs, hardness, highVariance, lowVariance);
displayResults(l,m,u,a,'watch_constrained.csv');

