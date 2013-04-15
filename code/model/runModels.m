% Coffee Maker
[l,m,u,a] = exaggeration_March2013_v1('coffee_maker.csv', 'coffee_affect.csv');
displayResults(l,m,u,a,'coffee_realAffect_states14.csv');

% Electric Kettle
[l,m,u,a] = exaggeration_March2013_v1('electric_kettle.csv', 'kettle_affect.csv');
displayResults(l,m,u,a,'kettle_realAffect_states14.csv');

% Headphones
[l,m,u,a] = exaggeration_March2013_v1('headphones.csv', 'headphones_affect.csv');
displayResults(l,m,u,a,'headphones_realAffect_states14.csv');

% Laptop
[l,m,u,a] = exaggeration_March2013_v1('laptop.csv', 'laptop_affect.csv');
displayResults(l,m,u,a,'laptop_realAffect_states14.csv');

% Sweater
[l,m,u,a] = exaggeration_March2013_v1('sweater.csv', 'sweater_affect.csv');
displayResults(l,m,u,a,'sweater_realAffect_states14.csv');

% Watch
[l,m,u,a] = exaggeration_March2013_v1('watch.csv', 'watch_affect.csv');
displayResults(l,m,u,a,'watch_realAffect_states14.csv');



%%%%%% SCRAPED PRICE PRIORS

% Coffee Maker
[l,m,u,a] = exaggeration_March2013_v1('coffee_maker.txt', 'coffee_affect.csv');
displayResults(l,m,u,a,'coffee_realAffect_states14.csv');

% Electric Kettle
[l,m,u,a] = exaggeration_March2013_v1('electric_kettle.txt', 'kettle_affect.csv');
displayResults(l,m,u,a,'kettle_realAffect_states14.csv');

% Headphones
[l,m,u,a] = exaggeration_March2013_v1('headphones.txt', 'headphones_affect.csv');
displayResults(l,m,u,a,'headphones_realAffect_states14.csv');

% Laptop
[l,m,u,a] = exaggeration_March2013_v1('newegg_laptop.txt', 'laptop_affect.csv');
displayResults(l,m,u,a,'laptop_realAffect_states14.csv');

% Sweater
[l,m,u,a] = exaggeration_March2013_v1('sweater.txt', 'sweater_affect.csv');
displayResults(l,m,u,a,'sweater_realAffect_states14.csv');

% Watch
[l,m,u,a] = exaggeration_March2013_v1('watch.txt', 'watch_affect.csv');
displayResults(l,m,u,a,'watch_realAffect_states14.csv');
