function [listener_highest_depth, meanings, utterances, affect_prior] = ...
    exaggeration_Oct2013_v1(pricePriorFileName, affectPriorFileName, ...
    utterance_costs, useScrapedPriors)
% 
% Now computes in log-space.
% No lexical uncertainty for valence -- only lexical uncertainty for state.
% Valence is correlated in prior with state -- higher states more likely to
% have valence.
% Uncertainty about goals: may want to communicate both valence and state or
% just one or the other
% 
% See if we can get more meanings, but fix the number of utterances
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% Set parameters
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

hardness = 1.5;
depth = 1;
prices = [50, 500, 1000, 5000, 10000];

% These are the possible meanings, i.e. the numerical states
meanings_round = prices;
meanings_sharp = meanings_round + 1;
meanings = [meanings_round, meanings_sharp];

% These are the possible utterances
utterances = meanings;
num_states = size(meanings,2);
num_utterances = size(utterances,2);


% Reads price priors from csv

%%%%%%%%
% Use constrained priors, so no need to fit
%%%%%%%%
% Read price priors from csv
D = csvread(['../../data/mTurkExp/pricePriors/', pricePriorFileName]);
priors = D(:,2)';
meaning_prior = log(priors);

% Reads affect priors from csv
A = csvread(['../../data/mTurkExp/affectPriors/', affectPriorFileName]);
affect_prior = A(:,2)';


% % Reads affect priors from csv
% A = csvread(['../../data/mTurkExp/affectPriors/', affectPriorFileName], 2, 1);
% 
% % Need to find a fitted line (continous) for lowess regression
% % So that we can find prob affect given any price value
% 
% % Smoothes affect priors
% % 1st attempt: Get a plot, but cannot predict unseen values
% % smoothed_affect_prior = [A(:,1), smooth(A(:,1), A(:,2), 'lowess')];
% % plot(smoothed_affect_prior(:,1), smoothed_affect_prior(:,2));
% 
% % 2nd attempt: This fits input data into a lowess curve, but the problem is
% % that the curve may not be monotonically increasing.
% % f = fit([x, y], z, ft)
% % z = f(x, y)
% f = fit([A(:,1), ones(length(A),1)], A(:,2), 'lowess');
% % ys = f(1:10005, ones(10005, 1));
% % plot(ys);
% %%%%%%%%%%
% % plot(A(:,1), f(A(:,1), ones(19,1)))
% % To predict: f(A(:,1), ones(19,1)) = y
% %%%%%%%%%%
% 
% % 3rd attempt: Fit input into a lowess curve, then interpolate the smoothed
% % data from the curve to predict unseen values.
% interp_affect_priors = mylowess(A, 1:10005, 1-eps);
% affect_prior = cutOffAt(interp_affect_priors(meanings), 1, 'above');

% Prior that the speaker DOES NOT have affect

valence_prior = log(1 - affect_prior);

num_goal_combinations = 3; % value 1 is when the speaker wants to communicate both valence and state; 2 is wants to communicate just state; 3 is wants to communicate just valence


% first column of lexical entries is the sd
% second and third column is whether the utterance is consistent with
% valence 0 and valence 1

lexical_entries = [1; 0.1];
num_lexical_entries = size(lexical_entries,1);


num_dictionaries = num_lexical_entries^num_utterances;
indices = 0:(num_dictionaries * (num_utterances)-1);
indices = reshape([indices(:) ; zeros(rem(num_utterances - rem(numel(indices),num_utterances),num_utterances),1)],num_utterances,[])';
utterances_in_dict = utterances(1+mod(indices,num_utterances));

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% Naive listener computations
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% naive_listener is a num_lexical_entries^num_utterances by num_utterances
% by num_states by 2 matrix, containing all of the depth-0 interpretations of
% the utterances
% The last dimension determines codes whether we are specifying the probability of valence
% 0 or valence 1
naive_listener = zeros(num_dictionaries, num_utterances, num_states, 2);

sd_dict = lexical_entries( 1 + mod( ...
    floor ( ...
        ( ceil(indices/num_utterances) - 1 ) .* ...
        ( num_lexical_entries.^ (1 + mod(indices,num_utterances)) ) / ...
        num_dictionaries ), ...
    num_lexical_entries));

% Compute unnormalized distribution for naive listener
for i=1:num_states
    answer_propto = meaning_prior(i) + log_of_normal(meanings(i),utterances_in_dict, sd_dict); %Combine meaning prior with literal distribution
    naive_listener(:,:,i,1) = answer_propto + valence_prior(i); %Compute joint probability of state and valence 0
    naive_listener(:,:,i,2) = answer_propto + log(1-exp(valence_prior(i)));  %Compute joint probability of state and valence 1
end

% now renormalize
max_for_each_utterance = max(naive_listener(:,:,:,:),[],3);
max_for_each_utterance = max(max_for_each_utterance(:,:,:,:),[],4);
matrix_of_maxes = repmat(max_for_each_utterance,[1,1,num_states,2]);
normalizing_constants = ...
    log(sum(sum(exp(naive_listener(:,:,:,:) - matrix_of_maxes),3),4)) + ...
    max_for_each_utterance;
matrix_of_normalizing_constants = repmat(normalizing_constants,[1,1,num_states,2]);
naive_listener(:,:,:,:) = naive_listener(:,:,:,:) - matrix_of_normalizing_constants;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% Compute speaker depth 0
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% speaker_depth_0 is a num_dictionaries by num_states by 2 by num_utterances by num_goal_combinations matrix, containing
% the speaker's distribution over utterances given his belief about the lexicon and the
% meaning that he wants to convey
speaker_depth_0 = zeros(num_dictionaries, num_utterances, num_states,2,num_goal_combinations);
matrix_of_costs = repmat(utterance_costs, [num_dictionaries,1,num_states,1]);
% compute speaker's likelihood given that he wants to communicate the
% valence 0
speaker_depth_0(:,:,:,1,1) = hardness*(matrix_of_costs + naive_listener(:,:,:,1));
% now compute speaker's likelihood given that he wants to communicate
% valence 1
speaker_depth_0(:,:,:,2,1) = hardness*(matrix_of_costs + naive_listener(:,:,:,2));

% compute speaker's likelihood given goal of just communicating the state
speaker_depth_0(:,:,:,1,2) = hardness*(matrix_of_costs + log(exp(naive_listener(:,:,:,1)) + exp(naive_listener(:,:,:,2)))); 
speaker_depth_0(:,:,:,2,2) = hardness*(matrix_of_costs + log(exp(naive_listener(:,:,:,1)) + exp(naive_listener(:,:,:,2))));

% compute speaker's likeohood given goal of just communicating valence
speaker_depth_0(:,:,:,1,3) = hardness*(matrix_of_costs + repmat(log(sum(exp(naive_listener(:,:,:,1)),3)),[1,1,num_states,1]));
speaker_depth_0(:,:,:,2,3) = hardness*(matrix_of_costs + repmat(log(sum(exp(naive_listener(:,:,:,2)),3)),[1,1,num_states,1]));

% now we compute normalizing constants
for i=1:2
    for j=1:num_goal_combinations
        max_for_each_meaning = max(speaker_depth_0(:,:,:,i,j),[],2);
        matrix_of_maxes = repmat(max_for_each_meaning,[1,num_utterances,1,1,1]);
        normalizing_constants = log(sum(exp(speaker_depth_0(:,:,:,i,j) - matrix_of_maxes),2)) + max_for_each_meaning;
        matrix_of_normalizing_constants = repmat(normalizing_constants,[1,num_utterances,1,1,1]);
        speaker_depth_0(:,:,:,i,j) = speaker_depth_0(:,:,:,i,j) - matrix_of_normalizing_constants;

    end
end

% we reshuffle the dimensions of speaker_depth_0
speaker_depth_0 = permute(speaker_depth_0, [1,3,4,5,2]); %num_dictionaries,num_sttes,valence,num_goal_combinations,num_utterances

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% Compute higher depth speakers and listeners
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

listener_higher_depths = zeros(num_utterances, num_states, 2, depth+1);
speaker_higher_depths = zeros(num_states,2,num_goal_combinations, num_utterances, depth);

% First define the listener's interpretation at depth 1 (this needs to be
% done separately because of the uncertainty about the speaker's lexical
% beliefs)

for i=1:num_utterances
    for j=1:num_states
        avg_speaker_response_1 = log(sum(sum(exp(speaker_depth_0(:,j,1,:,i)-max(max(speaker_depth_0(:,j,1,:,i))))))) +max(max(speaker_depth_0(:,j,1,:,i)));
        avg_speaker_response_2 = log(sum(sum(exp(speaker_depth_0(:,j,2,:,i)-max(max(speaker_depth_0(:,j,2,:,i))))))) +max(max(speaker_depth_0(:,j,2,:,i)));
        listener_higher_depths(i,j,1,1) = meaning_prior(j) + valence_prior(j) + avg_speaker_response_1;
        listener_higher_depths(i,j,2,1) = meaning_prior(j) + log(1-exp(valence_prior(j))) + avg_speaker_response_2;
    end
end

% now calculate normalizing constants
for i=1:num_utterances
    max_element = max(max(listener_higher_depths(i,:,:,1)));
    normalizing_constant = log(sum(sum(exp(listener_higher_depths(i,:,:,1) - max_element)))) + max_element;
    listener_higher_depths(i,:,:,1) = listener_higher_depths(i,:,:,1) - normalizing_constant;
end

% Now calculate beliefs and moves for higher depths
for i=1:depth
    
    for j=1:num_states
        answer_propto = zeros(num_goal_combinations, num_utterances);
        for k=1:2 % loop through valences
            % answer_propto(m,:) is answer for goal=m
            answer_propto(1,:) = hardness*(utterance_costs + listener_higher_depths(:,j,k,i)');
            answer_propto(2,:) = hardness*(utterance_costs + log(sum(exp(listener_higher_depths(:,j,:,i)),3)'));
            answer_propto(3,:) = hardness*(utterance_costs + log(sum(exp(listener_higher_depths(:,:,k,i)),2)'));
            speaker_higher_depths(j,k,1,:,i) = answer_propto(1,:) - (log(sum(exp(answer_propto(1,:) - max(answer_propto(1,:))))) + max(answer_propto(1,:)));
            speaker_higher_depths(j,k,2,:,i) = answer_propto(2,:) - (log(sum(exp(answer_propto(2,:) - max(answer_propto(2,:))))) + max(answer_propto(2,:)));
            speaker_higher_depths(j,k,3,:,i) = answer_propto(3,:) - (log(sum(exp(answer_propto(3,:) - max(answer_propto(3,:))))) + max(answer_propto(3,:)));
        end
    end
    
    for j=1:num_utterances
        answer_propto_1 = meaning_prior + valence_prior + log(sum(exp(speaker_higher_depths(:,1,:,j,i)),3)');
        answer_propto_2 = meaning_prior + log(1-exp(valence_prior)) + log(sum(exp(speaker_higher_depths(:,2,:,j,i)),3)');
        max_element = max(max(answer_propto_1),max(answer_propto_2));
        normalizing_constant = log(sum(exp(answer_propto_1-max_element)+exp(answer_propto_2-max_element))) + max_element;
        listener_higher_depths(j,:,1,i+1) = answer_propto_1 - normalizing_constant;
        listener_higher_depths(j,:,2,i+1) = answer_propto_2 - normalizing_constant;
    end
    
end

listener_highest_depth = exp(listener_higher_depths(:,:,:,depth));