function listener_highest_depth = exaggeration_MCMC_v4()
%Now computes in log-space
%No lexical uncertainty for valence -- only lexical uncertainty for state
%Valence is correlated in prior with state -- higher states more likely to
%have valence
%Uncertainty about goals: may want to communicate both valence and state or
%just one or the other

%Use Metropolis Hastings to sample joint distribution of PL1(dict, meaning,
%valence,goal,utterance) for every utterance

global hardness depth meanings utterances num_states num_utterances num_goal_combinations
global meaning_prior valence_prior utterance_costs lexical_entries num_lexical_entries
 
%__________________
%Set parameters

hardness = 2;
depth = 1;

%These are the possible meanings, i.e. the numerical states
%meanings = [1:20];
%meanings = [1 2];
meanings = [1 2 3 4 5 6 7 8];

%These are the possible utterances -- we are using two "2's" because one
%will be costly
%utterances = [1:20];
%utterances = [1 2 3];
utterances = [1 2 3 4 5 6 7 8];

num_states = size(meanings,2);
num_utterances = size(utterances,2);

%Prior on meanings (in log space)
%meaning_prior = log(arrayfun(@(x) 1/x^2,1:num_states)); 
%Note that this is unnormalized; but we only need the probabilities up to the normalizing constant
%meaning_prior = log(fliplr(1:num_states)/sum(1:num_states));
%meaning_prior = log([0.8 0.2]);

%%heavy tail
%meaning_prior = log([0.35 0.35 0.1 0.1 0.05 0.05]);

%%thin tail
meaning_prior = log([0.399 0.399 0.08 0.08 0.02 0.02 0.001 0.001]);

%Prior that the speaker is opinionated
%valence_prior = log([0.8 0.2]);

%Uniform valence prior
%valence_prior = log([0.8 0.8 0.8 0.8 0.8 0.8]);

%Moderate valence prior
valence_prior = log([0.8 0.8 0.7 0.7 0.5 0.5 0.01 0.01]);

%Extreme valence prior
%valence_prior = log([0.9 0.9 0.7 0.7 0.2 0.2]);

%valence_prior = log(arrayfun(@(x) 1/(x+0.1),1:num_states)); 
%valence_prior = log(fliplr(1:num_states)/sum(1:num_states));

%These are inverse costs!
%utterance_costs = [4 4 1];
%Alternating costs
utterance_costs = [4 1 4 1 4 1 4 1];

%Uniform costs
%utterance_costs = [1 1 1 1 1 1];
%utterance_costs = ones(1,num_utterances)*4;
%utterance_costs(5) = 1;
%utterance_costs(10) = 1;


num_goal_combinations = 3; %value 1 is when the speaker wants to communicate both valence and state; 2 is wants to communicate just state; 3 is wants to communicate just valence


%first column of lexical entries is the sd
%second and third column is whether the utterance is consistent with
%valence 0 and valence 1

lexical_entries = [1; 0.1];
num_lexical_entries = size(lexical_entries,1);


%Compute higher depth speakers and listeners
%________________________________________________

listener_higher_depths = zeros(num_utterances, num_states, 2, depth+1);
speaker_higher_depths = zeros(num_states,2,num_goal_combinations, num_utterances, depth);

%Enumerate through utterances and sample 
%listener_1_(dictionary,meaning,valence,goal,utterance)
for i=1:num_utterances
   current_samples = listener_1_sampler(i);
   current_samples = current_samples/(sum(sum(current_samples)));
   listener_higher_depths(i,:,:,1) = log(current_samples);
end


%Now calculate beliefs and moves for higher depths
for i=1:depth
    
    for j=1:num_states
        answer_propto = zeros(num_goal_combinations, num_utterances);
        for k=1:2 %loop through valences
            %answer_propto(m,:) is answer for goal=m
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

end

%--------------------
% Computes naive listener L0 given utterance and variance (variance sampled
% through MCMC)
function naive_listener = compute_LO(utterance,var)

global meanings num_states meaning_prior valence_prior 
 
%The last dimension determines whether we are specifying the probability of valence
%0 or valence 1
naive_listener = zeros(num_states,2);

%Compute unnormalized distribution for naive listener
for i=1:num_states
    answer_propto = meaning_prior(i) + log_of_normal(meanings(i),utterance,var); %Combine meaning prior with literal distribution
    naive_listener(i,1) = answer_propto + valence_prior(i); %Compute joint probability of state and valence 0
    naive_listener(i,2) = answer_propto + log(1-exp(valence_prior(i)));  %Compute joint probability of state and valence 1
end

%now renormalize
naive_listener = naive_listener - (log(sum(sum(exp(naive_listener-max(max(naive_listener)))))) + max(max(naive_listener)));
end

%------------------
%Computes PS0(utterance | dictionary, meaning, valence, goal)

function speaker_depth_0 = compute_speaker_0(dictionary, meaning, valence, goal)

global hardness utterances num_states num_utterances utterance_costs

speaker_depth_0 = zeros(num_utterances, 1);
binders_of_naive_listeners = zeros(num_utterances,num_states,2);
for i=1:num_utterances
   binders_of_naive_listeners(i,:,:) = compute_LO(utterances(i),dictionary(i)); 
end

% Goal: communicate both meaning and valence
if goal==1
    speaker_depth_0 = hardness*(utterance_costs + binders_of_naive_listeners(:,meaning,valence)');

% Goal: communicate just valence
elseif goal==2
    speaker_depth_0 = hardness*(utterance_costs + squeeze(log(sum(exp(binders_of_naive_listeners(:,meaning,:)),3)))');

% Goal: communicate just meaning
elseif goal==3
    speaker_depth_0 = hardness*(utterance_costs + squeeze(log(sum(exp(binders_of_naive_listeners(:,:,valence)),2)))');
end

%now we compute normalizing constant
speaker_depth_0 = speaker_depth_0 - ...
    (log(sum(exp(speaker_depth_0 - max(speaker_depth_0)))) + ...
    max(speaker_depth_0));
end

%Use MCMC to sample listener1(dictionary, meaning, valence, goal, utterance) given an utterance

function samples = listener_1_sampler(utterance)
 
global num_states num_utterances meaning_prior lexical_entries 
num_samples = 10000;

samples = zeros(num_states, 2);

%get initial sample
current_dict = randsample(lexical_entries, num_utterances, true);
current_meaning = discretesample(exp(meaning_prior),1);
current_valence = discretesample([0.8 0.2],1);
current_goal = discretesample([1/3 1/3 1/3],1);

current_speaker = compute_speaker_0(current_dict, current_meaning, current_valence, current_goal);

for i=1:num_samples
    
    proposal_dict = current_dict;
    for j=1:num_utterances
        if rand < 0.1
            proposal_dict(j) = randsample(lexical_entries, 1, true);
        end
    end
    
    
    proposal_meaning = current_meaning + 2*randn(1);
    proposal_meaning = round(proposal_meaning);
    proposal_meaning = mod(proposal_meaning,num_states);
    if proposal_meaning == 0
        proposal_meaning = num_states;
    end
    
    if rand < 0.2
        proposal_valence = mod(current_valence,2) +1;
    else
        proposal_valence = current_valence;
        
    end
    
    if rand < 0.2
        proposal_goal = discretesample([1/3 1/3 1/3],1);
    else
        proposal_goal = current_goal;
        
    end
    
    
    proposal_speaker = compute_speaker_0(proposal_dict, proposal_meaning, proposal_valence, proposal_goal);
    
    
    current_mass = compute_listener_mass(current_meaning, current_valence, current_speaker(utterance));
    
    proposal_mass = compute_listener_mass(proposal_meaning, proposal_valence, proposal_speaker(utterance));
    if rand < exp(proposal_mass - current_mass)
        current_dict = proposal_dict;
        current_meaning = proposal_meaning;
        current_valence = proposal_valence;
        current_goal = proposal_goal;
        
    end
    
    samples(current_meaning, current_valence) = samples(current_meaning, current_valence) + 1;
    
    
    
end



end


function log_prob = compute_listener_mass(meaning, valence, likelihood)

global meaning_prior valence_prior 

if valence == 1
    log_prob = meaning_prior(meaning) + valence_prior(meaning) + likelihood;
elseif valence == 2
    log_prob = meaning_prior(meaning) + log(1-exp(valence_prior(meaning))) + likelihood;
end

end