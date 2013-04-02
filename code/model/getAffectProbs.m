function affectProbs = getAffectProbs(price_vector, affect_priors)
% takes in a vector of prices and a matrix of affect priors, then
% returns  the estimated probabiity of having an affect given the prices

affectProbs = zeros(1,size(price_vector,2));
for i = 1:size(price_vector,2)
    index = 1;
    price = price_vector(i);
    for j = 1:size(affect_priors, 1)
        if affect_priors(j,1) - price >= 0
            index = j;
        end
    end
    if index == 1 
        affectProb = affect_priors(index,2);
    else
        prop = (price - affect_priors(index - 1, 1)) / (affect_priors(index, 1) - affect_priors(index - 1,1));
        affectProb = affect_priors(index - 1, 2) + (affect_priors(index,2) - affect_priors(index - 1, 2)) * prop;
    end
    affectProbs(i) = affectProb;
    
end
end