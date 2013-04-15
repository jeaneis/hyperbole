function [ outputs ] = cutOffAt( inputs, cutoff, options )
% [ outputs ] = cutOffAt(inputs, cutoffValue, options)
% options:  'above' - values in inputs above cutoff are rounded down
%           'below' - values in inputs below cutoff are rounded up

assert(ismember(options, {'above', 'below'})); 
outputs = inputs;

if options == 'above'
    outputs(find(inputs >= cutoff)) = cutoff - eps;
elseif options == 'below'
    outputs(find(inputs <= cutoff)) = cutoff + eps;
    
end

