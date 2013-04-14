function [ outputs ] = cutOffAt( inputs, cutoff )

outputs = inputs;
outputs(find(inputs >= cutoff)) = cutoff - 0.00001;

end

