function [displayMat] = displayResults(listener, meanings_labels, utterances_labels, affect_prior, outFileName)

assert(length(size(listener)) == 3);
 
% %displayMat = cell(prod(size(listener)), 4);
% 
% phi = struct('var', [1 2 3], 'card', size(listener), 'val', numel(listener));
% 
% phi.val = reshape(listener, 1, numel(listener));
% 
% %   GetValueOfAssignment(phi, [1 2 1]) 
% %
% % yields the value phi(X_3 = 1, X_1 = 2, X_2 = 1). Again, the variables for 
% % which the assignments refer to are given by the .var field of the factor.
% %
 
% create a zero matrix of the number of elements in listener x 4
displayMat = zeros(numel(listener), 4);
% columns 1-3
displayMat(:,1:3) = IndexToAssignment(1:numel(listener), size(listener));
% column 4 is just all elements of the listener reshape to a vector
displayMat(:,4)   = reshape(listener, 1, numel(listener))';

% writeToFileNoLabels(displayMat, outFileName);
writeToFile(displayMat, meanings_labels, utterances_labels, affect_prior, outFileName);
 
end

function A = IndexToAssignment(I, D)
 
D = D(:)'; % ensure that D is a row vector
A = mod(floor(repmat(I(:) - 1, 1, length(D)) ./ repmat(cumprod([1, D(1:end - 1)]), length(I), 1)), ...
        repmat(D, length(I), 1)) + 1;
  
end

function writeToFile( displayMat, meanings_labels, utterances_labels, affect_prior, outFileName )

fid = fopen(['../../data/model/predict_',outFileName], 'w');
fprintf(fid, 'utterance,meaning,valence,probability,affect_prior\n');

for i = 1:size(displayMat,1),
    fprintf(fid, '%s,%s,%s,%s,%s\n', ...
        num2str(utterances_labels(displayMat(i,1))), ... 
        num2str(meanings_labels(displayMat(i,2))), ...
        num2str(displayMat(i,3)), ...
        num2str(displayMat(i,4)), ...
        num2str(affect_prior(displayMat(i,2))));
end
 
fclose(fid);
 
end


function writeToFileNoLabels( displayMat, outFileName )
 
fid = fopen(['../../data/model/predict_',outFileName], 'w');
fprintf(fid, 'utterance,meaning,valence,probability\n');

for i = 1:size(displayMat,1),
    fprintf(fid, '%s,%s,%s,%s\n', ...
        num2str(displayMat(i,1)), ... 
        num2str(displayMat(i,2)), ...
        num2str(displayMat(i,3)), ...
        num2str(displayMat(i,4)));
end
 
fclose(fid);
 
end

