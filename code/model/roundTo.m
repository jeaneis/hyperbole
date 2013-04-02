% takes in a vector and rounds all numbers to the nearest number that's divisable by
% "unit"
function roundedNumbers = roundTo(vector, unit)
for i = 1:size(vector,2)
    number = vector(i);
    vector(i) = number - mod(number, unit);
    roundedNumbers = vector;
end
end