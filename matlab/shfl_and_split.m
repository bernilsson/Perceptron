function [trainSet,testSet] = shfl_and_split(images,testSize)
n = length(images);
r = randperm(n);
    for i = 1:testSize
        testSet(i) = images(r(i));
    end
    for i = testSize+1:n
        trainSet(i-testSize) = images(r(i));
    end
    
end