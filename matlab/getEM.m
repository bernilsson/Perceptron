function [ eye,mouth ] = getEM(face)

switch face
    case 1
       eye   = 0;
       mouth = 0;
    case 2
       eye   = 0;
       mouth = 1;
    case 3
       eye   = 1;
       mouth = 0;
    case 4
       eye   = 1;
       mouth = 1;
end

