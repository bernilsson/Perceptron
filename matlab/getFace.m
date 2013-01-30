function [y] = getFace(eye,mouth)
% 0 = Good, 1 = Bad STEP

y=1;
if eye >= 0.5
	y = y + 2;
end
if mouth >= 0.5
    y = y + 1;
end

