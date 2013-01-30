function [ answers,corrects ] = percieve( images , ew, mw)


answers = zeros(0,length(images));
corrects = 0;
for j=1:length(images)

        answers(j)=getFace(ew*images{j}.data,mw*images{j}.data);
		if answers(j) == getFace( images{j}.eye , images{j}.mouth)
            corrects = corrects + 1;
        end
end

end

