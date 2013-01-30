function [ew,mw ] = train(images,ew,mw,lr)


for j=1:length(images)

		ey = sigmoid(ew*images{j}.data);
		my  = sigmoid(mw*images{j}.data);
        
        delta = (images{j}.eye-ey);
        ew = ew + lr*delta.*images{j}.data';
        
        delta = (images{j}.mouth-my);
        mw = mw + lr*delta.*images{j}.data';

end

end

