MAXNUM=31;

fid = fopen('../training.txt', 'rt') 
ansfid = fopen('../training-facit.txt','rt')
header = textscan(fid,'%f','CommentStyle','#')
anshead = textscan(ansfid,'%f','CommentStyle','#')
nelem = header{1}(1)
images=[];
for i=1:nelem;
	input.name=fgetl(fid);
	temp=textscan(fid,'%f','CommentStyle','#');
	input.data = temp{1}./MAXNUM;
	images{i}=input;

	garb = fscanf(ansfid,'%s',1);
	[eye,mouth] = faceToEyeMouth(fscanf(ansfid,'%f',1));
    images{i}.eye  = eye;
    images{i}.mouth  = mouth;
    
end
fclose(fid);
fclose(ansfid)

ew = random('unif',-0.3,0.3,1,400);
mw = random('unif',-0.3,0.3,1,400);
lr = 0.001;
[trainSet,testSet] = shfl_and_split(images,30);
for i=1:1000
    [ew,mw] = train(trainSet,ew,mw,lr);
end

img = (reshape( mw,20,20));
imagesc(img);figure(gcf);
