function [] = test(filename)

MAXNUM=31;

fid = fopen(filename, 'rt');
if fid == -1
	fprintf(2,'No such file, exiting.\n');
	quit
end

header = textscan(fid,'%f','CommentStyle','#');

images=[];
nelem = header{1}(1);
for i=1:nelem;
	input.name=fgetl(fid);
	temp=textscan(fid,'%f','CommentStyle','#');
	input.data = temp{1}./MAXNUM;
	images{i}=input;

    
end
fclose(fid);
load('ew.mat');
load('mw.mat');
answers = zeros(0,length(images));
for j=1:length(images)
	answers(j)=getFace(ew*images{j}.data,mw*images{j}.data);
	fprintf('%s %d\n',images{j}.name, answers(j));
end

