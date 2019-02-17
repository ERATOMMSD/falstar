function matlabconfig(path)
    disp(path);
    f=fopen(path,'wt');
    fprintf(f,['MATLABROOT="' matlabroot '"\n']);
    fclose(f);
end
