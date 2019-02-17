function matlabconfig(path)
    try
        f=fopen(path,'wt');
        fprintf(f,['MATLABROOT="' matlabroot '"\n']);
        fclose(f);
    catch e
        disp('writing matlab path failed')
    end
    quit;
end
