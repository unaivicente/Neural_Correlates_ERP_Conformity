S=dir('*R.set');
freq=1:30;
SCALE=1024*1.5./freq; %cmor1s-1.5

for suje=1:length(S)

EEG = pop_loadset('filename',S(suje).name,'filepath','/Users/unaiv/Desktop/Analysis/timefreq/Data/');

%%%

EEG1 = pop_epoch( EEG, {  '2'  }, [-2  2], 'newname', 'EEProbe continuous data epochs pruned with ICA pruned with ICA epochs', 'epochinfo', 'yes');
EEG1 = pop_rmbase( EEG1, [-100    0]);
L1D=find(max(max(abs(EEG1.data(:,1946:3073,:))))>100);

EEG1 = eeg_checkset( EEG1 );


clear POWD
kelec=1;
for elec=[6 15 24]
    for trial=1:size(EEG1.data,3)
        COEFS=cwt(EEG1.data(elec,:,trial),SCALE,'cmor1-1.5');
        POWD(:,:,kelec,trial)=(abs(COEFS).^2);
    end;
    kelec=kelec+1;
end;

if ~isempty(L1D)
    for uu1D=1:length(L1D)
        POWD(:,:,:,L1D(uu1D))=NaN;
    end
end

BASE=repmat(nanmean(nanmean(POWD(:,1639:1946,:,:),2),4),1,size(EEG1.data,2)); %from -400ms to -100

POWD=POWD./BASE;

APOWD2(:,:,:,:,suje)=POWD;

end

save('/Users/unaiv/Desktop/Analysis/timefreq/APOWD2.mat','APOWD2', '-v7.3');


for suje=1:length(S)

EEG = pop_loadset('filename',S(suje).name,'filepath','/Users/unaiv/Desktop/Analysis/timefreq/Data/');

%%%

EEG1 = pop_epoch( EEG, {  '4'  }, [-2  2], 'newname', 'EEProbe continuous data epochs pruned with ICA pruned with ICA epochs', 'epochinfo', 'yes');
EEG1 = pop_rmbase( EEG1, [-100    0]);
L1C=find(max(max(abs(EEG1.data(:,1946:3073,:))))>100);

EEG1 = eeg_checkset( EEG1 );


clear POWC
kelec=1;
for elec=[6 15 24]
    for trial=1:size(EEG1.data,3)
        COEFS=cwt(EEG1.data(elec,:,trial),SCALE,'cmor1-1.5');
        POWC(:,:,kelec,trial)=(abs(COEFS).^2);
    end;
    kelec=kelec+1;
end;

if ~isempty(L1C)
    for uu1C=1:length(L1C)
        POWC(:,:,:,L1C(uu1C))=NaN;
    end
end

BASE=repmat(nanmean(nanmean(POWC(:,1639:1946,:,:),2),4),1,size(EEG1.data,2));

POWC=POWC./BASE;

APOWC2(:,:,:,:,suje)=POWC;

end

save('/Users/unaiv/Desktop/Analysis/timefreq/APOWC2.mat','APOWC2', '-v7.3');


for suje=1:length(S)

EEG = pop_loadset('filename',S(suje).name,'filepath','/Users/unaiv/Desktop/Analysis/timefreq/Data/');

%%%

EEG1 = pop_epoch( EEG, {  '6'  }, [-2  2], 'newname', 'EEProbe continuous data epochs pruned with ICA pruned with ICA epochs', 'epochinfo', 'yes');
EEG1 = pop_rmbase( EEG1, [-100    0]);
L1S=find(max(max(abs(EEG1.data(:,1946:3073,:))))>100);

EEG1 = eeg_checkset( EEG1 );


clear POWS
kelec=1;
for elec=[6 15 24]
    for trial=1:size(EEG1.data,3)
        COEFS=cwt(EEG1.data(elec,:,trial),SCALE,'cmor1-1.5');
        POWS(:,:,kelec,trial)=(abs(COEFS).^2);
    end;
    kelec=kelec+1;
end;

if ~isempty(L1S)
    for uu1S=1:length(L1S)
        POWS(:,:,:,L1S(uu1S))=NaN;
    end
end

BASE=repmat(nanmean(nanmean(POWS(:,1639:1946,:,:),2),4),1,size(EEG1.data,2));

POWS=POWS./BASE;

APOWS2(:,:,:,:,suje)=POWS;

end

save('/Users/unaiv/Desktop/Analysis/timefreq/APOWS2.mat','APOWS2', '-v7.3');

%%%%THETA

%%frequencies 4 to 8 from 180 to 230ms


fid=fopen('/Users/unaiv/Desktop/Analysis/timefreq/freqP4_8_180.csv','w');
fprintf(fid,'valor;sujeto;electrodo;condicion;trial\n');
for i=1:36
    for elec=1:3
        for trials=1:100
            AA=nanmean(nanmean(APOWD2(4:8,2234:2285,elec,trials,i),1),2);
            fprintf(fid,'%f;%i;%i;%s;%i\n',AA,i,elec,'2',trials);
            BB=nanmean(nanmean(APOWC2(4:8,2234:2285,elec,trials,i),1),2);
            fprintf(fid,'%f;%i;%i;%s;%i\n',BB,i,elec,'4',trials);
            CC=nanmean(nanmean(APOWS2(4:8,2234:2285,elec,trials,i),1),2);
            fprintf(fid,'%f;%i;%i;%s;%i\n',CC,i,elec,'6',trials);
        end;
    end;
end;


fclose(fid);


%%frequencies 4 to 8 from 230 to 500ms


fid=fopen('/Users/unaiv/Desktop/Analysis/timefreq/freqP4_8_230.csv','w');
fprintf(fid,'valor;sujeto;electrodo;condicion;trial\n');
for i=1:36
    for elec=1:3
        for trials=1:100
            AA=nanmean(nanmean(APOWD2(4:8,2285:2561,elec,trials,i),1),2);
            fprintf(fid,'%f;%i;%i;%s;%i\n',AA,i,elec,'2',trials);
            BB=nanmean(nanmean(APOWC2(4:8,2285:2561,elec,trials,i),1),2);
            fprintf(fid,'%f;%i;%i;%s;%i\n',BB,i,elec,'4',trials);
            CC=nanmean(nanmean(APOWS2(4:8,2285:2561,elec,trials,i),1),2);
            fprintf(fid,'%f;%i;%i;%s;%i\n',CC,i,elec,'6',trials);
        end;
    end;
end;

fclose(fid);

%%%%ALPHA

%%frequencies 8 to 12 from 180 to 230ms


fid=fopen('/Users/unaiv/Desktop/Analysis/timefreq/freqP8_12_180.csv','w');
fprintf(fid,'valor;sujeto;electrodo;condicion;trial\n');
for i=1:36
    for elec=1:3
        for trials=1:100
            AA=nanmean(nanmean(APOWD2(8:12,2234:2285,elec,trials,i),1),2);
            fprintf(fid,'%f;%i;%i;%s;%i\n',AA,i,elec,'2',trials);
            BB=nanmean(nanmean(APOWC2(8:12,2234:2285,elec,trials,i),1),2);
            fprintf(fid,'%f;%i;%i;%s;%i\n',BB,i,elec,'4',trials);
            CC=nanmean(nanmean(APOWS2(8:12,2234:2285,elec,trials,i),1),2);
            fprintf(fid,'%f;%i;%i;%s;%i\n',CC,i,elec,'6',trials);
        end;
    end;
end;


fclose(fid);


%%frequencies 8 to 12 from 230 to 500ms


fid=fopen('/Users/unaiv/Desktop/Analysis/timefreq/freqP8_12_230.csv','w');
fprintf(fid,'valor;sujeto;electrodo;condicion;trial\n');
for i=1:36
    for elec=1:3
        for trials=1:100
            AA=nanmean(nanmean(APOWD2(8:12,2285:2561,elec,trials,i),1),2);
            fprintf(fid,'%f;%i;%i;%s;%i\n',AA,i,elec,'2',trials);
            BB=nanmean(nanmean(APOWC2(8:12,2285:2561,elec,trials,i),1),2);
            fprintf(fid,'%f;%i;%i;%s;%i\n',BB,i,elec,'4',trials);
            CC=nanmean(nanmean(APOWS2(8:12,2285:2561,elec,trials,i),1),2);
            fprintf(fid,'%f;%i;%i;%s;%i\n',CC,i,elec,'6',trials);
        end;
    end;
end;

fclose(fid);

%%%%BETA

%%frequencies 12 to 30 from 180 to 230ms


fid=fopen('/Users/unaiv/Desktop/Analysis/timefreq/freqP12_30_180.csv','w');
fprintf(fid,'valor;sujeto;electrodo;condicion;trial\n');
for i=1:36
    for elec=1:3
        for trials=1:100
            AA=nanmean(nanmean(APOWD2(12:30,2234:2285,elec,trials,i),1),2);
            fprintf(fid,'%f;%i;%i;%s;%i\n',AA,i,elec,'2',trials);
            BB=nanmean(nanmean(APOWC2(12:30,2234:2285,elec,trials,i),1),2);
            fprintf(fid,'%f;%i;%i;%s;%i\n',BB,i,elec,'4',trials);
            CC=nanmean(nanmean(APOWS2(12:30,2234:2285,elec,trials,i),1),2);
            fprintf(fid,'%f;%i;%i;%s;%i\n',CC,i,elec,'6',trials);
        end;
    end;
end;


fclose(fid);


%%frequencies 12 to 30 from 230 to 500ms


fid=fopen('/Users/unaiv/Desktop/Analysis/timefreq/freqP12_30_230.csv','w');
fprintf(fid,'valor;sujeto;electrodo;condicion;trial\n');
for i=1:36
    for elec=1:3
        for trials=1:100
            AA=nanmean(nanmean(APOWD2(12:30,2285:2561,elec,trials,i),1),2);
            fprintf(fid,'%f;%i;%i;%s;%i\n',AA,i,elec,'2',trials);
            BB=nanmean(nanmean(APOWC2(12:30,2285:2561,elec,trials,i),1),2);
            fprintf(fid,'%f;%i;%i;%s;%i\n',BB,i,elec,'4',trials);
            CC=nanmean(nanmean(APOWS2(12:30,2285:2561,elec,trials,i),1),2);
            fprintf(fid,'%f;%i;%i;%s;%i\n',CC,i,elec,'6',trials);
        end;
    end;
end;

fclose(fid);

