S=dir('*R.set');
C500FZ=[];
C500CZ=[];
C500PZ=[];
C500OZ=[];


C230FZ=[];
C230CZ=[];
C230PZ=[];
C230OZ=[];


C300FZ=[];
C300CZ=[];
C300PZ=[];
C300OZ=[];


C350FZ=[];
C350CZ=[];
C350PZ=[];
C350OZ=[];


for suje=1:length(S)

C500FZa=[];
C500CZa=[];
C500PZa=[];


C230FZa=[];
C230CZa=[];
C230PZa=[];


C300FZa=[];
C300CZa=[];
C300PZa=[];


C350FZa=[];
C350CZa=[];
C350PZa=[];

EEG = pop_loadset('filename',S(suje).name,'filepath','~/Google Drive/Doctorado Social Neuroscience/Exp 1 - Social Conformity/Results/EEG Data/');

EEG = pop_reref( EEG, [13 17] ,'keepref','on');


EEG1 = pop_epoch( EEG, {  '2'  }, [-2  2], 'newname', 'EEProbe continuous data epochs pruned with ICA pruned with ICA epochs', 'epochinfo', 'yes');
%EEG1 = pop_rmbase( EEG1, [-100    0]);

C500FZa=[C500FZa;[squeeze(EEG1.data(6,2561:2766,:),2) ones(100,1)*2]];
C500CZa=[C500CZa;[squeeze(EEG1.data(15,2561:2766,:),2) ones(100,1)*2]];
C500PZa=[C500PZa;[squeeze(EEG1.data(24,2561:2766,:),2) ones(100,1)*2]];


C230FZa=[C230FZa;[squeeze(EEG1.data(6,2239:2285,:),2) ones(100,1)*2]];%180-230
C230CZa=[C230CZa;[squeeze(EEG1.data(15,2239:2285,:),2) ones(100,1)*2]];
C230PZa=[C230PZa;[squeeze(EEG1.data(24,2239:2285,:),2) ones(100,1)*2]];


C300FZa=[C300FZa;[squeeze(EEG1.data(6,2357:2408,:),2) ones(100,1)*2]];
C300CZa=[C300CZa;[squeeze(EEG1.data(15,2357:2408,:),2) ones(100,1)*2]];
C300PZa=[C300PZa;[squeeze(EEG1.data(24,2357:2408,:),2) ones(100,1)*2]];


C350FZa=[C350FZa;[squeeze(EEG1.data(6,2510:2561,:),2) ones(100,1)*2]];
C350CZa=[C350CZa;[squeeze(EEG1.data(15,2510:2561,:),2) ones(100,1)*2]];
C350PZa=[C350PZa;[squeeze(EEG1.data(24,2510:2561,:),2) ones(100,1)*2]];


EEG1 = pop_eegthresh(EEG1,1,[1:30] ,-100,100,-0.1,1,1,1);
EEG1 = eeg_checkset( EEG1 );


EEG2 = pop_epoch( EEG, {  '4'  }, [-2  2], 'newname', 'EEProbe continuous data epochs pruned with ICA pruned with ICA epochs', 'epochinfo', 'yes');
%EEG2 = pop_rmbase( EEG2, [-100    0]);

C500FZa=[C500FZa;[squeeze(EEG2.data(6,2561:2766,:),2) ones(100,1)*4]];
C500CZa=[C500CZa;[squeeze(EEG2.data(15,2561:2766,:),2) ones(100,1)*4]];
C500PZa=[C500PZa;[squeeze(EEG2.data(24,2561:2766,:),2) ones(100,1)*4]];


C230FZa=[C230FZa;[squeeze(EEG2.data(6,2285:2326,:),2) ones(100,1)*4]];
C230CZa=[C230CZa;[squeeze(EEG2.data(15,2285:2326,:),2) ones(100,1)*4]];
C230PZa=[C230PZa;[squeeze(EEG2.data(24,2285:2326,:),2) ones(100,1)*4]];


C300FZa=[C300FZa;[squeeze(EEG2.data(6,2357:2408,:),2) ones(100,1)*4]];
C300CZa=[C300CZa;[squeeze(EEG2.data(15,2357:2408,:),2) ones(100,1)*4]];
C300PZa=[C300PZa;[squeeze(EEG2.data(24,2357:2408,:),2) ones(100,1)*4]];

C350FZa=[C350FZa;[squeeze(EEG2.data(6,2510:2561,:),2) ones(100,1)*2]];
C350CZa=[C350CZa;[squeeze(EEG2.data(15,2510:2561,:),2) ones(100,1)*2]];
C350PZa=[C350PZa;[squeeze(EEG2.data(24,2510:2561,:),2) ones(100,1)*2]];


EEG2 = pop_eegthresh(EEG2,1,[1:30] ,-100,100,-0.1,1,1,1);
EEG2 = eeg_checkset( EEG2 );

EEG3 = pop_epoch( EEG, {  '6'  }, [-2  2], 'newname', 'EEProbe continuous data epochs pruned with ICA pruned with ICA epochs', 'epochinfo', 'yes');
%EEG3 = pop_rmbase( EEG3, [-100    0]);

C500FZa=[C500FZa;[squeeze(EEG3.data(6,2561:2766,:),2) ones(100,1)*4]];
C500CZa=[C500CZa;[squeeze(EEG3.data(15,2561:2766,:),2) ones(100,1)*4]];
C500PZa=[C500PZa;[squeeze(EEG3.data(24,2561:2766,:),2) ones(100,1)*4]];


C230FZa=[C230FZa;[squeeze(EEG3.data(6,2285:2326,:),2) ones(100,1)*4]];
C230CZa=[C230CZa;[squeeze(EEG3.data(15,2285:2326,:),2) ones(100,1)*4]];
C230PZa=[C230PZa;[squeeze(EEG3.data(24,2285:2326,:),2) ones(100,1)*4]];


C300FZa=[C300FZa;[squeeze(EEG3.data(6,2357:2408,:),2) ones(100,1)*4]];
C300CZa=[C300CZa;[squeeze(EEG3.data(15,2357:2408,:),2) ones(100,1)*4]];
C300PZa=[C300PZa;[squeeze(EEG3.data(24,2357:2408,:),2) ones(100,1)*4]];

C350FZa=[C350FZa;[squeeze(EEG3.data(6,2510:2561,:),2) ones(100,1)*2]];
C350CZa=[C350CZa;[squeeze(EEG3.data(15,2510:2561,:),2) ones(100,1)*2]];
C350PZa=[C350PZa;[squeeze(EEG3.data(24,2510:2561,:),2) ones(100,1)*2]];

EEG3 = pop_eegthresh(EEG3,1,[1:30] ,-100,100,-0.1,1,1,1);
EEG3 = eeg_checkset( EEG3 );

%DOS(:,:,suje)=(EEG1.data,3);

%CUA(:,:,suje)=(EEG2.data,3);

%SEI(:,:,suje)=(EEG3.data,3);

C500FZa=[C500FZa ones(300,1)*suje];
C500FZ=[C500FZ;C500FZa];
C500CZa=[C500CZa ones(300,1)*suje];
C500CZ=[C500CZ;C500CZa];
C500PZa=[C500PZa ones(300,1)*suje];
C500PZ=[C500PZ;C500PZa];


C230FZa=[C230FZa ones(300,1)*suje];
C230FZ=[C230FZ;C230FZa];
C230CZa=[C230CZa ones(300,1)*suje];
C230CZ=[C230CZ;C230CZa];
C230PZa=[C230PZa ones(300,1)*suje];
C230PZ=[C230PZ;C230PZa];


C300FZa=[C300FZa ones(300,1)*suje];
C300FZ=[C300FZ;C300FZa];
C300CZa=[C300CZa ones(300,1)*suje];
C300CZ=[C300CZ;C300CZa];
C300PZa=[C300PZa ones(300,1)*suje];
C300PZ=[C300PZ;C300PZa];


C350FZa=[C350FZa ones(300,1)*suje];
C350FZ=[C350FZ;C350FZa];
C350CZa=[C350CZa ones(300,1)*suje];
C350CZ=[C350CZ;C350CZa];
C350PZa=[C350PZa ones(300,1)*suje];
C350PZ=[C350PZ;C350PZa];


csvwrite('ALLdata_C500_Fz',C500FZ)
csvwrite('ALLdata_C500_Cz',C500CZ)
csvwrite('ALLdata_C500_Pz',C500PZ)

csvwrite('ALLdata_C230_Fz',C230FZ)
csvwrite('ALLdata_C230_Cz',C230CZ)
csvwrite('ALLdata_C230_Pz',C230PZ)

csvwrite('ALLdata_C300_Fz',C300FZ)
csvwrite('ALLdata_C300_Cz',C300CZ)
csvwrite('ALLdata_C300_Pz',C300PZ)

csvwrite('ALLdata_C350_Fz',C350FZ)
csvwrite('ALLdata_C350_Cz',C350CZ)
csvwrite('ALLdata_C350_Pz',C350PZ)

end;

save('EEG1_ERP.mat','EEG1');
save('EEG2_ERP.mat','EEG2');
save('EEG3_ERP.mat','EEG3');


