%%frequencias positivas 4 a 8 de 300 a 500ms

D=squeeze(mean(mean(APDB(4:8,2357:2561,:,:),1),2));
C=squeeze(mean(mean(APCB(4:8,2357:2561,:,:),1),2));
S=squeeze(mean(mean(APSB(4:8,2357:2561,:,:),1),2));



fid=fopen('freqP4_8.csv','w');
fprintf(fid,'valor;sujeto;electrodo;condicion\n');
for elec=[5:7 14:16 23:25]
for i=1:36

fprintf(fid,'%f;%i;%i;%s\n',D(elec,i),i,elec,'2');
fprintf(fid,'%f;%i;%i;%s\n',C(elec,i),i,elec,'4');
fprintf(fid,'%f;%i;%i;%s\n',S(elec,i),i,elec,'6');

end;
end;



fclose(fid);



%%frequencias positivas 8 a 12 de 180 a 230ms

D=squeeze(mean(mean(APDB(8:12,2239:2285,:,:),1),2));
C=squeeze(mean(mean(APCB(8:12,2239:2285,:,:),1),2));
S=squeeze(mean(mean(APSB(8:12,2239:2285,:,:),1),2));


fid=fopen('freqP8_12.csv','w');
fprintf(fid,'valor;sujeto;electrodo;condicion\n');
for elec=[5:7 14:16 23:25]
for i=1:36

fprintf(fid,'%f;%i;%i;%s\n',D(elec,i),i,elec,'2');
fprintf(fid,'%f;%i;%i;%s\n',C(elec,i),i,elec,'4');
fprintf(fid,'%f;%i;%i;%s\n',S(elec,i),i,elec,'6');

end;
end;



fclose(fid);


%%frequencias positivas 12 a 30 de 180 a 230ms

D=squeeze(mean(mean(APDB(12:30,2239:2285,:,:),1),2));
C=squeeze(mean(mean(APCB(12:30,2239:2285,:,:),1),2));
S=squeeze(mean(mean(APSB(12:30,2239:2285,:,:),1),2));


fid=fopen('freqP12_30.csv','w');
fprintf(fid,'valor;sujeto;electrodo;condicion\n');
for elec=[5:7 14:16 23:25]
for i=1:36

fprintf(fid,'%f;%i;%i;%s\n',D(elec,i),i,elec,'2');
fprintf(fid,'%f;%i;%i;%s\n',C(elec,i),i,elec,'4');
fprintf(fid,'%f;%i;%i;%s\n',S(elec,i),i,elec,'6');

end;
end;



fclose(fid);



