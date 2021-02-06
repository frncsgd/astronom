Unit Ast_plan;

{$MODE Delphi}

interface

Uses LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, AST_FIC,AST_GEN, AST_SUN;

procedure orbites(instant : real;n:byte;
                  var vr,vdistance,vll,vb,vdelta ,vecart : real;
                  var valpha : str8) ;
procedure edit_planete(numplanete:byte;date1,date2:str10;
                       pas,latitude,longitude:real);
procedure edit_longhelio(date1,date2:str10;pas:real);

procedure elements;
procedure edition_planete;
procedure edition_longhelio;

implementation


var
 l,e,i,w,omega        	: array[1..7,0..3] of real;
 a                    	: array[1..7] of real;
 memodep,memofin        : string;
 memoper		: real;
procedure initialise(numplanete:byte);   (* chƒpitre 20*)
begin
 case numplanete of
  1 : (* MERCURE *)
     begin
      l[1,0]:=178.179078;
      a[1]:=0.3870986;
      e[1,0]:=0.20561421;
      l[1,1]:=149474.07078;
      e[1,1]:=0.00002046;
      l[1,2]:=0.0003011;
      e[1,2]:=-0.000000030;

      i[1,0]:=7.002881;
      w[1,0]:=28.753753;
      omega[1,0]:=47.145944;
      i[1,1]:=0.0018608;
      w[1,1]:=0.3702806;
      omega[1,1]:=1.1852083;
      i[1,2]:=-0.0001208;
      w[1,2]:=0.0001208;
      omega[1,2]:=0.0001739;

      l[1,3]:=0;
      e[1,3]:=0;
      i[1,3]:=0;
      w[1,3]:=0;
      omega[1,3]:=0;
     end;

  2 : (* VENUS *)
     begin
      l[2,0]:=342.767053;
      a[2]:=0.7233316;
      e[2,0]:=0.00682069;
      l[2,1]:=58519.21191;
      e[2,1]:=-0.00004774;
      l[2,2]:=0.0003097;
      e[2,2]:=0.000000091;

      i[2,0]:=3.393631;
      w[2,0]:=54.384186;
      omega[2,0]:=75.779647;
      i[2,1]:=0.0010058;
      w[2,1]:=0.5081861;
      omega[2,1]:=0.899850;
      i[2,2]:=-0.0000010;
      w[2,2]:=-0.0013864;
      omega[2,2]:=0.00041;

      l[2,3]:=0;
      e[2,3]:=0;
      i[2,3]:=0;
      w[2,3]:=0;
      omega[2,3]:=0;
     end;

     3 : (* MARS *)
     begin
      l[3,0]:=293.737334;
      a[3]:=1.5236883;
      e[3,0]:=0.09331290;
      l[3,1]:=19141.69551;
      e[3,1]:=0.000092064;
      l[3,2]:=0.0003107;
      e[3,2]:=-0.000000077;

      i[3,0]:=1.850333;
      w[3,0]:=285.431761;
      omega[3,0]:=48.786442;
      i[3,1]:=-0.000675;
      w[3,1]:=1.0697667;
      omega[3,1]:=0.7709917;
      i[3,2]:=-0.0000126;
      w[3,2]:=0.0001313;
      omega[3,2]:=-0.0000014;

      l[3,3]:=0;
      e[3,3]:=0;
      i[3,3]:=0;
      w[3,3]:=0.00000414;
      omega[3,3]:=-0.00000533;
     end;

    4 : (* JUPITER *)
     begin
      l[4,0]:=238.049257;
      a[4]:=5.202561;
      e[4,0]:=0.04833475;
      l[4,1]:=3036.301986;
      e[4,1]:=0.00016418;
      l[4,2]:=0.0003347;
      e[4,2]:=-0.0000004676;

      i[4,0]:=1.308736;
      w[4,0]:=273.277558;
      omega[4,0]:=99.443414;
      i[4,1]:=-0.0056961;
      w[4,1]:=0.5994317;
      omega[4,1]:=1.01053;
      i[4,2]:=0.0000039;
      w[4,2]:=0.00070405;
      omega[4,2]:=0.00035222;

      l[4,3]:=-0.00000165;
      e[4,3]:=-0.0000000017;
      i[4,3]:=0;
      w[4,3]:=0.00000508;
      omega[4,3]:=-0.00000851;
     end;

     5 : (* SATURNE *)
     begin
      l[5,0]:=266.564377;
      a[5]:=9.554747;
      e[5,0]:=0.05589232;
      l[5,1]:=1223.509884;
      e[5,1]:=-0.00034550;
      l[5,2]:=0.0003245;
      e[5,2]:=-0.000000728;

      i[5,0]:=2.492519;
      w[5,0]:=338.307800;
      omega[5,0]:=112.790414;
      i[5,1]:=-0.0039189;
      w[5,1]:=1.0852207;
      omega[5,1]:=0.8731951;
      i[5,2]:=-0.00001549;
      w[5,2]:=0.00097854;
      omega[5,2]:=-0.00015218;

      l[5,3]:=-0.0000058;
      e[5,3]:=0.00000000074;
      i[5,3]:=0.00000004;
      w[5,3]:=0.00000992;
      omega[5,3]:=0.00000531;
     end;

     6 : (* URANUS *)
     begin
      l[6,0]:=244.19747;
      a[6]:=19.21814;
      e[6,0]:=0.0463444;
      l[6,1]:=429.863546;
      e[6,1]:=-0.00002658;
      l[6,2]:=0.0003160;
      e[6,2]:=0.000000077;

      i[6,0]:=0.772464;
      w[6,0]:=98.071581;
      omega[6,0]:=73.477111;
      i[6,1]:=0.0006253;
      w[6,1]:=0.9857650;
      omega[6,1]:=0.4986678;
      i[6,2]:=0.0000395;
      w[6,2]:=-0.0010745;
      omega[6,2]:=0.0013117;

      l[6,3]:=-0.0000006;
      e[6,3]:=0;
      i[6,3]:=0;
      w[6,3]:=-0.00000061;
      omega[6,3]:=0;
     end;

    7 : (* NEPTUNE *)
     begin
      l[7,0]:=84.457994;
      a[7]:=30.10957;
      e[7,0]:=0.00899704;
      l[7,1]:=219.885914;
      e[7,1]:=0.000006330;
      l[7,2]:=0.0003205;
      e[7,2]:=-0.000000002;

      i[7,0]:=1.779242;
      w[7,0]:=276.045975;
      omega[7,0]:=130.681389;
      i[7,1]:=-0.0095436;
      w[7,1]:=0.3256394;
      omega[7,1]:=1.098935;
      i[7,2]:=-0.0000091;
      w[7,2]:=0.00014095;
      omega[7,2]:=0.00024987;

      l[7,3]:=-0.0000006;
      e[7,3]:=0;
      i[7,3]:=0;
      w[7,3]:=0.000004113;
      omega[7,3]:=-0.000004718;
     end;

 end;

end;

(**********************************************)
(*         fin proc‚dure initialise           *)
(**********************************************)
(*chƒpitre 20*)
procedure orbites(instant : real;n:byte;
                  var vr,vdistance,vll,vb,vdelta ,vecart : real;
                  var valpha : str8) ;
var
 vl,va,ve,vi,vw,vomega,vm,vee,vv : real;
 t,x,y,lambda,beta,epsilon,m      : real;
 vu : real;
 alpha : str8;
 delta,rayon,bidon,parallaxe,angsize :real;
 long_sol                            : real;
begin
 calc_soleil(instant,alpha,delta,rayon,long_sol,bidon,epsilon,m,parallaxe,angsize);

 t:=(instant-2415020)/36525;
 initialise(n);
 vl:=l[n,0]+l[n,1]*t+l[n,2]*sqr(t)+l[n,3]*sqr(t)*t;
 va:=a[n];
 ve:=e[n,0]+e[n,1]*t+e[n,2]*sqr(t)+e[n,3]*sqr(t)*t;
 vi:=i[n,0]+i[n,1]*t+i[n,2]*sqr(t)+i[n,3]*sqr(t)*t;
 vw:=w[n,0]+w[n,1]*t+w[n,2]*sqr(t)+w[n,3]*sqr(t)*t;
 vomega:=omega[n,0]+omega[n,1]*t+omega[n,2]*sqr(t)+omega[n,3]*sqr(t)*t;
 range0a360(vl,vl);
 range0a360(vomega,vomega);
 vm:=vl-vw-vomega;
 range0a360(vm,vm);
 kepler(ve,vm,vee);
 vv:=180/pi*2*arctan(sqrt((1+ve)/(1-ve))*tangente(vee/2));
 range0a360(vv,vv);
 vr:=va*(1-ve*cosinus(vee));
 vu:=vl+vv-vm-vomega;
 range0a360(vu,vu);
 y:=cosinus(vi)*sinus(vu);
 x:=cosinus(vu);
 corriquad(x,y,vll);
 vll:=vll+vomega;
 range0a360(vll,vll);

 vb:=arcsinus(sinus(vu)*sinus(vi));
 y:=vr*cosinus(vb)*sinus(vll-long_sol);
 x:=vr*cosinus(vb)*cosinus(vll-long_sol)+rayon;
 corriquad(x,y,lambda);
 lambda:=lambda+long_sol;

 vdistance:=sqrt(sqr(rayon)+sqr(vr)+2*rayon*vr*cosinus(vb)*cosinus(vll-long_sol));

 beta:=arcsinus(vr/vdistance*sinus(vb));

 par_27(lambda,beta,epsilon,valpha,vdelta);

 ecart(alpha,delta,valpha,vdelta,vecart);

end;

procedure elements;
var
 heure       : str8;
 date       : str10;
 chaine     : string;
 instant    : real;
 vr,vll,vb,vdelta,vdistance,vecart : real;
 valpha : str8;
 n : byte;
 fichier  : text;
begin

date:=copy(FormatDateTime('dd/mm/yyyy hh:mm:ss',Now),1,10);
heure:=copy(FormatDateTime('dd/mm/yyyy hh:mm:ss',Now),12,8);

chaine:=InputBox('Eléments des Orbites', 'Date', date);
date:=chaine;

chaine:=InputBox('Eléments des Orbites', 'Heure', heure);
heure:=chaine;

assign(fichier,'orbites.htm');
rewrite(fichier);

instant:=julien(date,heure);


write(fichier,'<html><head>  <title>El&eacute;ments des Orbites des Plan&egrave;tes</title>');
write(fichier,'</head><body style=''color: rgb(0, 0, 0); background-color: rgb(255, 255, 204)');
write(fichier,' alink=''#000099'' link=''#000099'' vlink=''#990099''>');
write(fichier,'<br> <div style=''text-align: center;''>El&eacute;ments des orbites des plan&egrave;tes le ',date,' &agrave; ',heure,'<br>');
write(fichier,'</div><br><table style=''text-align: left; width: 100%;'' border=''2''  cellpadding=''2'' cellspacing=''2''>');
write(fichier,'<tbody><tr><td></td><td>Mercure</td><td>V&eacute;nus</td><td>Mars</td><td>Jupiter</td><td>Saturne</td><td>Uranus</td>');
write(fichier,'<td>Neptune</td></tr>');

 write(fichier,'<tr><td>Dist.Terre</td>');
 for n:=1 to 7 do
 begin
  orbites(instant,n,vr,vdistance,vll,vb,vdelta,vecart,valpha);
  write(fichier,'<td>',vdistance:9:4,'</td>');
 end;

 write(fichier,'</tr><tr><td>Dist.Soleil</td>');

 for n:=1 to 7 do
  begin
  orbites(instant,n,vr,vdistance,vll,vb,vdelta,vecart,valpha);
  write(fichier,'<td>',vr:9:4,'</td>');
 end;

 write(fichier,'</tr><tr><td>Long.Hélio</td>');
 for n:=1 to 7 do
  begin
   orbites(instant,n,vr,vdistance,vll,vb,vdelta,vecart,valpha);
   write(fichier,'<td>',vll:9:4,'</td>');
 end;

 write(fichier,'</tr><tr><td>Vb</td>');
 for n:=1 to 7 do
  begin
  orbites(instant,n,vr,vdistance,vll,vb,vdelta,vecart,valpha);
  write(fichier,'<td>',vb:9:4,'</td>');
 end;

 write(fichier,'</tr><tr><td>Alpha</td>');
 for n:=1 to 7 do
  begin
    orbites(instant,n,vr,vdistance,vll,vb,vdelta,vecart,valpha);
    write(fichier,'<td>',valpha:8,'</td>');
 end;

 write(fichier,'</tr><tr><td>Delta</td>');
 for n:=1 to 7 do
  begin
   orbites(instant,n,vr,vdistance,vll,vb,vdelta,vecart,valpha);
   write(fichier,'<td>',vdelta:9:4,'</td>');
 end;

 write(fichier,'</tr><tr><td>Ecart</td>');
 for n:=1 to 7 do
  begin
   orbites(instant,n,vr,vdistance,vll,vb,vdelta,vecart,valpha);
   write(fichier,'<td>',vecart:9:4,'</td>');
 end;

 write(fichier,'</Tr></tbody></table></body></html>');

 close(fichier);
MessageDlg('Fin de génération, cliquer sur OK pour voir le résultat',mtInformation, [mbOK], 0);
  OpenDocument('orbites.htm'); { *Converti depuis ShellExecute* }


end;

procedure edit_planete(numplanete:byte;date1,date2:str10;
                       pas,latitude,longitude:real);
var
 heure,valpha,alpha2,lever,coucher,hmeridien : str8;
 jjulien,jjulien2,jjulien_dep,jjulien_arr,
 greenwich,delta2,vr,vll,vb,vdelta,vdistance,
 vecart,vecart2,angsize                              : real;
 n                                           : byte;
 ok					     : boolean;
 fichier				     : text;
begin

heure:='00:00:00';

assign(fichier,'planetes.txt');
rewrite(fichier);

write(fichier,'EPHEMERIDES ASTRONOMIQUES - ');
case numplanete of
 1: write(fichier,'Mercure');
 2: write(fichier,'V‚nus');
 3: write(fichier,'Mars');
 4: write(fichier,'Jupiter');
 5: write(fichier,'Saturne');
 6: write(fichier,'Uranus');
 7: write(fichier,'Neptune');
end;

write(fichier,' - Du ',date1,' au ',date2);

writeln(fichier,'');
writeln(fichier,'Latitude = ',latitude:6:2,' degres   Longitude : ',longitude:7:2,' degres');
writeln(fichier,'________________________________________________________________________________');
writeln(fichier,'                                        Position … 0hUT');
  write(fichier,'   Date      Lever   Meridien Coucher  ');
writeln(fichier,' Asc.Dr ',' ','Declin.',' ','Dist.T','  ','Dist.S.',' Ecart S.');
writeln(fichier,'________________________________________________________________________________');

jjulien_dep:=julien(date1,heure);
jjulien_arr:=julien(date2,heure);

while jjulien_dep<=jjulien_arr do
 begin

  invjulien(jjulien_dep,date1,heure);

  orbites(jjulien_dep,numplanete,vr,vdistance,vll,vb,vdelta,vecart,valpha);

  tsg(jjulien_dep,'00:00:00',greenwich);

  alpha2:=valpha;
  delta2:=vdelta;
  vecart2:=vecart;

  for n:=1 to 2 do
   begin
    levercoucher(true,alpha2,greenwich,latitude,longitude,delta2,0,angsize,lever);
    jjulien2:=julien(date1,lever);
    orbites(jjulien2,numplanete,vr,vdistance,vll,vb,delta2,vecart,alpha2);
 end;

  alpha2:=valpha;
  delta2:=vdelta;

  for n:=1 to 2 do
   begin
    levercoucher(false,alpha2,greenwich,latitude,longitude,delta2,0,angsize,coucher);
    jjulien2:=julien(date1,coucher);
    orbites(jjulien2,numplanete,vr,vdistance,vll,vb,delta2,vecart,alpha2);
 end;

  alpha2:=valpha;
  delta2:=vdelta;

  for n:=1 to 2 do
   begin
    meridien(alpha2,greenwich,longitude,hmeridien);
    jjulien2:=julien(date1,hmeridien);
    orbites(jjulien2,numplanete,vr,vdistance,vll,vb,delta2,vecart,alpha2);
   end;

   write(fichier,date1:10,'  ',lever:8,' ',hmeridien:8,' ',coucher:8,' ');
   write(fichier,valpha:8,' ',vdelta:7:2,' ',vdistance:6:3,'  ',vr:6:3,' ');
   writeln(fichier,vecart2:6:1);


  jjulien_dep:=jjulien_dep+pas;
 end;

 close(fichier);
MessageDlg('Fin de génération, cliquer sur OK pour voir le résultat',mtInformation, [mbOK], 0);
  OpenDocument('planetes.txt'); { *Converti depuis ShellExecute* }

end;

procedure edition_planete;
var
 memodep,memofin                           : str10;
 chaine                                      : string;
 pas                      		     : real;
 choix                                       : integer;
 numplanete                                  : byte;
 erreur                                      : integer;

 begin

memodep:=copy(FormatDateTime('dd/mm/yyyy hh:mm:ss',Now),1,10);

chaine:=InputBox('Ephémérides Planétaires', 'Date début', memodep);
memodep:=chaine;

chaine:=InputBox('Ephémérides Planétaires', 'Date fin', memodep);
memofin:=chaine;

chaine:=InputBox('Ephémérides Planétaires', 'Intervalle en jours', '1');
val(chaine,pas,erreur);

chaine:=InputBox('Ephémérides Planétaires', 'N°Planète Mer=1 Vén=2 Mar=3 Jup=4 Sat=5 Ura=6 Nep=7', '3');
val(chaine,numplanete,erreur);

edit_planete(numplanete,memodep,memofin,pas,global_latitude,global_longitude);

end;

procedure edit_longhelio(date1,date2:str10;pas:real);
var
 heure,alpha : str8;
 jjulien_dep,jjulien_arr,vr,vdistance,vll,vb,vdelta,vecart : real;
 n           : byte;
 ok	     : boolean;
 lst         : text;
begin

heure:='00:00:00';

assign(lst,'helioplanetes.txt');
rewrite(lst);

writeln(lst,'Longitudes Heliocentriques du ',date1,' au ',date2);
writeln(lst,'');
write(lst,'   Date   ',' ','Mercure':8,' ','Venus':8,' ','Mars':8);
writeln(lst,' ','Jupiter':8,' ','Saturne':8,' ','Uranus':8,' ','Neptune':8);
writeln(lst,'');

jjulien_dep:=julien(date1,heure);
jjulien_arr:=julien(date2,heure);

while jjulien_dep<=jjulien_arr do
 begin

  invjulien(jjulien_dep,date1,heure);

  write(lst,date1:10,' ');
  for n:=1 to 7 do
   begin
    orbites(jjulien_dep,n,vr,vdistance,vll,vb,vdelta,vecart,alpha);
    write(lst,vll:8:1,' ');
   end;
  writeln(lst);
  jjulien_dep:=jjulien_dep+pas;
 end;

close(lst);
MessageDlg('Fin de génération, cliquer sur OK pour voir le résultat',mtInformation, [mbOK], 0);
 OpenDocument('helioplanetes.txt'); { *Converti depuis ShellExecute* }

end;

procedure edition_longhelio;
var
 chaine      : string;
 date1,date2 : str10;
 pas         : real;
 erreur      : integer;
begin


date1:=copy(FormatDateTime('dd/mm/yyyy hh:mm:ss',Now),1,10);

chaine:=InputBox('Longitudes Héliocentriques', 'Date début', date1);
date1:=chaine;

chaine:=InputBox('Longitudes Héliocentriques', 'Date fin',date1);
date2:=chaine;

chaine:=InputBox('Longitudes Héliocentriques', 'Intervalle en jours', '1');
val(chaine,pas,erreur);

edit_longhelio(date1,date2,pas);


end;

end.
