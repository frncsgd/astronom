Unit Ast_Moon;

{$MODE Delphi}

Interface

Uses LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,
     AST_FIC,
     AST_GEN,
     AST_SUN;

procedure coordlune(date:str10;heure:str8;var lambda,beta,vpi,delta : real;var alpha:str8);
procedure donphysique(date:str10;heure:str8;var zeta,k : real);
procedure edit_lune(an,mo:word;latitude,longitude:real);
procedure edition_lune;
procedure edit_synthese(an:word);
procedure phases;
procedure pos_lune;
procedure physiques;


implementation
type
 tabstr10 = array[1..5] of str10;
 tabstr8  = array[1..5] of str8;
var
 e,e2,add : real;

procedure additif(puisse:byte;coeff,contenu:real);
begin
 case puisse of
  0: add:=add+coeff*sinus(contenu);
  1: add:=add+e*coeff*sinus(contenu);
  2: add:=add+e2*coeff*sinus(contenu);
 end;
end;

procedure additif2(puisse:byte;coeff,contenu:real);
begin
 case puisse of
  0: add:=add+coeff*cosinus(contenu);
  1: add:=add+e*coeff*cosinus(contenu);
  2: add:=add+e2*coeff*cosinus(contenu);
 end;
end;

procedure calc_lune(jourjulien : real;var lambda,beta,vpi,mprime:real);
var
 t,t2,t3 : real;
 lprime,m,d,f,omega             : real;
 b,omega1,omega2                : real;


begin
 t:=(jourjulien-2415020.0)/36525;
 t2:=sqr(t);
 t3:=t*t2;

 lprime:=270.434164+481267.8831*t-0.001133*t2+0.0000019*t3;
 m:=358.475833+35999.0498*t-0.000150*t2-0.0000033*t3;
 mprime:=296.104608+477198.8491*t+0.009192*t2+0.0000144*t3;
 d:=350.737486+445267.1142*t-0.001436*t2+0.0000019*t3;
 f:=11.250889+483202.0251*t-0.003211*t2-0.0000003*t3;
 omega:=259.183275-1934.1420*t+0.002078*t2+0.00000*t3;

 add:=sinus(51.2+20.2*t);
 lprime:=lprime+0.000233*add;
 m:=m-0.001778*add;
 mprime:=mprime+0.000817*add;
 d:=d+0.002011*add;

 add:=0.003964*sinus(346.56+132.87*t-0.0091731*t2);
 lprime:=lprime+add;
 mprime:=mprime+add;
 d:=d+add;
 f:=f+add;

 add:=sinus(omega);
 lprime:=lprime+0.001964*add;
 mprime:=mprime+0.002541*add;
 d:=d+0.001964*add;
 f:=f-0.024691*add;
 f:=f-0.004328*sinus(omega+275.05-2.3*t);

 e:=1-0.002495*t-0.00000752*t2;
 e2:=sqr(e);

 add:=0;

 additif(0,6.288750,mprime);
 additif(0,1.274018,(2*d-mprime));
 additif(0,0.658309,(2*d));
 additif(0,0.213616,(2*mprime));
 additif(1,-0.185596,m);
 additif(0,-0.114336,(2*f));
 additif(0,0.058793,(2*d-2*mprime));
 additif(1,0.057212,(2*d-m-mprime));
 additif(0,0.053320,(2*d+mprime));
 additif(1,0.045874,(2*d-m));
 additif(1,0.041024,(mprime-m));
 additif(0,-0.034718,(d));
 additif(1,-0.030465,(m+mprime));
 additif(0,0.015326,(2*d-2*f));
 additif(0,-0.012528,(2*f+mprime));
 additif(0,-0.010980,(2*f-mprime));
 additif(0,0.010674,(4*d-mprime));
 additif(0,0.010034,(3*mprime));
 additif(0,0.008548,(4*d-2*mprime));
 additif(1,-0.007910,(m-mprime+2*d));
 additif(1,-0.006783,(2*d+m));
 additif(0,0.005162,(mprime-d));
 additif(1,0.005000,(mprime+d));
 additif(1,0.004049,(mprime-m+2*d));
 additif(0,0.003996,(2*mprime+2*d));
 additif(0,0.003862,(4*d));
 additif(0,0.003665,(2*d-3*mprime));
 additif(1,0.002695,(2*mprime-m));
 additif(0,0.002602,(mprime-2*f-2*d));
 additif(1,0.002396,(2*d-m-2*mprime));
 additif(0,-0.002349,(mprime+d));
 additif(2,0.002249,(2*d-2*m));
 additif(1,-0.002125,(2*mprime+m));
 additif(2,-0.002079,(2*m));
 additif(2,0.002059,(2*d-mprime-2*m));
 additif(0,-0.001773,(mprime+2*d-2*f));
 additif(0,-0.001595,(2*f+2*d));
 additif(1,0.001220,(4*d-m-mprime));
 additif(0,-0.001110,(2*mprime+2*f));
 additif(0,0.000892,(mprime-3*d));
 additif(1,-0.000811,(m+mprime+2*d));
 additif(1,0.000761,(4*d-m-2*mprime));
 additif(2,0.000717,(mprime-2*m));
 additif(2,0.000704,(mprime-2*m-2*d));
 additif(1,0.000693,(m-2*mprime+2*d));
 additif(1,0.000598,(2*d-m-2*f));
 additif(0,0.000550,(mprime+4*d));
 additif(0,0.000538,(4*mprime));
 additif(1,0.000521,(4*d-m));
 additif(0,0.000486,(2*mprime-d));

 lambda:=lprime+add;
 range0a360(lambda,lambda);

 add:=0;

 additif(0,5.128189,(f));
 additif(0,0.280606,(mprime+f));
 additif(0,0.277693,(mprime-f));
 additif(0,0.173238,(2*d-f));
 additif(0,0.055413,(2*d+f-mprime));
 additif(0,0.046272,(2*d-f-mprime));
 additif(0,0.032573,(2*d+f));
 additif(0,0.017198,(2*mprime+f));
 additif(0,0.009267,(2*d+mprime-f));
 additif(0,0.008823,(2*mprime-f));
 additif(1,0.008247,(2*d-m-f));
 additif(0,0.004323,(2*d-f-2*mprime));
 additif(0,0.004200,(2*d+f+2*mprime));
 additif(1,0.003372,(f-m-2*d));
 additif(1,0.002472,(2*d+f-m-mprime));
 additif(1,0.002222,(2*d+f-m));
 additif(1,0.002072,(2*d-f-m-mprime));
 additif(1,0.001877,(f-m+mprime));
 additif(0,0.001828,(4*d-f-mprime));
 additif(1,-0.001803,(f+m));
 additif(0,-0.001750,(3*f));
 additif(1,0.001570,(mprime-m-f));
 additif(0,-0.001487,(f+d));
 additif(1,-0.001481,(f+m+mprime));
 additif(1,0.001417,(f-m-mprime));
 additif(1,0.001350,(f-m));
 additif(0,0.001330,(f-d));
 additif(0,0.001106,(f+3*mprime));
 additif(0,0.001020,(4*d-f));
 additif(0,0.000833,(f+4*d-mprime));
 additif(0,0.000781,(mprime-3*f));
 additif(0,0.000670,(f+4*d-2*mprime));
 additif(0,0.000606,(2*d-3*f));
 additif(0,0.000597,(2*d+2*mprime-f));
 additif(1,0.000492,(2*d+mprime-m-f));
 additif(0,0.000450,(2*mprime-f-2*d));
 additif(0,0.000439,(3*mprime-f));
 additif(0,0.000423,(f+2*d+2*mprime));
 additif(0,0.000422,(2*d-f-3*mprime));
 additif(1,-0.000367,(m+f+2*d-mprime));
 additif(1,-0.000353,(m+f+2*d));
 additif(0,0.000331,(f+4*d));
 additif(1,0.000317,(2*d+f-m+mprime));
 additif(2,0.000306,(2*d-2*m-f));
 additif(0,-0.000283,(mprime+3*f));

 b:=add;

 omega1:=0.0004664*cosinus(omega);
 omega2:=0.0000754*cosinus(omega+275.05-2.3*t);
 beta:=b*(1-omega1-omega2);

 range0a360(beta,beta);

 add:=0;

 vpi:=0.950724;

 additif2(0,0.051818,(mprime));
 additif2(0,0.009531,(2*d-mprime));
 additif2(0,0.007843,(2*d));
 additif2(0,0.002824,(2*mprime));
 additif2(0,0.000857,(2*d+mprime));
 additif2(1,0.000533,(2*d-m));
 additif2(1,0.000401,(2*d-m-mprime));
 additif2(1,0.000320,(mprime-m));
 additif2(0,-0.000271,(d));
 additif2(1,-0.000264,(m+mprime));
 additif2(0,-0.000198,(2*f-mprime));
 additif2(0,0.000173,(3*mprime));
 additif2(0,0.000167,(4*d-mprime));
 additif2(1,-0.000111,(m));
 additif2(0,0.000103,(4*d-2*mprime));
 additif2(0,-0.000084,(2*mprime-2*d));
 additif2(1,-0.000083,(2*d+m));
 additif2(0,0.000079,(2*d+2*mprime));
 additif2(0,0.000072,(4*d));
 additif2(1,0.000064,(2*d-m+mprime));
 additif2(1,-0.000063,(2*d+m-mprime));
 additif2(1,0.000041,(m+d));
 additif2(1,0.000035,(2*mprime-m));
 additif2(0,-0.000033,(3*mprime-2*d));
 additif2(0,-0.000030,(mprime+d));
 additif2(0,-0.000029,(2*f-2*d));
 additif2(1,-0.000029,(2*mprime+m));
 additif2(2,0.000026,(2*d-2*m));
 additif2(0,-0.000023,(2*f-2*d+mprime));
 additif2(1,0.000019,(4*d-m-mprime));

 vpi:=vpi+add;

 range0a360(vpi,vpi);

 end;

procedure coordlune(date:str10;heure:str8;var lambda,beta,vpi,delta : real;var alpha:str8);
var
 t         : real;
 alpha_sun : str8;
 delta_sun,rayon_sun,longitude_sun,epsilon,mprime,m,bidon,parallaxe,angsize : real;
begin
 t:=julien(date,heure);
 calc_lune(t,lambda,beta,vpi,mprime);
 calc_soleil(t,alpha_sun,delta_sun,rayon_sun,longitude_sun,bidon,epsilon,m,parallaxe,angsize);
 par_27(lambda,beta,epsilon,alpha,delta);
end;

procedure pos_lune;
var
 heure,alpha,par  : str8;
 date             : str10;
 lambda,vpi,beta  : real;
 chaine           : string;
 delta,jlunaire   : real;
 fichier          : text;
begin

date:=copy(FormatDateTime('dd/mm/yyyy hh:mm:ss',Now),1,10);
heure:=copy(FormatDateTime('dd/mm/yyyy hh:mm:ss',Now),12,8);

chaine:=InputBox('Position de la Lune', 'Date', date);
date:=chaine;

chaine:=InputBox('Position de la Lune', 'Heure', heure);
heure:=chaine;

coordlune(date,heure,lambda,beta,vpi,delta,alpha);


assign(fichier,'journlune.txt');
rewrite(fichier);

writeln(fichier,'Lambda = ',lambda:10:3);
writeln(fichier,'Beta   = ',beta:10:3);
heuredecmin(vpi,par);
writeln(fichier,'Parallaxe = ',par:10);
writeln(fichier,'Alpha  = ',alpha:8);
writeln(fichier,'Delta  = ',delta:6:2);
close(fichier);

MessageDlg('Fin de génération, cliquer sur OK pour voir le résultat',mtInformation, [mbOK], 0);
 OpenDocument('journlune.txt'); { *Converti depuis ShellExecute* }


end;

procedure calc_phases(mo,an:word;var date : tabstr10;var heure : tabstr8;var premphase : byte);
var
 t,jj,annee       : real;
 t2,t3,m,mprime,f : real;
 k                : real;
 kp               : array[1..5] of real;
 quotient         : longint;
 reste,n,jo       : integer;
 vdate            : str10;
 vheure           : str8;

begin

case mo of
 1 : jo:=0;
 2 : jo:=31;
 3 : jo:=59;
 4 : jo:=90;
 5 : jo:=120;
 6 : jo:=151;
 7 : jo:=181;
 8 : jo:=212;
 9 : jo:=243;
 10: jo:=273;
 11: jo:=304;
 12: jo:=334;
end;

if (an mod 4)<>0 then annee:=an+jo/365 else
 begin
  if mo<=2 then annee:=an+(jo)/366 else annee:=an+(jo+1)/366;
 end;


k:=100*(annee-1900)*12.3685;
quotient:=round(k) div 25;
reste:=round(k) mod 25;
if reste>0 then reste:=25;
k:=25*quotient+reste;
premphase:=0;

for n:=1 to 4 do
 begin
  if (round(k) mod 100)=0 then
   begin
    kp[1]:=k/100;
    if premphase=0 then premphase:=1;
   end;
  if (round(k) mod 100)=25 then
   begin
    kp[2]:=k/100;
    if premphase=0 then premphase:=2;
  end;
  if (round(k) mod 100)=50 then
   begin
    kp[3]:=k/100;
    if premphase=0 then premphase:=3;
   end;
  if (round(k) mod 100)=75 then
   begin
    kp[4]:=k/100;
    if premphase=0 then premphase:=4;
  end;
  k:=k+25;
end;

kp[5]:=k/100;

for n:=1 to 5 do
 begin
  t:=kp[n]/1236.85;
  t2:=sqr(t);
  t3:=t*t2;
  jj:=2415020.75933+29.53058868*kp[n]+0.0001178*t2-0.000000155*t3
                 +0.00033*sinus(166.56+132.87*t-0.009173*t2);

  m:=359.2242+29.10535608*kp[n]-0.0000333*t2-0.00000347*t3;
  mprime:=306.0253+385.81691806*kp[n]+0.0107306*t2+0.00001236*t3;
  f:=21.2964+390.67050646*kp[n]-0.0016528*t2-0.00000239*t3;

  add:=0;
  if (n=1) or (n=3) or (((premphase=1) or (premphase=3)) and (n=5)) then
   begin
    jj:=jj+(0.1734-0.000393*t)*sinus(m);
    additif(0,0.0021,(2*m));
    additif(0,-0.4068,(mprime));
    additif(0,0.0161,(2*mprime));
    additif(0,-0.0004,(3*mprime));
    additif(0,0.0104,(2*f));
    additif(0,-0.0051,(m+mprime));
    additif(0,-0.0074,(m-mprime));
    additif(0,0.0004,(2*f+m));
    additif(0,-0.0004,(2*f-m));
    additif(0,-0.0006,(2*f+mprime));
    additif(0,0.0010,(2*f-mprime));
    additif(0,0.0005,(m+2*mprime));
    jj:=jj+add;
   end;
  if (n=2) or (n=4) or (((premphase=2) or (premphase=4)) and (n=5)) then
   begin
    jj:=jj+(0.1721-0.0004*t)*sinus(m);
    additif(0,0.0021,(2*m));
    additif(0,-0.6280,(mprime));
    additif(0,0.0089,(2*mprime));
    additif(0,-0.0004,(3*mprime));
    additif(0,0.0079,(2*f));
    additif(0,-0.0119,(m+mprime));
    additif(0,-0.0047,(m-mprime));
    additif(0,0.0003,(2*f+m));
    additif(0,-0.0004,(2*f-m));
    additif(0,-0.0006,(2*f+mprime));
    additif(0,0.0021,(2*f-mprime));
    additif(0,0.0003,(m+2*mprime));
    additif(0,0.0004,(m-2*mprime));
    additif(0,-0.0003,(2*m+mprime));
    jj:=jj+add;
   end;

   if (n=2) or ((premphase=2) and (n=5))
         then jj:=jj+0.0028-0.0004*cosinus(m)+0.0003*cosinus(mprime);
   if (n=4) or ((premphase=4) and (n=5))
         then jj:=jj-0.0028+0.0004*cosinus(m)-0.0003*cosinus(mprime);

   invjulien(jj,vdate,vheure);
  date[n]:=vdate;
  heure[n]:=vheure;

  end; (* for n:=1 to 4...*)
end;

procedure edit_lune(an,mo:word;latitude,longitude:real);
var
 chaine             : string;
 n                  : integer;
 an0,mo0,jo         : word;
 jour,datedep       : str10;
 alpha,alpha2,lever,coucher,hmeridien,par       : str8;
 jjulien,greenwich,delta,delta2,vpi,lambda,beta : real;
 angsize                                        : real;
 fin,ok                                    : boolean;
 nomdumois                              : str10;
 vpi2,jourlune                          : real;
 jlunaire                               : string[2];
 date                                   : tabstr10;
 heure                                  : tabstr8;
 premphase,dernphase                    : byte;
 lst                                    : text;
 nomfichier                              : RawByteString;

begin

jo:=1;

fin:=false;

case mo of
 1 : nomdumois:='JANVIER';
 2 : nomdumois:='FEVRIER';
 3 : nomdumois:='MARS';
 4 : nomdumois:='AVRIL';
 5 : nomdumois:='MAI';
 6 : nomdumois:='JUIN';
 7 : nomdumois:='JUILLET';
 8 : nomdumois:='AOUT';
 9 : nomdumois:='SEPTEMBRE';
 10: nomdumois:='OCTOBRE';
 11: nomdumois:='NOVEMBRE';
 12: nomdumois:='DECEMBRE';
end;

mo0:=mo-1;
an0:=an;
if mo0=0 then
 begin
  mo0:=12;
  an0:=an-1;
 end;

calc_phases(mo0,an0,date,heure,premphase);
datedep:=date[1];
if (copy(date[1],4,2)=copy(date[5],4,2)) and (premphase=1) then datedep:=date[5];
  jour:='01/';
  str(mo:2,chaine);
  jour:=jour+chaine+'/';
  str(an:4,chaine);
  jour:=jour+chaine;
jourlune:=julien(jour,'00-00-0000')-julien(datedep,'00:00:00');

calc_phases(mo,an,date,heure,premphase);

str(mo,chaine);
nomfichier:='ephemLune'+chaine+'.txt';

assign(lst,nomfichier);
rewrite(lst);

writeln(lst,'EPHEMERIDES ASTRONOMIQUES - LUNE - ',nomdumois,' ',an:4);

writeln(lst,'');
writeln(lst,'Latitude = ',latitude:6:2,' degres   Longitude : ',longitude:7:2,' degres');
writeln(lst,'__________________________________________________________________________');
writeln(lst,'                       Position … 0hUT');
writeln(lst,'   JOUR       JOUR     Ascensio  Decli  Paral            Heures UT');
writeln(lst,'             LUNAIRE   Droite    naiso  -laxe     Lever   Meridien Coucher');
writeln(lst,'__________________________________________________________________________');
writeln(lst,'');

while not fin do
 begin



  str(jo:2,chaine);
  jour:=chaine+'/';
  str(mo:2,chaine);
  jour:=jour+chaine+'/';
  str(an:4,chaine);
  jour:=jour+chaine;
  jjulien:=julien(jour,'12:00:00');

  write(lst,jour:10,' ');

  jjulien:=julien(jour,'00:00:00');
  tsg(jjulien,'00:00:00',greenwich);

  coordlune(jour,'00:00:00',lambda,beta,vpi,delta,alpha);

  jlunaire:='';
  if jourlune>=1 then
   begin
    jourlune:=jourlune+1;
    str(jourlune:2:0,jlunaire);
   end;

  for n:=1 to 5 do
   begin
    if jour=date[n] then
     begin
      if (n=1) or ((n=5) and (premphase=1)) then
       begin
        jlunaire:='NL';
        jourlune:=1;
       end;
      if (n=2) or ((n=5) and (premphase=2)) then jlunaire:='PQ';
      if (n=3) or ((n=5) and (premphase=3)) then jlunaire:='PL';
      if (n=4) or ((n=5) and (premphase=4)) then jlunaire:='DQ';
     end;
   end;


  write(lst,'    ',jlunaire:2,'     ');

  heuredecmin(vpi,par);
  write(lst,alpha:8,' ',delta:7:2,' ',par:8,'  ');

  alpha2:=alpha;
  delta2:=delta;
  vpi2:=vpi;

  for n:=1 to 10 do
   begin
    levercoucher(true,alpha2,greenwich,latitude,longitude,delta2,vpi2,angsize,lever);
    coordlune(jour,lever,lambda,beta,vpi2,delta2,alpha2);
   end;

  alpha2:=alpha;
  delta2:=delta;
  vpi2:=vpi;

  for n:=1 to 10 do
   begin
    levercoucher(false,alpha2,greenwich,latitude,longitude,delta2,vpi2,angsize,coucher);
    coordlune(jour,coucher,lambda,beta,vpi2,delta2,alpha2);
   end;

  alpha2:=alpha;
  delta2:=delta;

  for n:=1 to 10 do
   begin
    meridien(alpha2,greenwich,longitude,hmeridien);
    coordlune(jour,hmeridien,lambda,beta,vpi,delta2,alpha2);
   end;


  alpha2:=alpha;
  delta2:=delta;

  for n:=1 to 10 do
   begin
    meridien(alpha2,greenwich,longitude,hmeridien);
    coordlune(jour,hmeridien,lambda,beta,vpi,delta2,alpha2);
   end;

  write(lst,lever:8,' ',hmeridien:8,' ',coucher:8);
  writeln(lst,'');

  inc(jo);

  if (jo=32) and ((mo=1) or (mo=3) or (mo=5) or (mo=7)or (mo=8)
                         or (mo=10) or (mo=12)) then fin:=true;
  if (jo=31) and ((mo=4) or (mo=6) or (mo=9) or (mo=11)) then fin:=true;
  if (jo=30) and ((an mod 4)=0) and (mo=2) then fin:=true;
  if (jo=29) and ((an mod 4)<>0) and (mo=2) then fin:=true;

  end; (* while not fin....*)

writeln(lst,'_______________________________________________________________________');

dernphase:=premphase;
  fin:=false;

  while not fin do
   begin
    case premphase of
     1 : writeln(lst,'Nouvelle Lune    : ',date[1]:10,' ',heure[1]:8);
     2 : writeln(lst,'Premier Quartier : ',date[2]:10,' ',heure[2]:8);
     3 : writeln(lst,'Pleine   Lune    : ',date[3]:10,' ',heure[3]:8);
     4 : writeln(lst,'Dernier Quartier : ',date[4]:10,' ',heure[4]:8);
    end;
    premphase:=premphase+1;
    if premphase=5 then premphase:=1;
    if premphase=dernphase then fin:=true;
    inc(n);
   end;
   if copy(date[1],4,2)=copy(date[5],4,2) then
    begin
     case premphase of
      1 : writeln(lst,'Nouvelle Lune    : ',date[5]:10,' ',heure[5]:8);
      2 : writeln(lst,'Premier Quartier : ',date[5]:10,' ',heure[5]:8);
      3 : writeln(lst,'Pleine   Lune    : ',date[5]:10,' ',heure[5]:8);
      4 : writeln(lst,'Dernier Quartier : ',date[5]:10,' ',heure[5]:8);
    end;
   end;

writeln(lst,#12);
close(lst);

MessageDlg('Fin de génération, cliquer sur OK pour voir le résultat',mtInformation, [mbOK], 0);
 OpenDocument(nomfichier); { *Converti depuis ShellExecute* }


end;


procedure edition_lune;
var
 inputstring : string;
 mois        : integer;
 annee       : integer;
 erreur      : integer;
 annee_st    : str10;
 mois_st     : string[2];
begin

 annee_st:=copy(FormatDateTime('dd/mm/yyyy hh:mm:ss',Now),7,4);
 mois_st:=copy(FormatDateTime('dd/mm/yyyy hh:mm:ss',Now),4,2);

 InputString:= InputBox('Ephémérides mensuelles Lune', 'Saisir année', annee_st);
 val(inputstring,annee,erreur);
 InputString:= InputBox('Ephémérides mensuelles Lune ', 'Saisir mois (0 si année complète)', mois_st);
 val(inputstring,mois,erreur);

if mois<>0 then
 begin
edit_lune(annee,mois,global_latitude,global_longitude);
end
else
begin
  mois:=1;
  while mois<=12 do
begin
 edit_lune(annee,mois,global_latitude,global_longitude);
 mois:=mois+1;
 end;

 end;

end;

procedure phases;
var
 date                  : tabstr10;
 heure                 : tabstr8;
 an,mo,jo,jsem         : word;
 premphase,dernphase,n : byte;
 fin                   : boolean;
 InputString           : string;
 fichier               : text;
 erreur                : integer;
 annee                 : str10;
 mois                  : string[2];
begin

 annee:=copy(FormatDateTime('dd/mm/yyyy hh:mm:ss',Now),7,4);
 mois:=copy(FormatDateTime('dd/mm/yyyy hh:mm:ss',Now),4,2);
 assign(fichier,'ephemphases.txt');
 rewrite(fichier);

 InputString:= InputBox('Phases Lune','Saisir année',annee);
 val(inputstring,an,erreur);

 mo:=1;

 while mo<=12 do
  begin

  calc_phases(mo,an,date,heure,premphase);

  dernphase:=premphase;
  fin:=false;
  n:=1;



  while not fin do
   begin
    case premphase of
     1 : writeln(fichier,'Nouvelle Lune    : ',date[1]:10,' ',heure[1]:8);
     2 : writeln(fichier,'Premier Quartier : ',date[2]:10,' ',heure[2]:8);
     3 : writeln(fichier,'Pleine   Lune    : ',date[3]:10,' ',heure[3]:8);
     4 : writeln(fichier,'Dernier Quartier : ',date[4]:10,' ',heure[4]:8);
    end;
    premphase:=premphase+1;
    if premphase=5 then premphase:=1;
    if premphase=dernphase then fin:=true;
    inc(n);
   end;
   if copy(date[1],4,2)=copy(date[5],4,2) then
    begin

     case premphase of
      1 : writeln(fichier,'Nouvelle Lune    : ',date[5]:10,' ',heure[5]:8);
      2 : writeln(fichier,'Premier Quartier : ',date[5]:10,' ',heure[5]:8);
      3 : writeln(fichier,'Pleine   Lune    : ',date[5]:10,' ',heure[5]:8);
      4 : writeln(fichier,'Dernier Quartier : ',date[5]:10,' ',heure[5]:8);
    end;
   end;

   mo:=mo+1;
   writeln(fichier,'');
  end;

   close(fichier);
   MessageDlg('Fin de génération, cliquer sur OK pour voir le résultat',mtInformation, [mbOK], 0);
    OpenDocument('ephemphases.txt'); { *Converti depuis ShellExecute* }

 
end;

procedure edit_synthese(an:word);
var
 mo                  : word;
 nomdumois           : str10;
 premphase,dernphase : byte;
 date                : tabstr10;
 heure               : tabstr8;
 fin,ok              : boolean;
 n                   : integer;
 lst                 : text;
 
begin

assign(lst,'synthlune.txt');
rewrite(lst);
writeln(lst,'LES PHASES DE LA LUNE - ANNEE ',an:4);

mo:=1;
while mo<>13 do
 begin

  case mo of
 1 : nomdumois:='JANVIER';
 2 : nomdumois:='FEVRIER';
 3 : nomdumois:='MARS';
 4 : nomdumois:='AVRIL';
 5 : nomdumois:='MAI';
 6 : nomdumois:='JUIN';
 7 : nomdumois:='JUILLET';
 8 : nomdumois:='AOUT';
 9 : nomdumois:='SEPTEMBRE';
 10: nomdumois:='OCTOBRE';
 11: nomdumois:='NOVEMBRE';
 12: nomdumois:='DECEMBRE';
end;


  calc_phases(mo,an,date,heure,premphase);
  dernphase:=premphase;
  fin:=false;
  n:=1;

  writeln(lst,'');
  writeln(lst,nomdumois,' : ');

  while not fin do
   begin
    case premphase of
     1 : writeln(lst,'Nouvelle Lune    : ',date[1]:10,' ',heure[1]:8);
     2 : writeln(lst,'Premier Quartier : ',date[2]:10,' ',heure[2]:8);
     3 : writeln(lst,'Pleine   Lune    : ',date[3]:10,' ',heure[3]:8);
     4 : writeln(lst,'Dernier Quartier : ',date[4]:10,' ',heure[4]:8);
    end;
    premphase:=premphase+1;
    if premphase=5 then premphase:=1;
    if premphase=dernphase then fin:=true;
    inc(n);
   end;
   if copy(date[1],4,2)=copy(date[5],4,2) then
    begin
     case premphase of
      1 : writeln(lst,'Nouvelle Lune    : ',date[5]:10,' ',heure[5]:8);
      2 : writeln(lst,'Premier Quartier : ',date[5]:10,' ',heure[5]:8);
      3 : writeln(lst,'Pleine   Lune    : ',date[5]:10,' ',heure[5]:8);
      4 : writeln(lst,'Dernier Quartier : ',date[5]:10,' ',heure[5]:8);
    end;
   end; (*if copy ...*)
   inc(mo);
  end;

  close(lst);
 MessageDlg('Fin de génération, cliquer sur OK pour voir le résultat',mtInformation, [mbOK], 0);
   OpenDocument('synthlune.txt'); { *Converti depuis ShellExecute* }

end;

procedure donphysique(date:str10;heure:str8;var zeta,k : real);
var
 jourjulien : real;
 lambda,beta,vpi,mprime,d,i,delta,ar1,ar2,x,y : real;
 alphas,alpha               : str8;
 deltas,rayon,longvraie,epsilon,m,bidon,parallaxe,angsize : real;
begin
jourjulien:=julien(date,heure);
calc_lune(jourjulien,lambda,beta,vpi,mprime);
calc_soleil(jourjulien,alphas,deltas,rayon,longvraie,bidon,epsilon,m,parallaxe,angsize);

d:=cosinus(lambda-longvraie)*cosinus(beta);
d:=arccosinus(d);
i:=180-d-0.1468*(1-0.0549*sinus(mprime))/(1-0.0167*sinus(m))*sinus(d);
k:=(1+cosinus(i))/2;

coordlune(date,heure,lambda,beta,vpi,delta,alpha);

heuremindec(alpha,ar1);
ar1:=ar1*15;
heuremindec(alphas,ar2);
ar2:=ar2*15;

y:=cosinus(deltas)*sinus(ar2-ar1);
x:=cosinus(delta)*sinus(deltas)-sinus(delta)*cosinus(deltas)*cosinus(ar2-ar1);
corriquad(x,y,zeta);

end;

procedure physiques;
var
 chaine : string;
 chaine2 : string;
 date   : str10;
 heure  : str8;
 k,zeta : real;
begin

date:=copy(FormatDateTime('dd/mm/yyyy hh:mm:ss',Now),1,10);
heure:=copy(FormatDateTime('dd/mm/yyyy hh:mm:ss',Now),12,8);

chaine:=InputBox('Données Physiques de la Lune', 'Date', date);
date:=chaine;

chaine:=InputBox('Données Physiques de la Lune', 'Heure', heure);
heure:=chaine;

donphysique(date,heure,zeta,k);
str(k:6:2,chaine);
chaine:='k='+chaine;
str(zeta:6:2,chaine2);
chaine2:='Zeta='+chaine2;
MessageDlg(chaine + chr(13) +chaine2,mtinformation, [mbOK], 0);


end;

end.
