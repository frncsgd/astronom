Unit AST_SUN;

{$MODE Delphi}

Interface

(* ajout ast_dive le 06/02/2021 pour usage fonction twilight heure nuit astronomique*)

Uses LCLIntf, LCLType,SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,AST_FIC,AST_GEN;


procedure calc_soleil(jourjulien : real;var alpha:str8;var delta:real;var rayon:real;
                              var longvraie,anomvraie :real;var epsilon : real;var m : real;
                              var parallaxe,angsize:real);
procedure edit_soleil(an,mo:word;latitude,longitude:real);
procedure edit_pbl(date1,date2:str10;pas:real);
procedure edition_pbl;
procedure edit_crn(annee:word);
procedure edit_parallaxe(annee,pas:word);
procedure chap15(date:str10;heure:str8;var alpha:str8;var delta:real;var rayon:real;
                                       var p,b,l,parallaxe,angsize : real);
function numero_crn(date:str10;heure:str8):real;


implementation

var fichier : text;

procedure calc_soleil(jourjulien : real;var alpha:str8;var delta:real;var rayon:real;
                              var longvraie,anomvraie :real;var epsilon : real;var m : real;
                              var parallaxe,angsize:real);
var
 t,l,e,c                           : real;
 omega,longitude                   : real;
 pa,pb,pc,pd,pe,ph,x,y             : real;

begin
 t:=(jourjulien-2415020.0)/36525;

 l:=279.69668+36000.76892*t+0.0003025*sqr(t);
 range0a360(l,l);

 m:=358.47583+35999.04975*t-0.000150*sqr(t)-0.0000033*sqr(t)*t;
 range0a360(m,m);

 e:=0.01675104-0.0000418*t-0.000000126*sqr(t);
 range0a360(e,e);

 c:=(1.919460-0.004789*t-0.000014*sqr(t))*sinus(m);
 c:=c+(0.020094-0.000100*t)*sinus(2*m);
 c:=c+0.000293*sinus(3*m);

 l:=l+c;          (*longitude vraie*)
 anomvraie:=m+c;
 rayon:=1.0000002*(1-sqr(e))/(1+e*cosinus(anomvraie));

 pa:=153.23+22518.7541*t;
 pb:=216.57+45037.5082*t;
 pc:=312.69+32964.3577*t;
 pd:=350.74+445267.1142*t-0.00144*sqr(t);
 pe:=231.19+20.2*t;
 ph:=353.4+65928.7155*t;

 l:=l+0.00134*cosinus(pa);
 l:=l+0.00154*cosinus(pb);
 l:=l+0.00200*cosinus(pc);
 l:=l+0.00179*sinus(pd);
 l:=l+0.00178*sinus(pe);

 rayon:=rayon+0.00000543*sinus(pa);
 rayon:=rayon+0.00001575*sinus(pb);
 rayon:=rayon+0.00001627*sinus(pc);
 rayon:=rayon+0.00003076*cosinus(pd);
 rayon:=rayon+0.00000927*sinus(pe);

 epsilon:=23.452294-0.0130125*t;
 epsilon:=epsilon-0.00000164*sqr(t);
 epsilon:=epsilon+0.000000503*sqr(t)*t;

 omega:=259.18-1934.142*t;
 range0a360(omega,omega);

 epsilon:=epsilon+0.00256*cosinus(omega);

 longitude:=l-0.00569-0.00479*sinus(omega);
 range0a360(longitude,longitude);

 par_27(longitude,0,epsilon,alpha,delta);

 longvraie:=l;

 parallaxe:=arcsinus(sinus(2.44277E-3)/rayon);
 y:=695500;
 x:=rayon*unite_astronomique;
 corriquad(x,y,angsize);
 angsize:=angsize*2;
 end;

procedure angles_pbl(jjulien:real;var p,b0,l0 : real);
var
 t,delta,omega,bidon2,anomvraie,epsilon,lambda0,m,x,y,a,zeta1,zeta2 : real;
 bidon                          : str8;
begin
 t:=(jjulien-2415020)/36525;
 delta:=(84*t)/60;
 omega:=74.3666666+delta;
 calc_soleil(jjulien,bidon,bidon2,bidon2,bidon2,anomvraie,epsilon,bidon2,bidon2,bidon2);
 lambda0:=anomvraie+282.596403;
 range0a360(lambda0,lambda0);
 y:=sinus(omega-lambda0)*cosinus(7.25);
 x:=-cosinus(omega-lambda0);
 corriquad(x,y,a);
 m:=360/25.38*(jjulien-2398220);
 range0a360(m,m);
 m:=360-m;
 l0:=a+m;
 range0a360(l0,l0);
 b0:=arcsinus(sinus(omega-lambda0)*sinus(7.25));
 b0:=-b0;
 zeta1:=180/pi*arctan(-cosinus(lambda0)*tangente(epsilon));
 zeta2:=180/pi*arctan(-cosinus(omega-lambda0)*tangente(7.25));
 p:=zeta1+zeta2;
end;

procedure chap15(date:str10;heure:str8;var alpha:str8;var delta:real;var rayon:real;
                                       var p,b,l,parallaxe,angsize : real);
var
 t         : real;
 longvraie : real;
 m         : real;

begin
 t:=julien(date,heure);
 calc_soleil(t,alpha,delta,rayon,longvraie,longvraie,longvraie,m,parallaxe,angsize);
 angles_pbl(t,p,b,l);

end;

function numero_crn(date:str10;heure:str8):real;
var
 jjulien,num_crn : real;
begin
 jjulien:=julien(date,heure);
 num_crn:=1690+(jjulien-2444235.34)/27.2753;
 numero_crn:=num_crn;
end;


procedure edit_soleil(an,mo:word;latitude,longitude:real);
var
 n                : integer;
 chaine           : string;
 jo               : word;
 jour             : str10;
 alpha,alpha2,lever,coucher,hmeridien,heuregreenwich : str8;
 bidon,jjulien,greenwich,delta,delta2,rayon,angsize          : real;
 fin                                     : boolean;
 nomdumois                               : str10;
 nomfichier                              : RawByteString;
 alpha_coucher,alpha_lever                          : str8;
  hleversol2 : real;
 hcouchersol2 : real;
 twilight2	: real;
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

str(mo,chaine);
nomfichier:='ephemSol'+chaine+'.txt';

assign(fichier,nomfichier);
rewrite(fichier);

writeln(fichier,'EPHEMERIDES ASTRONOMIQUES - SOLEIL - ',nomdumois,' ',an:4);

writeln(fichier,'');
writeln(fichier,'Latitude = ',latitude:6:2,' degres   Longitude : ',longitude:7:2,' degres');
writeln(fichier,'___________________________________________________________________________');
writeln(fichier,'                               Position … 12hUT');
writeln(fichier,'                Date    Temps  Ascensio  Decli');
writeln(fichier,'   JOUR       Julienne Sideral  Droite  naison         Heures en UT');
writeln(fichier,'              … 12h UT Greenwich                  Lever  Meridien Coucher');
writeln(fichier,'                       … 0h UT');
writeln(fichier,'___________________________________________________________________________');
writeln(fichier,'');

while not fin do
 begin
  str(jo:2,chaine);
  jour:=chaine+'/';
  str(mo:2,chaine);
  jour:=jour+chaine+'/';
  str(an:4,chaine);
  jour:=jour+chaine;
  jjulien:=julien(jour,'12:00:00');

  write(fichier,jour:10,' ',jjulien:10:0,' ');

  jjulien:=julien(jour,'00:00:00');
  tsg(jjulien,'00:00:00',greenwich);

  heuredecmin(greenwich,heuregreenwich);
  write(fichier,heuregreenwich:8,' ');

  chap15(jour,'12:00:00',alpha,delta,rayon,bidon,bidon,bidon,bidon,angsize);

  write(fichier,alpha:8,' ',delta:6:2,' ');

  alpha2:=alpha;
  delta2:=delta;
  (*Déterminer l'heure de lever*)
  for n:=1 to 10 do
   begin
    levercoucher(true,alpha2,greenwich,latitude,longitude,delta2,0,angsize,lever);
    chap15(jour,lever,alpha2,delta2,rayon,bidon,bidon,bidon,bidon,angsize);
   end;

  alpha2:=alpha;
  delta2:=delta;

  (*Déterminer l'heure de passage au méridien, est indépendant de la latitude*)
  for n:=1 to 10 do
   begin
    meridien(alpha2,greenwich,longitude,hmeridien);
    chap15(jour,hmeridien,alpha2,delta2,rayon,bidon,bidon,bidon,bidon,bidon);
   end;

  (*Déterminer l'heure de coucher*)
  for n:=1 to 10 do
   begin
    levercoucher(false,alpha2,greenwich,latitude,longitude,delta2,0,-angsize,coucher);  (*Mis -angsize en paramètres le 17/01/2021 ->Plus proche de réel ! *)
    chap15(jour,coucher,alpha2,delta2,rayon,bidon,bidon,bidon,bidon,angsize);
   end;


  write(fichier,lever:8,' ',hmeridien:8,' ',coucher:8);

  (*Déterminer l'heure de nuit astronomique
   heuremindec(lever,hleversol2);
   heuremindec(coucher,hcouchersol2);
   heuremindec(twilight(delta,global_latitude),twilight2);

   hleversol2:=hleversol2-twilight2;
   hcouchersol2:=hcouchersol2+twilight2;   *)

   writeln(fichier,'');

  inc(jo);

  if (jo=32) and ((mo=1) or (mo=3) or (mo=5) or (mo=7)or (mo=8)
                         or (mo=10) or (mo=12)) then fin:=true;
  if (jo=31) and ((mo=4) or (mo=6) or (mo=9) or (mo=11)) then fin:=true;
  if (jo=30) and ((an mod 4)=0) and (mo=2) then fin:=true;
  if (jo=29) and ((an mod 4)<>0) and (mo=2) then fin:=true;

  end; (* while not fin....*)

writeln(fichier,'__________________________________________________________________________');
writeln(fichier,#12);
close(fichier);

MessageDlg('Fin de génération, cliquer sur OK pour voir le résultat',mtInformation, [mbOK], 0);
 OpenDocument(nomfichier); { *Converti depuis ShellExecute* }

end;


procedure edit_pbl(date1,date2:str10;pas:real);
var
 heure : str8;
 jjulien_dep,jjulien_arr,p,b0,l0: real;
 ok				: boolean;
begin
heure:='12:00:00';

assign(fichier,'ephem.txt');
rewrite(fichier);

writeln(fichier,'Angles P, b0 et l0 du Soleil du ',date1,' au ',date2);
writeln(fichier,'Calcul pour 12h00:00 TU');
writeln(fichier,'');
writeln(fichier,'En degres');
writeln(fichier,'');
writeln(fichier,'   Date   ',' ','    P   ',' ','   b0   ',' ','   l0   ');
writeln(fichier,'');

jjulien_dep:=julien(date1,heure);
jjulien_arr:=julien(date2,heure);

while jjulien_dep<=jjulien_arr do
 begin

  invjulien(jjulien_dep,date1,heure);
  angles_pbl(jjulien_dep,p,b0,l0);

  writeln(fichier,date1:10,' ',p:8:2,' ',b0:8:2,' ',l0:8:2);

  jjulien_dep:=jjulien_dep+pas;
 end;
writeln(fichier,#12);
close(fichier);

end;

procedure edition_pbl;
var
 chaine      : string;
 date1,date2 : str10;
 pas         : real;
 erreur      : integer;

 debut_ch    : str10;
 fin_ch     : str10;

 begin

  debut_ch:=copy(FormatDateTime('01/01/yyyy',Now),1,10);
  fin_ch:=copy(FormatDateTime('31/12/yyyy',Now),1,10);


date1:= InputBox('Angles P B L', 'Saisir date départ', debut_ch);
date2:= InputBox('Angles P B L', 'Saisir date fin', fin_ch);
chaine:=InputBox('Angles P B L', 'Pas en jours', '1');

val(chaine,pas,erreur);


edit_pbl(date1,date2,pas);

MessageDlg('Fin de génération, cliquer sur OK pour voir le résultat',mtInformation, [mbOK], 0);
 OpenDocument('ephem.txt'); { *Converti depuis ShellExecute* }

end;


procedure edit_crn(annee:word);
var
 date1,date2             : str10;
 heure                   : str8;
 jjulien_dep,jjulien_arr,jjulien_inter : real;
 num_crn,jour,p,b,l      : real;
 erreur                  : integer;

begin

 str(annee,date1);
 date2:='31/12/'+date1;
 date1:='01/01/'+date1;
 heure:='00:00:00';
 num_crn:=int(numero_crn(date1,heure));
 jjulien_dep:=27.2753*(num_crn-1690)+2444235.34;
 jjulien_arr:=julien(date2,heure);

 assign(fichier,'ephcrn.txt');
 rewrite(fichier);


 writeln(fichier,'Rotations Synodiques - Annee ',annee);
 writeln(fichier,'');
 writeln(fichier,' CRN ',' ','   Date   ',' ','  Heure ',' ','Jour');
 writeln(fichier,'');

 while jjulien_dep<=jjulien_arr do
  begin
   angles_pbl(jjulien_dep,p,b,l);
   if l>180 then l:=l-360;
   jjulien_dep:=jjulien_dep+l*27.2753/360;
   invjulien(jjulien_dep,date1,heure);
   jjulien_inter:=julien(date1,'00:00:00');
   val(copy(date1,1,2),jour,erreur);
   jour:=jour+jjulien_dep-jjulien_inter;
   writeln(fichier,num_crn:5:0,' ',date1:10,' ',heure:8,' ',jour:5:2);
   num_crn:=num_crn+1;
   jjulien_dep:=27.2753*(num_crn-1690)+2444235.34;
  end;
 writeln(fichier,#12);
 close(fichier);

MessageDlg('Fin de génération, cliquer sur OK pour voir le résultat',mtInformation, [mbOK], 0);
 OpenDocument('ephcrn.txt'); { *Converti depuis ShellExecute* }


end;

procedure edit_parallaxe(annee,pas:word);
var
 date1,date2   : str10;
 heure,alpha   : str8;
 angsize,rayon,parallaxe,bidon,jjulien_dep,jjulien_arr,jjulien2 : real;
 distance,secondes                                              : real;
 degres,minutes                                                 : word;

begin
angsize:=0.0;

str(annee:4,date1);
date2:=date1;
date1:='01/01/'+date1;
date2:='31/12/'+date2;
heure:='00:00:00';


assign(fichier,'paral.txt');
rewrite(fichier);

writeln(fichier,'SOLEIL - Annee ',annee);
writeln(fichier,'Parallaxe Horizontale Equatoriale, Demi-Diametre et Distance');
writeln(fichier,'');
writeln(fichier,'');
writeln(fichier,'   Date    Parallaxe   Demi-Diametre     Distances … 0hUT');
writeln(fichier,'           Horiz.Eq.     … 12hUT       EN UA   En 10000 km');
writeln(fichier,'');

jjulien_dep:=julien(date1,heure);
jjulien_arr:=julien(date2,heure)+1;

while jjulien_dep<=jjulien_arr do
 begin

 invjulien(jjulien_dep,date1,heure);
 jjulien2:=jjulien_dep+0.5;
 calc_soleil(jjulien_dep,alpha,bidon,rayon,bidon,bidon,bidon,bidon,parallaxe,angsize);
 distance:=rayon;
 write(fichier,date1:10,'  ',3600*parallaxe:5:2,'"      ');
 calc_soleil(jjulien2,alpha,bidon,rayon,bidon,bidon,bidon,bidon,parallaxe,angsize);
 angsize:=angsize/2;
 conversion_angle(angsize,degres,minutes,secondes);
 write(fichier,minutes:2,' mn ',secondes:5:2,'"');
 writeln(fichier,' ',distance:7:5,'     ',(distance*unite_astronomique/10000):5:0);

 jjulien_dep:=jjulien_dep+pas;
end;


close(fichier);

MessageDlg('Fin de génération, cliquer sur OK pour voir le résultat',mtInformation, [mbOK], 0);
 OpenDocument('paral.txt'); { *Converti depuis ShellExecute* }

end;







end.
