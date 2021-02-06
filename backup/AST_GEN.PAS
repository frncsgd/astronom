Unit AST_GEN;

{$MODE Delphi}

interface
uses LCLIntf, LCLType, LMessages, SysUtils,AST_FIC;

const
 unite_astronomique=149597870;

function Power(X, Y: Real): Real;
function sinus(valeur:real):real;
function cosinus(valeur:real):real;
function tangente(valeur:real):real;
function arcsinus(valeur:real):real;
function arccosinus(valeur:real):real;
procedure range0a360(entree:real;var sortie:real);
procedure heuredecmin(decimal:real;var hms:str8);
procedure heuremindec(hms:str8;var decimal:real);
procedure corriquad(x,y:real;var resu:real);
procedure par_27(lambda,beta,epsilon:real;var alpha : str8;var delta:real);
function julien(date:str10;heure:str8):real;
procedure kepler(petit_e,M:real;var grand_e:real);
procedure ecart(alpha1:str8;delta1:real;alpha2:str8;delta2:real;var ecart:real);
procedure tsg(jj:real;heure:str8;var zeta:real);
procedure tsg2(date : str10;heure : str8;var zeta : real);

procedure tsl(jj:real;heure:str8;vlongitude:real;var tlocal:real);
procedure anglesdivers2(jj:real;heure,alpha:str8;vlatitude,vlongitude,delta:real;
                      var h,z,s : real);
procedure Equatorial_to_Horizontal(jj:real;heure:str8;centiemes:word;alpha:str8;vlatitude,vlongitude,delta:real;
                      var hauteur,z : real);
procedure levercoucher(lever:boolean;alpha:str8;greenwich,vlatitude,vlongitude,delta,
                       parallaxe,angsize:real;var instant :str8);
procedure meridien(alpha:str8;greenwich,vlongitude:real;var instant :str8);
procedure invjulien(jjulien:real;var date:str10;var heure:str8);
procedure conversion_angle(angle:real;var degres,minute : word;var secondes:real);
function dateSysteme: string;

implementation


function Power(X, Y: Real): Real;
begin
	Power := Exp(Y * Ln(X));
end;

function sinus(valeur:real):real;
var resultat : real;
begin
 resultat:=sin(valeur*pi/180);
 sinus:=resultat;
end;

function cosinus(valeur:real):real;
var resultat : real;
begin
 resultat:=cos(valeur*pi/180);
 cosinus:=resultat;
end;

function tangente(valeur:real):real;
var resultat : real;
begin
 resultat:=sinus(valeur)/cosinus(valeur);
 tangente:=resultat;
end;

function arcsinus(valeur:real):real;
var resultat : real;
begin
resultat:=sqrt(1-sqr(valeur));
resultat:=180/pi*arctan(valeur/resultat);
arcsinus:=resultat;
end;

function arccosinus(valeur:real):real;
var
x,y,resultat : real;
begin

y:=sqrt((1-sqr(valeur)));
x:=valeur;
corriquad(x,y,resultat);
arccosinus:=resultat;
end;

procedure range0a360(entree:real;var sortie:real);
var
 resultat :real;
 entier	: integer;
begin

while entree<0 do entree:=entree+360;
while entree>=360 do entree:=entree-360;
sortie:=entree;
end;

procedure corriquad(x,y:real;var resu:real);
begin

 if x<>0 then resu:=180/pi*arctan(y/x)
 else
  begin
   if y<0 then resu:=270 else resu:=90;
  end;

 if y=0 then
  begin
   if x<0 then resu:=180 else resu:=0;
  end;

 range0a360(resu,resu);

 if (x>0) and (y>0) and (resu>180) then resu:=resu-180;
 if (x<0) and (y>0) and (resu>270) then resu:=resu-180;
 if (x<0) and (y<0) and (resu<180) then resu:=resu+180;
 if (x>0) and (y<0) and (resu<270) then resu:=resu+180;
end;

(* conversion des heures et decimales en heures minutes secondes*)
procedure heuredecmin(decimal:real;var hms:str8);
var
 h,m,s : integer;
 reste : real;
 chaine: string;
begin
 h:=round(int(decimal));
 reste:=decimal-h;
 m:=round(int(60*reste));
 reste:=60*reste-m;
 s:=round(int(60*reste));

 str(h:2,chaine);
 hms:=chaine+':';
 str(m:2,chaine);
 hms:=hms+chaine+':';
 str(s:2,chaine);
 hms:=hms+chaine;

end;

procedure heuremindec(hms:str8;var decimal:real);
var
 erreur : integer;
 h,m,s  : word;

begin

 val(copy(hms,1,2),h,erreur);
 val(copy(hms,4,2),m,erreur);
 val(copy(hms,7,2),s,erreur);

 decimal:=h+m/60+s/3600;

end;


procedure par_27(lambda,beta,epsilon:real;var alpha : str8;var delta:real);
var
 sindelta,x,y,alphaprime,reste : real;
 he,mi,se                      : word;
 chaine                        : string;

begin

 sindelta:=sinus(beta)*cosinus(epsilon)+cosinus(beta)*sinus(epsilon)*sinus(lambda);
 delta:=arcsinus(sindelta);

 y:=sinus(lambda)*cosinus(epsilon)-tangente(beta)*sinus(epsilon);
 x:=cosinus(lambda);

 corriquad(x,y,alphaprime);

 alphaprime:=alphaprime/15;

 heuredecmin(alphaprime,alpha);

end;

function julien(date:str10;heure:str8):real;
var
 jo,mo,an,he,mi,se : word;
 erreur            : integer;
 a,b,y,m           : real;
 gregorien         : boolean;

begin

 val(copy(date,1,2),jo,erreur);
 val(copy(date,4,2),mo,erreur);
 val(copy(date,7,4),an,erreur);

 val(copy(heure,1,2),he,erreur);
 val(copy(heure,4,2),mi,erreur);
 val(copy(heure,7,2),se,erreur);

 if mo<=2 then
  begin
   y:=an-1;
   m:=mo+12;
  end
  else
  begin
   y:=an;
   m:=mo;
  end;

  if (an>1582)
  or ((an=1582) and (mo>10))
  or ((an=1582) and (mo=10) and (jo>=15)) then
  begin
   a:=round(int(y/100));
   b:=2-a+round(int(a/4));
   gregorien:=true;
  end
  else gregorien:=false;

  if not gregorien then b:=0;

  julien:=jo+he/24+mi/1440+se/86400+round(int(365.25*y))+
                                    round(int(30.6001*(m+1)))+
                                    1720994.5+b;

end;

procedure kepler(petit_e,M:real;var grand_e:real);
var
 ecart,exe : real;
begin
exe:=m;
ecart:=10E6;
while ecart>=1 do
begin
 grand_e:=m+180/pi*petit_e*sinus(exe);
 ecart:=10E6*abs(grand_e-exe);
 exe:=grand_e;
end;

end;

procedure ecart(alpha1:str8;delta1:real;alpha2:str8;delta2:real;var ecart:real);
var
 h1,h2 : real;
begin

heuremindec(alpha1,h1);
h1:=15*h1;

heuremindec(alpha2,h2);
h2:=15*h2;

ecart:=arccosinus(sinus(delta1)*sinus(delta2)+cosinus(delta1)*cosinus(delta2)*cosinus(h1-h2));
end;
{------------------------------------}
{  calcul nutation                   }
{Calculs Astronomiques page 47       }
{------------------------------------}

Procedure Nutation(jj:real;var deltapsi : real;var deltaepsilon : real);
var
 t,l,lprime,m,mprime,omega,psi,epsilon	: double;

begin
 t:=(1.0*jj-2415020.0)/36525.0;
 l:=279.6967+36000.7689*t+0.000303*sqr(t);
 lprime:=270.4342+484267.8831*t-0.001133*sqr(t);
 m:=358.4758+35999.0498*t-0.000150*sqr(t);
 mprime:=296.1046+477198.8491*t+0.009192*sqr(t);
 omega:=259.1833-1934.1420*t+0.002078*sqr(t);

 psi:=-(17.2327+0.001737*t)*sinus(omega);
 psi:=psi-(1.2729+0.00013*t)*sinus(2*l);
 psi:=psi+0.2088*sinus(2*omega);
 psi:=psi-0.2037*sinus(2*lprime);
 psi:=psi+(0.1261-0.00031*t)*sinus(m);
 psi:=psi+0.0675*sinus(mprime);
 psi:=psi-(0.0497-0.00012*t)*sinus(2*l+m);
 psi:=psi-0.0342*sinus(2*lprime-omega);
 psi:=psi-0.0261*sinus(2*lprime+mprime);
 psi:=psi+0.0214*sinus(2*l-m);
 psi:=psi-0.0149*sinus(2*l-2*lprime+mprime);
 psi:=psi+0.0123*sinus(2*l-omega);
 psi:=psi+0.0114*sinus(2*lprime-mprime);

 deltapsi:=psi;

end;
function range0a24 (x :real) : real;
begin
 if x<0 then
  begin
   while x<0 do x:=x+24;
  end;
 if x>=24 then
  begin
   while x>=24 do x:=x-24;
  end;
 range0a24:=x;
end;

{------------------------------------}
{  calcul temps sid‚ral … Greenwich  }
{Calculs Astronomiques page 27       }
{------------------------------------}
procedure tsg(jj:real;heure:str8;var zeta:real);
var
 instant 		: real;
 t,interm,correction	: double;
 deltapsi,deltaepsilon	: real;
 minute,chaine		: string;
 vminute,erreur		: integer;
begin
   t:=(1.0*jj-2415020.0)/36525.0;
   interm:=0.276919398+100.0021359*t+0.000001075*sqr(t);
   interm:=24.0*(interm-int(interm));

   heuremindec(heure,instant);
   interm:=interm+1.002737908*instant;
   zeta:=interm;
   if zeta>=24 then zeta:=zeta-24;

   nutation(jj,deltapsi,deltaepsilon);
   correction:=(deltapsi*cosinus(23.4417))/15;
   correction:=correction/3600;
   zeta:=zeta+correction;

   {M‚thode 2  post 2000
   t:=(1.0*jj-2451543.5);

   heuremindec(heure,instant);
   interm:=6.5987042532333+(24*t+instant)/(86164.091/86400);
   interm:=range0a24(interm);
   zeta:=interm;}
end;



procedure tsg2(date : str10;heure : str8;var zeta : real);
var
 hh,mn,sec,jj,an,f,q,mois 	: integer;
 xt,s,i,interm 			: real;
 erreur				: integer;
begin
 val(copy(heure,1,2),hh,erreur);
 val(copy(heure,4,2),mn,erreur);
 val(copy(heure,7,2),sec,erreur);
 val(copy(date,1,2),jj,erreur);
 val(copy(date,4,2),mois,erreur);
 val(copy(date,7,4),an,erreur);

 case mois of
  1 : mois:=0;
  2 : mois:=31;
  3 : mois:=59;
  4 : mois:=90;
  5 : mois:=120;
  6 : mois:=151;
  7 : mois:=181;
  8 : mois:=212;
  9 : mois:=243;
  10 : mois:=273;
  11 : mois:=304;
  12 : mois:=334;

 end;
 xt:=jj+mois;

 if an>2000 then
  begin
   xt:=xt+1;
   if ((((an-2000)/4)-int((an-2000)/4)=0)) then
    begin
     if xt<59 then xt:=xt-1;
    end;
   end;
   q:=an-2000;
   s:=q*365;
   s:=s+int(q/4);
   xt:=xt+s;
  if an=2000 then
  begin
   if xt>59 then xt:=xt+1;
  end;

  i:=hh+mn/60+sec/3600;
  interm:=6.5987042532333+(24*xt+i)/(86164.091/86400);
  interm:=range0a24(interm);

  zeta:=interm;
end;

(* calcul temps sid‚ral local *)

procedure tsl(jj:real;heure:str8;vlongitude:real;var tlocal:real);
var
 t0,tt : real;
 t1    : real;
 minute,chaine		: string;
 vminute,erreur		: integer;
begin
 tsg(jj,'00:00:00',t0);

 heuremindec(heure,tt);
 tt:=tt*1.002737908; {Temps moyen en temps sid‚ral}
 t1:=t0+tt;
 tlocal:=(15*t1)+vlongitude;
 range0a360(tlocal,tlocal);
 tlocal:=tlocal/15;
end;

{Propre … la lune, origine formule pas trouv‚e, … quoi correspond s ??}
procedure anglesdivers2(jj:real;heure,alpha:str8;vlatitude,vlongitude,delta:real;
                      var h,z,s : real);
var
 tslocal : real;
 a1,a2   : real;
begin
tsl(jj,heure,vlongitude,tslocal);
tslocal:=tslocal*1.0027379;
heuremindec(alpha,a1);
a1:=a1*15;
a2:=tslocal*15;
h:=a2-a1;
range0a360(h,h);

a1:=sinus(vlatitude)*sinus(delta);
a2:=cosinus(vlatitude)*cosinus(delta)*cosinus(h);
z:=arccosinus(a1+a2);

a1:=cosinus(vlatitude)*sinus(h)/sinus(z);
s:=arcsinus(a1);
if h>180 then s:=-s;

end;

procedure Equatorial_to_Horizontal(jj:real;heure:str8;centiemes:word;alpha:str8;vlatitude,vlongitude,delta:real;
                      var hauteur,z : real);
var
 tslocal : real;
 a1,a2   : real;
 h	 : double;
begin
tsl(jj,heure,vlongitude,tslocal);
heuremindec(alpha,a1);
a1:=a1*15;
a2:=(tslocal+centiemes/360000)*15;
h:=a2-a1;

hauteur:=sinus(vlatitude)*sinus(delta)+cosinus(h)*cosinus(vlatitude)*cosinus(delta);
hauteur:=arcsinus(hauteur);

z:=sinus(delta)-sinus(vlatitude)*sinus(hauteur);
z:=z/(cosinus(vlatitude)*cosinus(hauteur));

z:=arccosinus(z);

if sinus(h)>0 then z:=360-z;

end;


procedure tempsmoyen(heure,greenwich,vlongitude:real;var instant:str8);
var heure2 :real;
begin

(*longitude en degr‚s et positive vers l'est*)
 heure:=15*heure-vlongitude-15*greenwich;
 range0a360(heure,heure2);
 heure:=heure2/15;
 heure:=heure*0.9972696;
 heuredecmin(heure,instant);
end;

procedure levercoucher(lever:boolean;alpha:str8;greenwich,vlatitude,vlongitude,delta,
                       parallaxe,angsize:real;var instant :str8);
var
 tsp           : real;
 h             : real;
 a,b,cosh,sinh0: real;
begin

(*formule expliquée sur https://promenade.imcce.fr/fr/pages3/367.html *)

 heuremindec(alpha,tsp);
 if lever then
 begin
  sinh0:=sinus(parallaxe-0.566-0.5*angsize);      (*0.566 correspond à 34' de réfraction *)
 end
 else
 begin
  sinh0:=sinus(parallaxe-0.566+0.5*angsize);      (*0.566 correspond à 34' de réfraction *)
 end;

 a:=sinus(vlatitude)*sinus(delta);
 b:=cosinus(vlatitude)*cosinus(delta);
 cosh:=(sinh0-a)/b;
 h:=arccosinus(cosh)/15;
 (*temps sidéral lever ou coucher*)
 if lever then tsp:=tsp-h else tsp:=tsp+h;
 tempsmoyen(tsp,greenwich,vlongitude,instant);

end;

procedure meridien(alpha:str8;greenwich,vlongitude:real;var instant :str8);
var
 tsp : real;
begin
 heuremindec(alpha,tsp);
 (*longitude en degr‚s et positive vers l'est*)
 tempsmoyen(tsp,greenwich,vlongitude,instant);
end;

procedure invjulien(jjulien:real;var date:str10;var heure:str8);
var
 z,f,a,b,c,d,e,alpha : real;
 an,mo,jo,he         : real;
 chaine              : string;
begin
 jjulien:=jjulien+0.5;
 z:=int(jjulien);
 f:=jjulien-z;
 if z<2299161 then a:=z else
  begin
   alpha:=int((z-1867216.25)/36524.25);
   a:=z+1+alpha-int(alpha/4);
  end;
 b:=a+1524;
 c:=int((b-122.1)/365.25);
 d:=int(365.25*c);
 e:=int((b-d)/30.6001);

 jo:=b-d-int(30.6001*e)+f;
 he:=24*(jo-int(jo));
 jo:=int(jo);
 if e<13.5 then mo:=e-1 else mo:=e-13;
 if mo>2.5 then an:=c-4716 else an:=c-4715;

heuredecmin(he,heure);
str(jo:2:0,chaine);
date:=chaine+'/';
str(mo:2:0,chaine);
date:=date+chaine+'/';
str(an:4:0,chaine);
date:=date+chaine;

end;

procedure conversion_angle(angle:real;var degres,minute : word;var secondes:real);
var
 interm : real;
begin
degres:=round(int(angle));
interm:=60*(angle-degres);
minute:=round(int(interm));
secondes:=60*(interm-minute);

end;

function datesysteme : string;
var
 y,mois,j,js 		: word;
 vdate,chaine	     	: string;
begin
      datesysteme:=copy(DateTimeToStr(Now),1,10);
end;

end.
