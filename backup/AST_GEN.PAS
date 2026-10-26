Unit AST_GEN;

{$MODE Delphi}

interface
uses LCLIntf, LCLType, SysUtils,AST_FIC,Dialogs,Math;

const
 unite_astronomique = 149597870;     { km }
 HEURE_ABSENTE      = '--:--:--';    { pas de lever / coucher (jour ou nuit polaire) }

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
function visible(jj : real;alpha : str8;delta : real;alphaS : str8;deltaS : real) : boolean;


implementation

const
  REFRACTION_DEG = 0.566;     { 34' de refraction atmospherique a l'horizon }

  { ------------------------------------------------------------------ }
  {  Trigonometrie en degres                                             }
  { ------------------------------------------------------------------ }

  function Power(X, Y: Real): Real;
  begin
    Result := Math.Power(X, Y);
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

{ Les arguments sont ramenes dans [-1, 1] : une erreur d'arrondi
  (1.0000000000000002) ne provoque plus d'exception. }
function arcsinus(valeur:real):real;
begin
  if valeur > 1 then valeur := 1;
  if valeur < -1 then valeur := -1;
  Result := RadToDeg(ArcSin(valeur));
end;

function arccosinus(valeur:real):real;
begin
  if valeur > 1 then valeur := 1;
  if valeur < -1 then valeur := -1;
  Result := RadToDeg(ArcCos(valeur));
end;
{ ramene entree dans [0, 360[ (sans boucle : sur pour les grandes valeurs) }
procedure range0a360(entree:real;var sortie:real);
begin
  entree := entree - 360.0*Int(entree/360.0);
  if entree < 0 then entree := entree + 360.0;
  if entree >= 360.0 then entree := 0;      { arrondi flottant, ex. -1E-20 }
  sortie := entree;
end;

{ ramene x dans [0, 24[ }
function range0a24(x : real) : real;
begin
  x := x - 24.0*Int(x/24.0);
  if x < 0 then x := x + 24.0;
  if x >= 24.0 then x := 0;
  Result := x;
end;

{ angle polaire de (x,y) en degres, dans [0, 360[ }
procedure corriquad(x,y:real;var resu:real);
begin
  resu := RadToDeg(ArcTan2(y, x));
  range0a360(resu, resu);
end;

function ChampEntier(const s, sep : string; n : Integer) : Integer;
var
  p, k   : Integer;
  reste  : string;
begin
  reste := s;
  for k := 1 to n - 1 do
  begin
    p := Pos(sep, reste);
    if p = 0 then
    begin
      Result := 0;
      Exit;
    end;
    reste := Copy(reste, p + 1, Length(reste));
  end;
  p := Pos(sep, reste);
  if p > 0 then reste := Copy(reste, 1, p - 1);
  Result := StrToIntDef(Trim(reste), 0);
end;

procedure LireHMS(const hms : string; out h, m, s : Integer);
begin
  h := ChampEntier(hms, ':', 1);
  m := ChampEntier(hms, ':', 2);
  s := ChampEntier(hms, ':', 3);
end;
(* conversion des heures et decimales en heures minutes secondes*)
procedure heuredecmin(decimal:real;var hms:str8);
var
  total, h, m, s : Int64;
  signe          : string;
begin
  total := Round(Abs(decimal) * 3600);
  if (decimal < 0) and (total > 0) then signe := '-' else signe := '';
  h := total div 3600;
  m := (total mod 3600) div 60;
  s := total mod 60;
  hms := Format('%s%.2d:%.2d:%.2d', [signe, h, m, s]);
end;

procedure heuremindec(hms:str8;var decimal:real);
  var
    h, m, s : Integer;
  begin
    LireHMS(hms, h, m, s);
    decimal := h + m/60 + s/3600;
  end;


{ ------------------------------------------------------------------ }
{  Coordonnees                                                         }
{ ------------------------------------------------------------------ }

{ ecliptique -> equatorial }
procedure par_27(lambda,beta,epsilon:real;var alpha : str8;var delta:real);
var
  sindelta, x, y, alphaprime : real;
begin
  sindelta := sinus(beta)*cosinus(epsilon)+cosinus(beta)*sinus(epsilon)*sinus(lambda);
  delta := arcsinus(sindelta);

  y := sinus(lambda)*cosinus(epsilon)-tangente(beta)*sinus(epsilon);
  x := cosinus(lambda);
  corriquad(x, y, alphaprime);

  alphaprime := alphaprime/15;
  heuredecmin(alphaprime, alpha);
  { l'arrondi peut donner 24:00:00 : une ascension droite vaut alors 00:00:00 }
  if Copy(alpha, 1, 2) = '24' then alpha := '00' + Copy(alpha, 3, 6);
end;

{--------------------------------------------------------------}
{ Conversion des coordonnees geocentriques en topocentriques   }
{ Chat GPT le 26/10/2026                                       }
{                                                              }
{ alphaGeo     : ascension droite geocentrique HH:MM:SS        }
{ deltaGeo     : declinaison geocentrique en degres            }
{ vpi          : parallaxe horizontale en degres               }
{ latitudeObs  : latitude geographique, nord positif           }
{ longitudeObs : longitude geographique, est positif           }
{ altitudeObs  : altitude de l'observatoire en metres          }
{ jj           : jour julien de l'instant d'observation        }
{                                                              }
{ alphaTopo    : ascension droite topocentrique HH:MM:SS       }
{ deltaTopo    : declinaison topocentrique en degres           }
{                                                              }
{ Le vecteur de l'observateur est calcule avec l'ellipsoide     }
{ WGS84. Les distances sont exprimees en kilometres.           }
{--------------------------------------------------------------}

procedure GeoVersTopo(
  alphaGeo: str8;
  deltaGeo, vpi: real;
  latitudeObs, longitudeObs, altitudeObs: real;
  jj: real;
  var alphaTopo: str8;
  var deltaTopo: real
);

const
  A_TERRE = 6378.137;           { Rayon equatorial WGS84, km }
  E2_TERRE = 0.00669437999014;  { Premiere excentricite au carre }

var
  alphaG, deltaG : real;
  distanceLune   : real;
  latitudeRad    : real;
  theta          : real;
  gst            : real;
  altitudeKm     : real;
  n              : real;

  { Vecteur geocentrique de la Lune }
  xL, yL, zL     : real;

  { Vecteur geocentrique de l'observateur }
  xO, yO, zO     : real;

  { Vecteur topocentrique de la Lune }
  xT, yT, zT     : real;

  rho            : real;
  alphaT         : real;

begin

  {------------------------------------------------------------}
  { 1. Conversion de l'ascension droite en degres               }
  {------------------------------------------------------------}

  heuremindec(alphaGeo, alphaG);
  alphaG := alphaG * 15.0;

  deltaG := deltaGeo;

  {------------------------------------------------------------}
  { 2. Distance geocentrique de la Lune                         }
  {------------------------------------------------------------}

  { La parallaxe horizontale doit etre positive et non nulle. }

  if vpi <= 0 then
  begin
    alphaTopo := alphaGeo;
    deltaTopo := deltaGeo;
    Exit;
  end;

  distanceLune := A_TERRE / sinus(vpi);

  {------------------------------------------------------------}
  { 3. Vecteur geocentrique de la Lune                          }
  {------------------------------------------------------------}

  xL := distanceLune
        * cosinus(deltaG)
        * cosinus(alphaG);

  yL := distanceLune
        * cosinus(deltaG)
        * sinus(alphaG);

  zL := distanceLune
        * sinus(deltaG);

  {------------------------------------------------------------}
  { 4. Position de l'observateur sur l'ellipsoide WGS84         }
  {------------------------------------------------------------}

  latitudeRad := latitudeObs;
  altitudeKm := altitudeObs / 1000.0;

  n := A_TERRE /
       sqrt(1.0 - E2_TERRE * sqr(sinus(latitudeRad)));

  {------------------------------------------------------------}
  { 5. Temps sideral apparent de Greenwich                      }
  {------------------------------------------------------------}

  { jj correspond a l'instant d'observation, fraction de jour
    comprise. On passe 00:00:00 pour ne pas ajouter une seconde
    fois l'heure a ce jour julien. }

  tsg(jj, '00:00:00', gst);

  { Angle sideral local, en degres.
    La longitude est positive vers l'est. }

  theta := gst * 15.0 + longitudeObs;
  range0a360(theta, theta);

  {------------------------------------------------------------}
  { 6. Vecteur de l'observateur dans le repere equatorial       }
  {------------------------------------------------------------}

  xO := (n + altitudeKm)
        * cosinus(latitudeObs)
        * cosinus(theta);

  yO := (n + altitudeKm)
        * cosinus(latitudeObs)
        * sinus(theta);

  zO := (n * (1.0 - E2_TERRE) + altitudeKm)
        * sinus(latitudeObs);

  {------------------------------------------------------------}
  { 7. Vecteur topocentrique : Lune moins observateur            }
  {------------------------------------------------------------}

  xT := xL - xO;
  yT := yL - yO;
  zT := zL - zO;

  {------------------------------------------------------------}
  { 8. Retour aux coordonnees equatoriales                      }
  {------------------------------------------------------------}

  rho := sqrt(sqr(xT) + sqr(yT));

  alphaT := RadToDeg(ArcTan2(yT, xT));
  range0a360(alphaT, alphaT);

  deltaTopo := RadToDeg(ArcTan2(zT, rho));

  { Ascension droite en heures, minutes et secondes }

  heuredecmin(alphaT / 15.0, alphaTopo);

  if Copy(alphaTopo, 1, 2) = '24' then
    alphaTopo := '00' + Copy(alphaTopo, 3, 6);

end;

{ ------------------------------------------------------------------ }
{  Calendrier                                                          }
{ ------------------------------------------------------------------ }

{ Jour julien ; date 'jj/mm/aaaa', heure 'hh:mm:ss' }
function julien(date:str10;heure:str8):real;
var
 jo,mo,an,he,mi,se : integer;
 a,b,y,m           : real;
 gregorien         : boolean;
begin
  jo := ChampEntier(date, '/', 1);
  mo := ChampEntier(date, '/', 2);
  an := ChampEntier(date, '/', 3);
  LireHMS(heure, he, mi, se);

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

  gregorien := (an>1582)
            or ((an=1582) and (mo>10))
            or ((an=1582) and (mo=10) and (jo>=15));

  if gregorien then
  begin
    a:=round(int(y/100));
    b:=2-a+round(int(a/4));
  end
  else b:=0;

  Result := jo+he/24+mi/1440+se/86400+round(int(365.25*y))+
            round(int(30.6001*(m+1)))+1720994.5+b;
end;

{ ------------------------------------------------------------------ }
{  Equation de Kepler                                                  }
{ ------------------------------------------------------------------ }

procedure kepler(petit_e,M:real;var grand_e:real);
var
 ecart,exe : real;
 n         : integer;
begin
  exe:=m;
  grand_e:=m;
  ecart:=10E6;
  n:=0;
  while (ecart>=1) and (n<100) do      { 100 : garde-fou contre une non-convergence }
  begin
    grand_e:=m+180/pi*petit_e*sinus(exe);
    ecart:=10E6*abs(grand_e-exe);
    exe:=grand_e;
    inc(n);
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

{------------------------------------}
{  calcul temps sideral a Greenwich  }
{Calculs Astronomiques page 27       }
{------------------------------------}
procedure tsg(jj:real;heure:str8;var zeta:real);
var
 instant, interm, t, correction : real;
 deltapsi, deltaepsilon         : real;
begin
  t:=(1.0*jj-2415020.0)/36525.0;
  interm:=0.276919398+100.0021359*t+0.000001075*sqr(t);
  interm:=24.0*(interm-int(interm));

  heuremindec(heure,instant);
  interm:=interm+1.002737908*instant;

  { correction de nutation en heures (equation des equinoxes) }
  nutation(jj,deltapsi,deltaepsilon);
  correction:=(deltapsi*cosinus(23.4417))/15;
  correction:=correction/3600;

  zeta:=range0a24(interm+correction);
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
 a1,a2,d : real;
 h       : double;
begin
  tsl(jj,heure,vlongitude,tslocal);
  heuremindec(alpha,a1);
  a1:=a1*15;
  a2:=(tslocal+centiemes/360000)*15;
  h:=a2-a1;

  hauteur:=sinus(vlatitude)*sinus(delta)+cosinus(h)*cosinus(vlatitude)*cosinus(delta);
  hauteur:=arcsinus(hauteur);

  d:=cosinus(vlatitude)*cosinus(hauteur);
  if Abs(d) < 1E-12 then
    z:=0                                   { au pole ou au zenith : azimut indetermine }
  else
  begin
    z:=(sinus(delta)-sinus(vlatitude)*sinus(hauteur))/d;
    z:=arccosinus(z);
    if sinus(h)>0 then z:=360-z;
  end;
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
 tsp                    : real;
 h                      : real;
 a,b,coshor,sinh0       : real;
begin
  heuremindec(alpha,tsp);
  sinh0:=sinus(parallaxe-REFRACTION_DEG-0.5*Abs(angsize));

  a:=sinus(vlatitude)*sinus(delta);
  b:=cosinus(vlatitude)*cosinus(delta);
  if Abs(b) < 1E-12 then
  begin
    instant:=HEURE_ABSENTE;
    Exit;
  end;

  coshor:=(sinh0-a)/b;
  if Abs(coshor) > 1 then
  begin
    instant:=HEURE_ABSENTE;
    Exit;
  end;

  h:=arccosinus(coshor)/15;
  { temps sideral du lever ou du coucher }
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

{ Jour julien -> date et heure, arrondies a la seconde la plus proche.
  L'arrondi est fait AVANT le decoupage : l'heure ne peut jamais valoir 24:00:00. }
procedure invjulien(jjulien:real;var date:str10;var heure:str8);
var
 jj, z, f, a, b, c, d, e, alpha : real;
 an, mo, jo                     : Integer;
 secondes                       : Int64;
begin
  jj := Round((jjulien + 0.5) * 86400) / 86400;
  z := int(jj);
  f := jj - z;

  secondes := Round(f * 86400);
  if secondes > 86399 then secondes := 86399;

  if z<2299161 then a:=z else
  begin
    alpha:=int((z-1867216.25)/36524.25);
    a:=z+1+alpha-int(alpha/4);
  end;
  b:=a+1524;
  c:=int((b-122.1)/365.25);
  d:=int(365.25*c);
  e:=int((b-d)/30.6001);

  jo := Trunc(b-d-int(30.6001*e));
  if e<13.5 then mo:=Trunc(e-1) else mo:=Trunc(e-13);
  if mo>2.5 then an:=Trunc(c-4716) else an:=Trunc(c-4715);

  date  := Format('%.2d/%.2d/%.4d', [jo, mo, an]);
  heure := Format('%.2d:%.2d:%.2d',
                  [secondes div 3600, (secondes mod 3600) div 60, secondes mod 60]);
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

{ Date du jour au format jj/mm/aaaa, quelle que soit la locale }
function datesysteme : string;
begin
  Result := FormatDateTime('dd"/"mm"/"yyyy', Now);
end;

function visible(jj : real;alpha : str8;delta : real;alphaS : str8;deltaS : real) : boolean;
var 
 jjulien0 : real;
 date : str10;
 heure : str8;
 z : real;
 interm : boolean;
 hauteur:real;
 hauteurS:real;
begin
 {Test si l'objet est visible hauteur>0 ET hauteur soleil<0}
    invjulien(jj,date,heure);  {Pour récupérer heure}
    jjulien0:=julien(date,'00:00:00');

    Equatorial_to_Horizontal(jjulien0,heure,0,alpha,global_latitude,global_longitude,delta,hauteur,z);
    equatorial_to_Horizontal(jjulien0,heure,0,alphaS,global_latitude,global_longitude,deltaS,hauteurS,z);

    if (hauteur>0) and (hauteurS<0) then interm:=true else interm:=false;

    Visible:=interm;

end;


end.
