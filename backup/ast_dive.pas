Unit Ast_dive;

{$MODE Delphi}

interface
uses AST_FIC,
     AST_GEN,
     AST_SUN,
     ast_moon,
     sysutils,
     ast_plan,


Dialogs;
     {ast_codage;}

procedure divers;
procedure annuaire;
Function Twilight(delta:real;latitude:real) : str8;
Procedure renvoi_coordMessier(NumMessier : byte;var alpha :str8;var delta : real);
Procedure renvoi_coordNGC(NumNGC : longint;var alpha :str8;var delta : real);


implementation

Function Twilight(delta:real;latitude:real) : str8;
var
 h 	: real;
 Hprime	: real;
 t	: real;
 heure	: str8;
begin
 h:=arccosinus(-tangente(latitude)*tangente(delta));
 hprime:=arccosinus((cosinus(108)-sinus(latitude)*sinus(delta))/(cosinus(latitude)*cosinus(delta)));
 t:=(hprime-h)/15;
 t:=t*0.9973;
 heuredecmin(t,heure);
 twilight:=heure;

end;

procedure tempsagreenwich;
var
 date           : str10;
 heure,resultat : str8;
 chaine         : string;
 instant        : real;
begin
{
window(1,1,80,25);
cadre;
titre('Temps Sid‚ral … Greenwich');
gotoxy(4,4);
write('Date ? ');
saisstr(false,false,false,10,'01/01/1980',chaine);
date:=chaine;
gotoxy(4,6);
write('Heure ? ');
saisstr(false,false,false,8,'00:00:00',chaine);
heure:=chaine;
instant:=julien(date,'00:00:00');
tsg(instant,heure,instant);
heuredecmin(instant,resultat);
gotoxy(4,8);
write('R‚sultat : ',resultat);
pauseclav;

window(1,1,80,25);
 }
end;

procedure levcou;
var

 choix   : integer;
 quitter : boolean;
 chaine  : string;
 date    : str10;
 heure,alpha   : str8;
 instant,delta,greenwich : real;
begin
 {quitter:=false;
 while not quitter do
  begin
   cadre;
   titre('Lever/M‚ridien/Coucher d''un Astre');
   gotoxy(4,4);
   option[1]:='Calculer';
   option[2]:='Quitter';

   choixligne(2,option,choix);

   if choix=1 then
    begin
     gotoxy(20,4);
     write('Date ? ');
     saisstr(false,false,false,10,'01/01/1980',chaine);
     date:=chaine;
     heure:='00:00:0000';

     instant:=julien(date,'00:00:00');
     tsg(instant,heure,greenwich);

     gotoxy(4,7);
     write('Coordonn‚e Alpha ? ');
     saisstr(false,false,false,8,'00:00:00',chaine);
     alpha:=chaine;
     gotoxy(4,8);
     write('Coordonn‚e Delta ? ');
     saisreal(6,2,0,delta);

     gotoxy(4,10);
     write('Latitude en Degr‚s ? ');
     saisreal(6,2,latitude,latitude);

     gotoxy(4,11);
     write('Longitude en Degr‚s ? (+ vers Est)');
     saisreal(6,2,longitude,longitude);

     levercoucher(true,alpha,greenwich,latitude,longitude,delta,0,heure);
     gotoxy(4,13);
     write('LEVER    = ',heure);

     meridien(alpha,greenwich,longitude,heure);
     gotoxy(4,14);
     write('MERIDIEN = ',heure);

     levercoucher(false,alpha,greenwich,latitude,longitude,delta,0,heure);
     gotoxy(4,15);
     write('COUCHER  = ',heure);
     pauseclav;

   end
   else
    quitter:=true;
  end;
  }
end;

procedure passmerid;
var
 chaine            : string;
 date              : str10;
 alpha,heure       : str8;
 instant,greenwich : real;
begin
{window(1,1,80,25);
cadre;
titre('Passage au M‚ridien d''un Astre');
gotoxy(4,4);
write('Date ? ');
saisstr(false,false,false,10,'01/01/1980',chaine);
date:=chaine;
heure:='00:00:0000';

instant:=julien(date,'00:00:00');
tsg(instant,heure,greenwich);

gotoxy(4,7);
write('Coordonn‚e Alpha ? ');
saisstr(false,false,false,8,'00:00:00',chaine);
alpha:=chaine;

gotoxy(4,11);
write('Longitude en Degr‚s ? (+ vers Est)');
saisreal(6,0,longitude,longitude);

meridien(alpha,greenwich,longitude,heure);

gotoxy(4,13);
write('Instant = ',heure);
pauseclav;
window(1,1,80,25);
}end;

procedure TestJulien;
var
 chaine            : string;
 date              : str10;
 heure             : str8;
 instant	   : real;
begin
{window(1,1,80,25);
cadre;
titre('Jour Julien');
gotoxy(4,4);
write('Date ? ');
saisstr(false,false,false,10,'01/01/1980',chaine);
date:=chaine;
gotoxy(4,7);
write('Heure ? ');
saisstr(false,false,false,8,'00:00:00',chaine);
heure:=chaine;

instant:=julien(date,heure);

gotoxy(4,9);
write('Jour Julien = ',instant:0:3);
pauseclav;
 }
end;

procedure tempsidloc;
var
 chaine            : string;
 date              : str10;
 heure             : str8;
 instant,greenwich : real;
begin
{window(1,1,80,25);
cadre;
titre('Temps Sid‚ral Local');
gotoxy(4,4);
write('Date ? ');
saisstr(false,false,false,10,'01/01/1980',chaine);
date:=chaine;
gotoxy(4,7);
write('Heure ? ');
saisstr(false,false,false,8,'00:00:00',chaine);
heure:=chaine;

instant:=julien(date,'00:00:00');
tsg(instant,heure,greenwich);

gotoxy(4,9);
write('Longitude en Degr‚s ? (+ vers Est)');
saisreal(7,2,longitude,longitude);

tsl(instant,heure,longitude,instant);
heuredecmin(instant,heure);
gotoxy(4,13);
write('Temps sid‚ral local = ',heure);
window(1,1,80,25);
pauseclav;
 }
end;

Procedure renvoi_coordMessier(NumMessier : byte;var alpha :str8;var delta : real);
type Messier_Cat= record
  Num_Messier	: byte;
  Num_Ngc	: integer;
  Constellation : string[14];
  ConstAb	: string[6];
  TypObj	: string[2];
  SType		: string[2];
  ad		: string[8];
  delta		: real;
  Magnitude	: real;
  Diametre	: string[8];
  Distance	: real;
  Comment	: string[255];
 end;
 var
 fdat : file of Messier_Cat;
 vdat : Messier_Cat;
begin
 assign(fdat,'messicat.dat');
 reset(fdat);
 while not eof(fdat) do
  begin
   read(fdat,vdat);
   if vdat.num_messier=numMessier then
    begin
     alpha:=vdat.ad;
     delta:=vdat.delta;
     seek(fdat,filesize(fdat));
    end;
  end;
  close(fdat);
end;

Procedure renvoi_coordNGC(NumNGC : longint;var alpha :str8;var delta : real);
type Messier_Cat= record
  Num_Messier	: byte;
  Num_Ngc	: integer;
  Constellation : string[14];
  ConstAb	: string[6];
  TypObj	: string[2];
  SType		: string[2];
  ad		: string[8];
  delta		: real;
  Magnitude	: real;
  Diametre	: string[8];
  Distance	: real;
  Comment	: string[255];
 end;
 var
 fdat : file of ficobj;
 vdat : ficobj;
 toto : integer;
       erreur : word;
begin
 assign(fdat,'cata1.dat');
 reset(fdat);
 while not eof(fdat) do
  begin
   read(fdat,vdat);
   try
        val(trim(vdat.nom),toto,erreur);
   finally
   end;

   if toto=numNGC then
    begin
     alpha:=vdat.alpha;
     delta:=vdat.delta;
     seek(fdat,filesize(fdat));
    end;


  end;
  close(fdat);
end;


procedure divers;
var

 choix   : integer;

begin
{
quitter:=false;

while not quitter do
begin
 cadre;
 titre('EPHEMERIDES ASTRONOMIQUES - Calculs Divers');
 gotoxy(4,4);
 option[1]:='Jour Julien';
 option[2]:='Temps Sid‚ral Greenwich';
 option[3]:='Lever/M‚ridien/Coucher d''astre';
 option[4]:='Passage au m‚ridien';
 option[5]:='Temps Sid‚ral Local';
 option[6]:='Suivi temps r‚el';
 option[7]:='Essais codeur souris';
 option[8]:='Quitter';

 choixligne(8,option,choix);

 case choix of
  1: testjulien;
  2: tempsagreenwich;
  3: levcou;
  4: passmerid;
  5: tempsidloc;
  6: suivitpsreel;
  7: {codage}; {Unit‚ Ast_Codage.pas}
 { 8: quitter:=true;
 end;

end;
 }
end;

procedure annuaire;
var
 mo,annee           : word;
 choix              : integer;
 ch                 : char;
 abandon            : boolean;
begin
 {window(1,1,80,25);
 cadre;
 titre('Edition ComplŠte Eph‚m‚rides Annuels');
 gotoxy(4,4);
 write('Ann‚e ? ');
 saisword(4,1992,annee);
 gotoxy(4,6);
 write('Latitude Lieu Observation ? ');
 saisreal(5,2,latitude,latitude);
 gotoxy(4,8);
 write('Longitude du lieu d''Observation ? ');
 saisreal(8,3,longitude,longitude);
 gotoxy(4,10);
 write('Lancement Edition ? ');
 option[1]:='NON';
 option[2]:='OUI';
 choixligne(2,option,choix);

 if choix=2 then
 begin
  abandon:=false;
  for mo:=1 to 12 do
   begin
    if keypressed then
    begin
     ch:=readkey;
     abandon:=true;
    end;

   if not abandon then edit_soleil(annee,mo,latitude,longitude);

    if keypressed then
    begin
     ch:=readkey;
     abandon:=true;
    end;

    if not abandon then edit_lune(annee,mo,latitude,longitude);
   inc(mo);

  end;




 end;
 window(1,1,80,25);
 }
 end;

end.

