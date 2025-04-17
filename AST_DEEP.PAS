Unit AST_DEEP;

{$MODE Delphi}

interface
uses LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,AST_FIC,
     AST_GEN,
     AST_SUN,
     ast_moon,
     ast_plan,
     ast_dive;

Procedure ConversionDeep;
procedure Hautaz;

Implementation


Procedure ConversionDeep;
var
 ftext  : text;
 chaine : string;
 fdat	: file of ficobj;
 vdat	: ficobj;
 erreur	: integer;
 valeur	: real;
 chaine2: string;
begin
 assign(ftext,'AGCTOUT.TXT');
 reset(ftext);
 assign(fdat,'CATA1.dat');
 rewrite(fdat);
 while not eof(ftext) do
  begin
   readln(ftext,chaine);
   if copy(chaine,1,1)<>' ' then
    begin
     vdat.constellation:=copy(chaine,1,20)
    end
    else
    begin
     vdat.nom:=copy(chaine,5,7);
     vdat.alpha:=copy(chaine,14,2)+':'+copy(chaine,17,2);
     val(copy(chaine,20,1),valeur,erreur);
     valeur:=60*valeur;
     str(valeur:2:0,chaine2);
     vdat.alpha:=vdat.alpha +':'+ chaine2;

     val(copy(chaine,23,5),vdat.delta,erreur);
     val(copy(chaine,30,4),vdat.magnitude,erreur);
     if erreur<>0 then val(copy(chaine,30,3),vdat.magnitude,erreur);
     if erreur<>0 then vdat.magnitude:=-99;
     vdat.typegal:=copy(chaine,36,4);
     vdat.commentaire:=copy(chaine,43,length(chaine)-42);
     write(fdat,vdat);
    end;
  end;
 close(fdat);
 close(ftext);
 MessageDlg('Conversion OK ciel profond',mtInformation, [mbOK], 0);


end;


procedure Hautaz;
var
  chaine 	: string;
  date		: str10;
  heure		: str8;
  hmeridien     : str8;
  hcoucher	: str8;
  hleversol	: str8;
  hcouchersol	: str8;
  instant 	: real;
  greenwich	: real;
  hauteur	: real;
  z		: real;
  m		: real;
  delta		: real;
  rayon		: real;
  t         	: real;
  long1,long2,long3 : real;
  parallaxe	: real;
  angsize	: real;
  hmini		: integer;
  magmax	: integer;
  erreur        : integer;
 fdat : file of ficobj;
 vdat : ficobj;
 n,o	: integer;
 tri	: integer;
 fichier : text;

 choix	: integer;
 hleversol2 : real;
 hcouchersol2 : real;
 twilight2	: real;
 alpha		: str8;
begin


date:=copy(FormatDateTime('dd/mm/yyyy hh:mm:ss',Now),1,10);
heure:=copy(FormatDateTime('dd/mm/yyyy hh:mm:ss',Now),12,8);

chaine:=InputBox('Ciel Profond', 'Date', date);
date:=chaine;

chaine:=InputBox('Ciel Profond', 'Heure', heure);
heure:=chaine;

instant:=julien(date,'00:00:00');
tsg(instant,'00:00:00',greenwich);

chaine:=InputBox('Ciel Profond', 'Hauteur Min', '30');
val(chaine,hmini,erreur);

chaine:=InputBox('Ciel Profond', 'Magnitude Maxi', '10');
val(chaine,magmax,erreur);

 
 assign(fdat,'CATA1.dat');
 reset(fdat);

 assign(fichier,'deephaz.htm');

 rewrite(fichier);

 writeln(fichier,'<html>');
 writeln(fichier,'<head>');
 writeln(fichier,'<meta http-equiv="Content-Type"');
 writeln(fichier,'content="text/html; charset=iso-8859-1">');
 writeln(fichier,'<title>Positions d''Objets Ciel Profond</title>');
 writeln(fichier,'</head>');

 writeln(fichier,'<p align="center"><font size="2">Hauteur et Azimut des Objets');
 writeln(fichier,'Ciel Profond le ',date,' à ',heure,' <BR> Hauteur mini : ',hmini, ' degres <BR> Magnitude Maxi : ',magmax);
 writeln(fichier,'</font></p>');


 writeln(fichier,'<p align="center"><font size="2">Latitude ',global_latitude:5:2,'°');
 writeln(fichier,'Longitude ',global_longitude:5:2,'°</font></p>');


 writeln(fichier,'<p><font size="2">Informations Eclairage Soleil</font><br>');

 t:=julien(date,'00:00:00');
 calc_soleil(t,alpha,delta,rayon,long1,long2,long3,m,parallaxe,angsize);
 levercoucher(true,alpha,greenwich,global_latitude,global_longitude,delta,parallaxe,angsize,hleversol);
 levercoucher(false,alpha,greenwich,global_latitude,global_longitude,delta,parallaxe,angsize,hcouchersol);

 writeln(fichier,'Coucher à ',hcouchersol,'<BR>');
 writeln(fichier,'Lever  à ',hleversol,'<BR>');

 writeln(fichier,'Nuit astronomique a +/-',twilight(delta,global_latitude),'<BR>');

 heuremindec(hleversol,hleversol2);
 heuremindec(hcouchersol,hcouchersol2);
 heuremindec(twilight(delta,global_latitude),twilight2);

 hleversol2:=hleversol2-twilight2;
 hcouchersol2:=hcouchersol2+twilight2;

 heuredecmin(hleversol2,hleversol);
 heuredecmin(hcouchersol2,hcouchersol);

 writeln(fichier,'Debut Nuit Astronomique à ',hcouchersol,'<BR>');
 writeln(fichier,'Fin Nuit Astromique  à ',hleversol,'<BR>');
 writeln(fichier,'<BR>');

 writeln(fichier,'<table border="1" width="100%">');
 writeln(fichier,'<tr>');
 writeln(fichier,'<td>Numero Colorado</td>');
 writeln(fichier,'<td>Constellation</td>');
 writeln(fichier,'<td>Commentaire</td>');
 writeln(fichier,'<td>Type</td>');
 writeln(fichier,'<td>Alpha</td>');
 writeln(fichier,'<td>Delta degr&eacute;s</td>');
 writeln(fichier,'<td>Magnitude</td>');
 writeln(fichier,'<td>Hauteur</td>');
 writeln(fichier,'<td>Azimut</td>');
 writeln(fichier,'</tr>');

 n:=0;

 while not eof(fdat) do
  begin
   read(fdat,vdat);

   Equatorial_to_Horizontal(instant,heure,0,vdat.alpha,global_latitude,global_longitude,vdat.delta,hauteur,z);


   if (hauteur>=hmini) and (vdat.magnitude<=magmax) then
    begin
     inc(n);
     writeln(fichier,'<tr>');
     writeln(fichier,'<td>', vdat.nom ,'</td>');
     writeln(fichier,'<td>', vdat.constellation ,'</td>');
     writeln(fichier,'<td>', vdat.commentaire ,'</td>');
     writeln(fichier,'<td>', vdat.TypeGal ,'</td>');
     writeln(fichier,'<td>', vdat.alpha ,'</td>');
     writeln(fichier,'<td>', vdat.delta:6:1 ,'</td>');
     writeln(fichier,'<td>', vdat.magnitude:4:1 ,'</td>');
     writeln(fichier,'<td>',hauteur:3:2 ,'</td>');
     writeln(fichier,'<td>', z:3:2 ,'</td>');

     writeln(fichier,'</tr>');
    end;
   end;

  close(fdat);

    writeln(fichier,'</table><BR>');
    writeln(fichier,n,' Objets list&eacute;s<BR><BR>');

    writeln(fichier,'Definition des Types: <BR>');
    writeln(fichier,'PN   = PLANETARY NEBULA<BR>');
    writeln(fichier,'OC   = OPEN CLUSTER<BR>');
    writeln(fichier,'GN   = GASEOUS NEBULA<BR>');
    writeln(fichier,'GC   = GLOBULAR CLUSTER<BR>');
    writeln(fichier,'DN   = DIFFUSE NEBULA<BR>');
    writeln(fichier,'G    = GALAXY<BR>');
    writeln(fichier,'DS   = DOUBLE STAR (M-40)<BR>');
    writeln(fichier,'QSO  = QUASAR<BR>');
    writeln(fichier,'COMP = COMPOSITE OBJECT (OC + GN)<BR>');
    writeln(fichier,'RN   = REFLECTION NEBULA<BR>');
    writeln(fichier,'<BR>');
    writeln(fichier,'Calcul&eacute; par ASTRONOM / F.GOUYAUD');

    writeln(fichier,'</body>');
    writeln(fichier,'</html>');

   closefile(fichier);
   MessageDlg('Fin de génération, cliquer sur OK pour voir le résultat',mtInformation, [mbOK], 0);
    OpenDocument('deephaz.htm'); { *Converti depuis ShellExecute* }


end;

end.
