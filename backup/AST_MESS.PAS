Unit AST_MESS;

{$MODE Delphi}

interface
uses
     LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,AST_FIC,
     AST_GEN,
     AST_SUN,
     ast_plan,
     ast_dive;

Function OrderMara(num_messier:byte) : byte;
procedure Messier2;
Procedure NearStars;
Procedure ConversionMessier;

Implementation

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
Function OrderMara(num_messier:byte) : byte;
var
 ordre : byte;
begin

 case num_messier of
  77 : ordre:=1;
  74 : ordre:=2;
  33 : ordre:=3;
  31 : ordre:=4;
  32 : ordre:=5;
  110 : ordre:=6;
  52 : ordre:=7;
  103 : ordre:=8;
  76 : ordre:=9;
  34 : ordre:=10;
  45 : ordre:=11;
  79 : ordre:=12;
  42 : ordre:=13;
  43 : ordre:=14;
  78 : ordre:=15;
  1 : ordre:=16;
  35 : ordre:=17;
  37 : ordre:=18;
  36 : ordre:=19;
  38 : ordre:=20;
  41 : ordre:=21;
  93 : ordre:=22;
  47 : ordre:=23;
  46 : ordre:=24;
  50 : ordre:=25;
  48 : ordre:=26;
  44 : ordre:=27;
  67 : ordre:=28;
  95 : ordre:=29;
  96 : ordre:=30;
  105 : ordre:=31;
  65 : ordre:=32;
  66 : ordre:=33;
  81 : ordre:=34;
  82:ordre:=35;
  97 : ordre:=36;
  108 : ordre:=37;
  109 : ordre:=38;
  40 : ordre:=39;
  106 : ordre:=40;
  94 : ordre:=41;
  63 : ordre:=42;
  51 : ordre:=43;
  101 : ordre:=44;
  102:ordre:=45;53:ordre:=46;64:ordre:=47;3:ordre:=48;98:ordre:=49;
  99:ordre:=50;100:ordre:=51;85:ordre:=52;84:ordre:=53;86:ordre:=54;
  87:ordre:=55;89:ordre:=56;90:ordre:=57;88:ordre:=58;91:ordre:=59;58:ordre:=60;
  59:ordre:=61;60:ordre:=62;49:ordre:=63;61:ordre:=64;104:ordre:=65;
  68:ordre:=66;83:ordre:=67;5:ordre:=68;13: ordre:=69;
  92:ordre:=70;57:ordre:=71;56:ordre:=72;29:ordre:=73;39:ordre:=74;
  27:ordre:=75;71:ordre:=76;107:ordre:=77;12:ordre:=78;10:ordre:=79;
  14:ordre:=80;
  9:ordre:=81;4: ordre:=82;80:ordre:=83;19:ordre:=84;62:ordre:=85;
  6:ordre:=86;7 : ordre:=87;11 : ordre:=88;  26 : ordre:=89;
  16:ordre:=90;17 : ordre:=91;18 : ordre:=92;24 : ordre:=93;
  25:ordre:=94; 23 : ordre:=95; 21 : ordre:=96;20 : ordre:=97;
  8:ordre:=98;28 : ordre:=99;22 : ordre:=100;
  69:ordre:=101;70 : ordre:=102;54 : ordre:=103;55 : ordre:=104;
  75:ordre:=105;15 : ordre:=106;2 : ordre:=107;72 : ordre:=108;
  73:ordre:=109;30 : ordre:=110;
 end;

  {1.  M77     21.  M41     41.  M94     61.  M59     81.   M9    101.  M69
  2.  M74     22.  M93     42.  M63     62.  M60     82.   M4    102.  M70 
  3.  M33     23.  M47     43.  M51     63.  M49     83.  M80    103.  M54 
  4.  M31     24.  M46     44. M101     64.  M61     84.  M19    104.  M55
  5.  M32     25.  M50     45. M102?    65. M104     85.  M62    105.  M75 
  6. M110     26.  M48     46.  M53     66.  M68     86.   M6    106.  M15 
  7.  M52     27.  M44     47.  M64     67.  M83     87.   M7    107.   M2 
  8. M103     28.  M67     48.   M3     68.   M5     88.  M11    108.  M72 
  9.  M76     29.  M95     49.  M98     69.  M13     89.  M26    109.  M73 
 10.  M34     30.  M96     50.  M99     70.  M92     90.  M16    110.  M30 
 11.  M45     31. M105     51. M100     71.  M57     91.  M17 
 12.  M79     32.  M65     52.  M85     72.  M56     92.  M18 
 13.  M42     33.  M66     53.  M84     73.  M29     93.  M24 
 14.  M43     34.  M81     54.  M86     74.  M39     94.  M25 
 15.  M78     35.  M82     55.  M87     75.  M27     95.  M23 
 16.   M1     36.  M97     56.  M89     76.  M71     96.  M21 
 17.  M35     37. M108     57.  M90     77. M107     97.  M20 
 18.  M37     38. M109     58.  M88     78.  M12     98.   M8 
 19.  M36     39.  M40     59.  M91     79.  M10     99.  M28 
 20.  M38     40. M106     60.  M58     80.  M14    100.  M22}

 ordermara:=ordre;

end;


Procedure ConversionMessier;

var
 n	: integer;
 fdat : file of Messier_cat;
 vdat : messier_cat;
 ftext: text;
 chaine	: string;
 erreur	: integer;
 chaine2: string;
 chaine3: string;
 valeur	: real;
begin
 assign(fdat,'messicat.dat');
 rewrite(fdat);
 assign(ftext,'messier.dat');
 reset(ftext);
 while not eof(ftext) do
  begin
   readln(ftext,chaine);
   if (copy(chaine,1,1)<>'#') and (copy(chaine,1,1)<>' ') then
    begin
     with vdat do
      begin
       val(copy(chaine,1,3),num_messier,erreur);
       val(copy(chaine,6,4),num_ngc,erreur);
       constellation:=copy(chaine,11,14);
       constab:=copy(chaine,26,6);
       TypObj:=copy(chaine,34,1);
       SType:=copy(chaine,36,5);
       ad:=copy(chaine,42,2)+':'+copy(chaine,45,2);
       val(copy(chaine,48,1),valeur,erreur);
       valeur:=60*valeur;
       str(valeur:2:0,chaine2);
       ad:=ad +':'+ chaine2;
       chaine2:=copy(chaine,50,6);
       chaine2:=copy(chaine2,1,3)+'.'+copy(chaine2,5,2);
       val(chaine2,delta,erreur);
       val(copy(chaine,57,4),Magnitude,erreur);
       diametre:=copy(chaine,63,8);
       chaine2:=copy(chaine,72,7);
       chaine3:='';
       for n:=1 to 9 do
        begin
         if copy(chaine2,n,1)<>' ' then chaine3:=chaine3+copy(chaine2,n,1)
        end;
       val(chaine3,distance,erreur);
       if erreur<>0 then val(copy(chaine,73,5),distance,erreur);
       Comment:=copy(chaine,82,40);
       write(fdat,vdat);
      end;
    end;
  end;

  close(fdat);
  close(ftext);
  MessageDlg('Conversion OK objets messier',mtInformation, [mbOK], 0);


end;

procedure Messier2;

var
  chaine 	: string;
  date		: str10;
  hlever	: str8;
  hmeridien     : str8;
  hcoucher	: str8;
  hleversol	: str8;
  hcouchersol	: str8;
  heure		: str8;
  instant 	: real;
  greenwich	: real;
  alpha		: str8;
  delta		: real;
  rayon		: real;
  t         	: real;
  longvraie 	: real;
  m         	: real;
  parallaxe	: real;
  angsize	: real;
  erreur        : integer;

 fdat : file of Messier_cat;
 vdat : messier_cat;
 n	: integer;
 tri	: integer;
 fichier : text;

 choix	: integer;
 hleversol2 : real;
 hcouchersol2 : real;
 twilight2	: real;
begin

 date:=copy(DateTimeToStr(Now),1,10);
 heure:=copy(DateTimeToStr(Now),12,8);

 chaine:=InputBox('Lever/Coucher des Objets de Messier', 'Date', date);
 date:=chaine;

 heure:='00:00:0000';

 instant:=julien(date,'00:00:00');
 tsg(instant,heure,greenwich);


 chaine:=InputBox('Lever/Coucher des Objets de Messier', 'Trier par num.messier = 1 ou num.marathon = 2','1');
 val(chaine,tri,erreur);

 if (tri<>1) and (tri<>2) then tri:=1;

 assign(fdat,'messicat.dat');
 reset(fdat);

 if tri=1 then assign(fichier,'messier1.htm')
          else assign(fichier,'messier2.htm');

 rewrite(fichier);

 writeln(fichier,'<html>');
 writeln(fichier,'<head>');
 writeln(fichier,'<meta http-equiv="Content-Type"');
 writeln(fichier,'content="text/html; charset=iso-8859-1">');
 writeln(fichier,'<title>Visibilite des Objets Messier</title>');
 writeln(fichier,'</head>');


  writeln(fichier,'<p align="center"><font size="2">Caracteristiques Objets');
  writeln(fichier,'de Messier le ',date,'</font></p>');

  if tri=1 then
   writeln(fichier,'Par ordre de numero Messier <BR><BR>')
  else
    writeln(fichier,'Par ordre de numero Marathon <BR><BR>');

  writeln(fichier,'<p align="center"><font size="2">Latitude ',global_latitude:5:2,' deg.');
  writeln(fichier,'Longitude ',global_longitude:5:2,'deg.</font></p>');


  writeln(fichier,'<p><font size="2">Informations Eclairage Soleil</font><br>');

  hleversol:=heure;

  for n:=1 to 10 do
    begin
     t:=julien(date,hleversol);
     calc_soleil(t,alpha,delta,rayon,longvraie,longvraie,longvraie,m,parallaxe,angsize);
     levercoucher(true,alpha,greenwich,global_latitude,global_longitude,delta,0,angsize,hleversol);
    end;


  hcouchersol:=hleversol;
  for n:=1 to 10 do
    begin
     t:=julien(date,hcouchersol);
     calc_soleil(t,alpha,delta,rayon,longvraie,longvraie,longvraie,m,parallaxe,angsize);
     levercoucher(false,alpha,greenwich,global_latitude,global_longitude,delta,0,-angsize,hcouchersol);
    end;

  writeln(fichier,'Coucher : ',hcouchersol,'<BR>');
 writeln(fichier,'Lever  : ',hleversol,'<BR>');

 writeln(fichier,'Nuit astronomique : +/-',twilight(delta,global_latitude),'<BR>');

 heuremindec(hleversol,hleversol2);
 heuremindec(hcouchersol,hcouchersol2);
 heuremindec(twilight(delta,global_latitude),twilight2);

 hleversol2:=hleversol2-twilight2;
 hcouchersol2:=hcouchersol2+twilight2;

 heuredecmin(hleversol2,hleversol);
 heuredecmin(hcouchersol2,hcouchersol);

 writeln(fichier,'Debut Nuit Astronomique : ',hcouchersol,'<BR>');
 writeln(fichier,'Fin Nuit Astromique  : ',hleversol,'<BR>');
 writeln(fichier,'<BR>');

 writeln(fichier,'<table border="1" width="100%">');
 writeln(fichier,'<tr>');
 writeln(fichier,'<td>Messier</td>');
 writeln(fichier,'<td>NGC</td>');
 writeln(fichier,'<td>Constellation</td>');
 writeln(fichier,'<td>Type</td>');
 writeln(fichier,'<td>Sous-Type</td>');
 writeln(fichier,'<td>Alpha</td>');
 writeln(fichier,'<td>Delta deg.</td>');
 writeln(fichier,'<td>Magnitude</td>');
 writeln(fichier,'<td>Dimensions</td>');
 writeln(fichier,'<td>Distance</td>');
 writeln(fichier,'<td>Lever</td>');
 writeln(fichier,'<td>Meridien</td>');
 writeln(fichier,'<td>Coucher</td>');
 writeln(fichier,'<td>Marathon</td>');
 writeln(fichier,'</tr>');

 for n:=1 to 110 do
  begin
   if tri=2 then
    begin
     reset(fdat);
     while not eof(fdat) do
      begin
       read(fdat,vdat);
       if ordermara(vdat.num_messier)=n then seek(fdat,filesize(fdat));
      end;
     end
     else
     begin
      read(fdat,vdat);
     end;
   if 90-vdat.delta>global_latitude then
    begin
     levercoucher(true,vdat.ad,greenwich,global_latitude,global_longitude,vdat.delta,0,angsize,hlever);
     end
    else
     begin
      hlever:='CircumPo';
     end;

     meridien(vdat.ad,greenwich,global_longitude,hmeridien);
     if 90-vdat.delta>global_latitude then
      begin
       levercoucher(false,vdat.ad,greenwich,global_latitude,global_longitude,vdat.delta,0,-angsize,hcoucher);
      end
      else
      begin
       hcoucher:='CircumPol';
      end;

   writeln(fichier,'<tr>');
   writeln(fichier,'<td>', vdat.num_messier ,'</td>');
   writeln(fichier,'<td>', vdat.num_ngc ,'</td>');
   writeln(fichier,'<td>', vdat.constellation ,'</td>');
   writeln(fichier,'<td>', vdat.TypObj ,'</td>');
   writeln(fichier,'<td>', vdat.SType ,'</td>');
   writeln(fichier,'<td>', vdat.ad ,'</td>');
   writeln(fichier,'<td>', vdat.delta:6:1 ,'</td>');
   writeln(fichier,'<td>', vdat.magnitude:4:1 ,'</td>');
   writeln(fichier,'<td>', vdat.diametre ,'</td>');
   writeln(fichier,'<td>', vdat.distance:6:2 ,'</td>');

   writeln(fichier,'<td>',hlever ,'</td>');
   writeln(fichier,'<td>', hmeridien ,'</td>');
   writeln(fichier,'<td>', hcoucher ,'</td>');

   writeln(fichier,'<td>', ordermara(vdat.num_messier) ,'</td>');
   writeln(fichier,'</tr>');

    end;

    close(fdat);

    writeln(fichier,'</table>');

    writeln(fichier,'Definition des Types: <BR>');
    writeln(fichier,'   1  Open Cluster<BR>');
    writeln(fichier,'   2  Globular Cluster<BR>');
    writeln(fichier,'   3  Planetary Nebula<BR>');
    writeln(fichier,'   4  Diffuse Nebula<BR>');
    writeln(fichier,'   5  Spiral Galaxy<BR>');
    writeln(fichier,'   6  Elliptical Galaxy<BR>');
    writeln(fichier,'   7  Irregular Galaxy       (M82)<BR>');
    writeln(fichier,'   8  Lenticular (S0) Galaxy<BR>');
    writeln(fichier,'   8  Lenticular (S0) Galaxy<BR>');
    writeln(fichier,'   9  Supernova Remnant      (M1)<BR>');
    writeln(fichier,'   A  System of 4 stars or Asterism (M73)<BR>');
    writeln(fichier,'   B  Milky Way Patch        (M24)<BR>');
    writeln(fichier,'   C  Binary star            (M40)<BR>');


   writeln(fichier,'<p>Document calcule par ASTRONOM/F.GOUYAUD');
   writeln(fichier,'d''apres les donnees des objets messier de Hartmut Frommert,');
   writeln(fichier,'Chris Kronberg et Guy McArthur <a');
   writeln(fichier,'href="http://www.seds.org/messier" target="blank">http://www.seds.org/messier/');
   writeln(fichier,'</a></p>');

   writeln(fichier,'</body>');
   writeln(fichier,'</html>');
   closefile(fichier);

   MessageDlg('Fin de generation, cliquez sur OK pour voir le resultat',mtInformation, [mbOK], 0);
   if tri=1 then  OpenDocument('messier1.htm') { *Converti depuis ShellExecute* } else  OpenDocument('messier2.htm'); { *Converti depuis ShellExecute* }

  end;

Procedure NearStars;
var
 fdat 		: file of messier_cat;
 vdat		: messier_cat;
 fdat2		: file of bright_cat;
 vdat2		: bright_cat;
 fichier        : text;
 memo		: integer;
 resecart	: real;
 memoecart	: real;
begin

 (*ConversionBright; Pour generation fichier brightca.dat*)

 assign(fdat,'messicat.dat');
 reset(fdat);

 assign(fdat2,'brightca.dat');
 reset(fdat2);

 assign(fichier,'messier3.htm');
 rewrite(fichier);

 writeln(fichier,'<html>');
 writeln(fichier,'<head>');
 writeln(fichier,'<meta http-equiv="Content-Type"');
 writeln(fichier,'content="text/html; charset=iso-8859-1">');
 writeln(fichier,'<title>Etoiles Proches des Objets Messier</title>');
 writeln(fichier,'</head>');

 writeln(fichier,'<p align="center"><font size="2">ETOILE BRILLANTE LA PLUS');
 writeln(fichier,'PROCHE DE CHAQUE OBJET MESSIER</font></p>');

 writeln(fichier,'<p>Pour chaque objet Messier est calcule l''etoile de magnitude&lt;=3.0');
 writeln(fichier,'la plus proche et son ecart angulaire avec l''objet Messier.</p>');

 writeln(fichier,'<table border="1" width="100%">');
 writeln(fichier,'<tr>');
 writeln(fichier,'<td>Messier</td>');
 writeln(fichier,'<td>Alpha</td>');
 writeln(fichier,'<td>Delta</td>');
 writeln(fichier,'<td>Magnitude</td>');
 writeln(fichier,'<td>Etoile</td>');
 writeln(fichier,'<td>Alpha</td>');
 writeln(fichier,'<td>Delta</td>');
 writeln(fichier,'<td>Magnitude</td>');
 writeln(fichier,'<td>Nom Complet</td>');
 writeln(fichier,'<td>Ecart</td>');
 writeln(fichier,'</tr>');


 while not eof(fdat) do
  begin
   read(fdat,vdat);
   reset(fdat2);
   memoecart:=2000;
   while not eof(fdat2) do
    begin
     read(fdat2,vdat2);
     if vdat2.magnitude<=3 then
      begin
       ecart(vdat.ad,vdat.delta,vdat2.ad,vdat2.delta,resecart);
       if resecart<memoecart then
        begin
         memoecart:=resecart;
         memo:=vdat2.numero;
        end;
       end;
     end;

   reset(fdat2);
   seek(fdat2,memo-1);
   read(fdat2,vdat2);

   writeln(fichier,'<tr>');
   writeln(fichier,'<td>', vdat.num_messier ,'</td>');
   writeln(fichier,'<td>', vdat.ad ,'</td>');
   writeln(fichier,'<td>', vdat.delta:5:2 ,'</td>');
   writeln(fichier,'<td>', vdat.magnitude:4:1 ,'</td>');
   writeln(fichier,'<td>', vdat2.lettgrec,' ',vdat2.nomconst ,'</td>');
   writeln(fichier,'<td>', vdat2.ad ,'</td>');
   writeln(fichier,'<td>', vdat2.delta:5:2 ,'</td>');
   writeln(fichier,'<td>', vdat2.magnitude:4:1 ,'</td>');
   writeln(fichier,'<td>', vdat2.libelle,'</td>');
   writeln(fichier,'<td>', memoecart:5:2,'</td>');

   writeln(fichier,'</tr>');

  end;

 close(fdat);
 close(fdat2);

 writeln(fichier,'</table>');
 writeln(fichier,'<p>Document calcule avec ASTRONOM/F.GOUYAUD');
 writeln(fichier,'d''apres les donnees des objets messier de Hartmut Frommert,');
 writeln(fichier,'Chris Kronberg et Guy McArthur <a');
 writeln(fichier,'href="http://www.seds.org/messier" target="blank">http://www.seds.org/messier/');
 writeln(fichier,'</a></p>');


 writeln(fichier,'</body>');
 writeln(fichier,'</html>');
 closefile(fichier);

 MessageDlg('Fin de generation, cliquez sur OK pour voir le resultat',mtInformation, [mbOK], 0);
  OpenDocument('messier3.htm'); { *Converti depuis ShellExecute* }

end;





 end.
