unit Unit3;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,AST_SUN,AST_GEN,AST_FIC,AST_MESS,ast_dive,ast_moon;

type
  TForm3 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Edit1: TEdit;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form3: TForm3;

implementation

uses Unit1;

{$R *.lfm}

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
procedure TForm3.Button1Click(Sender: TObject);
begin
  Form3.Hide;
  Form1.Show
end;

procedure TForm3.FormCreate(Sender: TObject);

var
 chaine : string;
begin
       edit1.text:=FormatDateTime('dd/mm/yyyy hh:mm:ss',Now);
 end;

procedure TForm3.Button2Click(Sender: TObject);

var
  chaine 	: string;
  date		: string [10];
  heure		: string [8];
  hmeridien     : string [8];
  hcoucher	: string [8];
  hleversol	: string [8];
  hcouchersol	: string [8];
  instant 	: real;
  greenwich	: real;
  hauteur	: real;
  z		: real;
  delta		: real;
  rayon		: real;
  t         	: real;
  longvraie 	: real;
  m         	: real;
  parallaxe	: real;
  angsize	: real;
  hmini		: integer;

 fdat : file of Messier_cat;
 vdat : messier_cat;
 n	: integer;
 tri	: integer;
 fichier : textfile;

 choix	: integer;
 hleversol2 : real;
 hcouchersol2 : real;
 twilight2	: real;
 alpha		: string [8];
 alphalune	: string [8];
 deltalune	: real;
 lambda		: real;
 a1,a2          : real;
 ecartlune	: real;

begin

date:=copy(edit1.text,1,10);
heure:=copy(edit1.text,12,8);
instant:=julien(date,'00:00:00');
tsg(instant,'00:00:00',greenwich);


hmini:=5;

 assignfile(fdat,'messicat.dat');
 reset(fdat);

 assignfile(fichier,'messhaz.htm');

 rewrite(fichier);

 writeln(fichier,'<html>');
 writeln(fichier,'<head>');
 writeln(fichier,'<meta http-equiv="Content-Type"');
 writeln(fichier,'content="text/html; charset=iso-8859-1">');
 writeln(fichier,'<title>Positions des Objets Messier</title>');
 writeln(fichier,'</head>');

  writeln(fichier,'<p align="center"><font size="2">Hauteur et Azimut des Objets');
  writeln(fichier,'de Messier le ',date,' - ',heure,' <BR> Hauteur mini : ',hmini, ' degres </font></p>');


  writeln(fichier,'<p align="center"><font size="2">Latitude ',global_latitude:5:2,'deg.');
  writeln(fichier,'Longitude ',global_longitude:5:2,'deg.</font></p>');


  writeln(fichier,'<p><font size="2">Informations Eclairage Soleil</font><br>');

 t:=julien(date,heure);
 calc_soleil(t,alpha,delta,rayon,longvraie,longvraie,longvraie,m,parallaxe,angsize);

 levercoucher(true,alpha,greenwich,global_latitude,global_longitude,delta,0,angsize,hleversol);
 levercoucher(false,alpha,greenwich,global_latitude,global_longitude,delta,0,-angsize,hcouchersol);


 writeln(fichier,'Nuit astronomique : +/-',twilight(delta,global_latitude),'<BR>');

 heuremindec(hleversol,hleversol2);
 heuremindec(hcouchersol,hcouchersol2);
 writeln(fichier,'Coucher : ',hcouchersol,'<BR>');
 writeln(fichier,'Lever  : ',hleversol,'<BR>');
 heuremindec(twilight(delta,global_latitude),twilight2);

 hleversol2:=hleversol2-twilight2;
 hcouchersol2:=hcouchersol2+twilight2;

 heuredecmin(hleversol2,hleversol);
 heuredecmin(hcouchersol2,hcouchersol);


 writeln(fichier,'Debut Nuit Astronomique : ',hcouchersol,'<BR>');
 writeln(fichier,'Fin Nuit Astromique : ',hleversol,'<BR>');
 writeln(fichier,'<BR>');

 coordlune(date,hcouchersol,a1,a2,lambda,deltalune,alphalune);
 writeln(Fichier,'Lune coeff=',lambda:3:2,' alpha=',alphalune,' et delta=',deltalune:6:2);
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
 writeln(fichier,'<td>Hauteur</td>');
 writeln(fichier,'<td>Azimut</td>');
 writeln(fichier,'<td>Marathon</td>');
 writeln(fichier,'<td>Ec.Lune</td>');
 writeln(fichier,'</tr>');

 for n:=1 to 110 do
  begin
   read(fdat,vdat);

   Equatorial_to_Horizontal(instant,heure,0,vdat.ad,global_latitude,global_longitude,vdat.delta,hauteur,z);

   {gotoxy(4,14);
   write('M',vdat.num_messier, 'Hauteur = ',hauteur:3:2,' Azimut=',z:3:2); }

   if hauteur>=hmini then
    begin
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

     writeln(fichier,'<td>',hauteur:3:2 ,'</td>');
     writeln(fichier,'<td>', z:3:2 ,'</td>');
     writeln(fichier,'<td>', ordermara(vdat.num_messier) ,'</td>');
     ecart(vdat.ad,vdat.delta,alphalune,deltalune,ecartlune);
     writeln(fichier,'<td>',ecartlune:5:1,'</td>');
     writeln(fichier,'</tr>');
    end;
   end;

  closefile(fdat);

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


   writeln(fichier,'<p>Document calcule et prepare par <a');
   writeln(fichier,'href="mailto:frangou2000@yahoo.fr">Francois GOUYAUD</a>');
   writeln(fichier,'d''apres les donnees des objets messier de Hartmut Frommert,');
   writeln(fichier,'Chris Kronberg et Guy McArthur <a');
   writeln(fichier,'href="http://www.seds.org/messier" target="blank">http://www.seds.org/messier/');
   writeln(fichier,'</a></p>');

   writeln(fichier,'</body>');
   writeln(fichier,'</html>');
   closefile(fichier);
   MessageDlg('Fin de génération, cliquer sur OK pour voir le résultat',mtInformation, [mbOK], 0);
    OpenDocument('messhaz.htm'); { *Converti depuis ShellExecute* }
   end;


end.
