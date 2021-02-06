Unit Ast_star;

{$MODE Delphi}

interface
uses
     LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,AST_FIC,
     AST_GEN,
     AST_SUN,
     ast_moon,
     ast_plan;

Procedure ConversionBright;

Implementation

Procedure ConversionBright;
var
 n	: integer;
 fdat : file of Bright_cat;
 vdat : Bright_cat;
 ftext: text;
 chaine	: string;
 erreur	: integer;
 valeur	: real;
 chaine2: string;
 chaine3: string;
begin
 assign(fdat,'brightca.dat');
 rewrite(fdat);
 assign(ftext,'bright.dat');
 reset(ftext);
 n:=1;
 while not eof(ftext) do
  begin
   readln(ftext,chaine);
   with vdat do
    begin
     numero:=n;
     val(copy(chaine,1,8),valeur,erreur);
     heuredecmin(valeur,ad);
     val(copy(chaine,10,8),delta,erreur);
     val(copy(chaine,19,5),Magnitude,erreur);
     val(copy(chaine,25,5),Magnitude_abs,erreur);
     TypeStar:=copy(chaine,31,2);
     val(copy(chaine,34,3),numconst,erreur);
     lettgrec:=copy(chaine,38,4);
     nomconst:=copy(chaine,43,3);
     libelle:=copy(chaine,47,25);
     write(fdat,vdat);
     inc(n);
    end;
    end;

  close(fdat);
  closefile(ftext);

  MessageDlg('Conversion OK Etoiles',mtInformation, [mbOK], 0);


end;

 end.
