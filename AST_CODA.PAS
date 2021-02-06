Unit Ast_Codage;
Interface
USES dos, crt, menu,mouse;



Procedure codage;

Implementation
   var
     decalaz	: real;
     decalh	: real;
     choix	: integer;

Procedure Codage2;

CONST hardware = 1;                                  { cursor types }
      software = 0;
      left     = 0;                                 { mouse buttons }
      right    = 1;

VAR  theMouse : resetRec;                         { for mouse fcn 0 }
     its      : locRec;                       { for mouse inquiries }
     col, row : integer;
     input    : string [80];
     ch		: char;
     nbtoursx	: double;
     nbtoursy	: double;
     anglex	: double;
     angley	: double;

BEGIN
  cadre;
  titre('Calibrage Souris');

  nbtoursx:=0;
  nbtoursy:=0;
  mReset (theMouse);                         { initialize the mouse }
  IF theMouse.exists THEN               { and make sure we have one }
    begin
      mTextCursor (software, $0000, $0718);        { set s/w cursor }
      mShow;                                       { turn cursor on }
      mcolrange;
      mrowrange;
      mmoveto(10000,10000);

      repeat
       mpos(its);
       if its.column>20000 then
        begin
         nbtoursx:=nbtoursx+1;
         its.column:=its.column-10000;
         mmoveto(its.column,its.row);
        end;
       if its.row>20000 then
        begin
         nbtoursy:=nbtoursy+1;
         its.row:=its.row-10000;
         mmoveto(its.column,its.row);
        end;
       if its.column<10000 then
        begin
         nbtoursx:=nbtoursx-1;
         its.column:=its.column+10000;
         mmoveto(its.column,its.row);
        end;
       if its.row<10000 then
        begin
         nbtoursy:=nbtoursy-1;
         its.row:=its.row+10000;
         mmoveto(its.column,its.row);
        end;
       mpos(its);

       gotoxy(4,10);
       {write(nbtoursx:3:0,' ',(its.column-10000):5,'/ ',nbtoursy:3:0,' ',its.row-10000:5);}

       anglex:=10000*nbtoursx+(its.column-10000);
       angley:=10000*nbtoursy+(its.row-10000);

       gotoxy(4,11);
       write('Pas x = ',anglex:10:0);
       gotoxy(4,12);
       write('Pas y = ',angley:10:0);

       if (choix=3) and (decalaz<>0) and (decalh<>0) then
        begin
         gotoxy(4,4);
         write('Azimut : ',anglex/decalaz:6:2,' ø');
         gotoxy(4,5);
         write('Hauteur: ',angley/decalh:6:2,' ø');
        end;
      until keypressed;

      if choix=1 then decalaz:=anglex/360;
      if choix=2 then decalh:=angley/360;



      ch:=readkey;

      CLRSCR;
    END
  ELSE
    WRITELN ('Mouse not present in system');
 end;

procedure codage;
var
 quitter	: boolean;
 option		: tableau;
begin

 decalaz:=0;
 decalh:=0;

 quitter:=false;
 while not quitter do
  begin
   cadre;
   titre('Codeur Souris');
   gotoxy(4,4);
   option[1]:='Calibrage en azimut';
   option[2]:='Calibrage en hauteur';
   option[3]:='Affichage d''angle';
   option[4]:='Quitter';

   choixligne(4,option,choix);

   case choix of
    1,2,3 : codage2;
    4 : quitter:=true;
   end;

  end;
 end;
END.