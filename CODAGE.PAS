PROGRAM Codage;     { Demo of the mouse unit }

USES dos, crt, mouse;

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
  CLRSCR;
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

       anglex:=nbtoursx+(its.column-10000)/10000;
       angley:=nbtoursy+(its.row-10000)/10000;

       gotoxy(4,11);
       write('Anglex = ',anglex:10:4);
       gotoxy(4,12);
       write('Angley = ',angley:10:4);


      until keypressed;
      ch:=readkey;

      CLRSCR;
    END
  ELSE
    WRITELN ('Mouse not present in system');
END.