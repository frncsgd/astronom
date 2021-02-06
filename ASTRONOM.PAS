Program Astronom;
Uses Menu,
     crt,
     graph,
     ast_fic,
     ast_sun,
     ast_moon,
     ast_star,
     ast_plan,
     ast_dive,
     ast_graph,
     ast_messier,
     ast_deep;

var
 quitter : boolean;
 option  : tableau;
 choix   : integer;
carte,mode :integer;

begin

    carte:=detect;
    if detect<>0 then
     begin
      initgraph(carte,mode,'');
      setbkcolor(12);
      setcolor(15);
      rectangle(0,0,getmaxx,getmaxy);
      setcolor(14);
      settextjustify(1,1);
      settextstyle(1,0,4);
      outtextxy(round(getmaxx/2),round(getmaxy/4),'Fran‡ois GOUYAUD');
      settextstyle(1,0,4);
      outtextxy(round(getmaxx/2),round(getmaxy/1.5),'EPHEMERIDES ASTRONOMIQUES');
      settextstyle(3,0,2);
      outtextxy(round(0.87*getmaxx),round(0.95*getmaxy),'F‚vrier 2003');
      delay(2000);
      closegraph;
     end;


    cadre;
    titre('EPHEMERIDES ASTRONOMIQUES');
    window(5,5,60,20);
    cadre;
    titre('Menu G‚n‚ral');

    gotoxy(4,4);
    write('Valeurs par d‚faut : ');

    gotoxy(4,6);
    write('Latitude en Degr‚s ? ');
    saisreal(6,2,45.38,latitude);

    gotoxy(4,8);
    write('Longitude en Degr‚s ? (+ vers Est)');
    saisreal(6,2,4.8,longitude);

    quitter:=false;

   while not quitter do
    begin
     cadre;
     titre('EPHEMERIDES ASTRONOMIQUES');
     window(5,4,60,22);
     cadre;
     titre('Menu G‚n‚ral');
     option[1]:='Soleil';
     option[2]:='Lune';
     option[3]:='PlanŠtes';
     option[4]:='Objets Messier';
     option[5]:='Etoiles';
     option[6]:='Eclipses';
     option[7]:='Graphismes';
     option[8]:='Divers';
     option[9]:='Annuaire';
     option[10]:='Ciel Profond';
     option[11]:='Quitter';

     gotoxy(4,16);
     write('D‚velopp‚ depuis 1991 par Fran‡ois GOUYAUD');
     gotoxy(4,17);
     write('email : fgouyaud@club-internet.fr');

     gotoxy(4,4);
     choixligne(11,option,choix);

     window(1,1,80,25);

     case choix of
      1  : soleil;
      3  : planetes;
      2  : lune;
      4  : messier;
      5  : etoiles;
      7  : graphiques;
      8  : divers;
      9  : annuaire;
      10 : deepsky;
      11 : quitter:=true;
     end;

    end;

    clrscr;

 end.