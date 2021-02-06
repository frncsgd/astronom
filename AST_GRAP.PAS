Unit Ast_Graph;
interface
uses ast_gen,
     ast_moon,
     ast_fic,
     graph,
     crt,
     menu,
     hardlst;

procedure graphiques;

implementation

const
 ratio_imp = 0.698;

type
point = array[1..2] of integer;
pointw= array[1..2] of word;

var
 maxx,maxy    : integer;
 carte,mode   : integer;
 o            : point;
 xmax,ymax    : word;

procedure initgraph1;
begin
 carte:=detect;
 initgraph(carte,mode,'');
end;

procedure initgraph2;
begin
 maxx:=getmaxx;
 maxy:=getmaxy;
 getaspectratio(xmax,ymax);
 o[1]:=round(int(maxx/2));
 o[2]:=round(int(maxy/2));
end;

procedure initgraph3;
begin
 carte:=5;
 mode:=3;
 initgraph(carte,mode,'');
end;
procedure demiellipsoide(pc:point;a,b:word;angle:real;oa,edition:boolean;var c:pointw);
var
 x1,x2,y1,y2,n          : integer;
 vx,vy,vx1,vx2,vy1,vy2  : real;
 alpha,alpha1,alpha2,r  : real;
 debut                  : boolean;
begin

debut:=true;

  for n:=-a to a do
  begin
   vx:=n;
   vy:=b*sqrt(1-sqr(vx/(a)));

   corriquad(vx,vy,alpha);

   if (alpha<>0) and (alpha<>180) then r:=vy/sinus(alpha) else r:=a;

   if oa then
    begin

     alpha1:=alpha+angle;
     vx1:=(ymax/xmax)*r*cosinus(alpha1);
     if edition then vx1:=vx1*ratio_imp;
     vy1:=r*sinus(alpha1);

     x1:=pc[1]+round(vx1);
     y1:=pc[2]+round(vy1);
     if debut then moveto(x1,y1) else lineto(x1,y1);
     debut:=false;

     if n=0 then
      begin
       c[1]:=x1;
       c[2]:=y1;
      end;

    end
    else
    begin

     alpha2:=alpha-angle;
     vx2:=(ymax/xmax)*r*cosinus(alpha2);
     if edition then vx2:=vx2*ratio_imp;
     vy2:=r*sinus(alpha2);

     x2:=pc[1]+round(vx2);
     y2:=pc[2]-round(vy2);
     if debut then moveto(x2,y2) else lineto(x2,y2);
     debut:=false;

     if n=0 then
      begin
       c[1]:=x2;
       c[2]:=y2;
      end;

    end;

  end;

end;

procedure ellipsoide(pc:point;a,b:word;angle:real);
var
 x1,x2,y1,y2,n              : integer;
 vx,vy,vx1,vx2,vy1,vy2      : real;
 alpha,alpha1,alpha2,r      : real;
begin
  for n:=(-a+1) to (a-1) do
  begin
   vx:=n;
   vy:=b*sqrt(1-sqr(n/a));

   if vx<>0 then corriquad(vx,vy,alpha);

   r:=vy/sinus(alpha);

   alpha1:=alpha+angle;

   vx1:=(ymax/xmax)*r*cosinus(alpha1);
   vy1:=r*sinus(alpha1);

   x1:=pc[1]+round(vx1);
   y1:=pc[2]+round(vy1);

   putpixel(x1,y1,15);

   alpha2:=alpha-angle;

   vx2:=(ymax/xmax)*r*cosinus(alpha2);
   vy2:=r*sinus(alpha2);

   x2:=pc[1]+round(vx2);
   y2:=pc[2]-round(vy2);

   putpixel(x2,y2,15);

  end;
end;

procedure graph_lune(k,zeta,s:real;edition:boolean);

var
 angle               : real;
 signe               : real;
 valeur,n            : integer;
 c,c1,c2             : pointw;
 rayon,rayon2        : word;
 a,b                 : real;
begin

 
 if maxx>maxy then rayon:=round(0.95*maxy/2)
              else rayon:=round(0.95*maxx/2);

 angle:=-s+zeta+90;
 range0a360(angle,angle);
 rayon2:=abs(round(rayon*(2*k-1)));
 signe:=round(rayon*(2*k-1))/rayon2;
 demiellipsoide(o,rayon,rayon,angle,true,edition,c1);
 if signe>0 then demiellipsoide(o,rayon,rayon2,angle,false,edition,c2)
            else demiellipsoide(o,rayon,rayon2,angle,true,edition,c2);

 for n:=1 to 2 do
  begin
   a:=c1[n];
   b:=c2[n];
   angle:=a+round((b-a)/2);
   c[n]:=round(angle);
  end;
 {if edition then floodfill(c[1],c[2],11) else floodfill(c[1],c[2],1);}

end;

procedure asp_lune;
var
 choix  : integer;
 option : tableau;
 edition: boolean;
 chaine : string;
 x,y    : integer;
 date   : str10;
 ok	: boolean;
 heure,alpha           : str8;
 lambda,beta,vpi,delta,zeta,k,jj,h,z,s : real;
begin
window(5,5,60,20);
cadre;
titre('Aspect Lune … une Date Donn‚e');
gotoxy(4,4);
write('Date ? ');
saisstr(false,false,false,10,'01/01/1980',chaine);
date:=chaine;
gotoxy(4,6);
write('Heure ? ');
saisstr(false,false,false,8,'00:00:00',chaine);
heure:=chaine;

gotoxy(4,8);
write('Latitude du lieu d''Observation  ? ');
saisreal(5,2,latitude,latitude);

gotoxy(4,10);
write('Longitude du lieu d''Observation ? ');
saisreal(8,3,longitude,longitude);

gotoxy(4,12);
write('Edition ? ');
option[1]:='NON';
option[2]:='OUI';
choixligne(2,option,choix);
case choix of
1 : edition:=false;
2 : edition:=true;
end;

coordlune(date,heure,lambda,beta,vpi,delta,alpha);
donphysique(date,heure,zeta,k);
jj:=julien(date,heure);
anglesdivers2(jj,heure,alpha,latitude,longitude,delta,h,z,s);

if edition then testimp(ok);

initgraph1;
initgraph2;
chaine:='Le '+date+' … '+heure;
settextstyle(0,0,1);
x:=maxx-216;
y:=maxy-9;
outtextxy(x,y,chaine);
graph_lune(k,zeta,s,edition);
if edition then  hardcopy(-2);
pauseclav;
closegraph;

end;

(*****************************************************************)
procedure anim_lune;
var
 ch         : char;
 carte,mode : integer;
 chaine : string;
 pas    : real;
 x,y    : integer;
 date   : str10;
 heure,alpha           : str8;
 lambda,beta,vpi,delta,zeta,k,jj,h,z,s : real;
 numpage : boolean;
begin
window(5,5,60,20);
cadre;
titre('D‚filement Phases de la Lune');
gotoxy(4,4);
write('Date D‚part ? ');
saisstr(false,false,false,10,'01/01/1980',chaine);
date:=chaine;
gotoxy(4,6);
write('Heure ? ');
saisstr(false,false,false,8,'00:00:00',chaine);
heure:=chaine;

gotoxy(4,8);
write('Latitude du lieu d''Observation  ? ');
saisreal(5,2,latitude,latitude);

gotoxy(4,10);
write('Longitude du lieu d''Observation ? ');
saisreal(8,3,longitude,longitude);

gotoxy(4,12);
write('Pas en Jours  ? ');
saisreal(5,1,1,pas);

initgraph3;
initgraph2;
numpage:=false;

while not keypressed do
begin
 coordlune(date,heure,lambda,beta,vpi,delta,alpha);
 donphysique(date,heure,zeta,k);
 jj:=julien(date,heure);
 anglesdivers2(jj,heure,alpha,latitude,longitude,delta,h,z,s);

 if not numpage then setactivepage(0) else setactivepage(1);
 clearviewport;

 chaine:='Le '+date+' … '+heure;
 settextstyle(0,0,1);
 x:=maxx-216;
 y:=maxy-9;
 outtextxy(x,y,chaine);
 graph_lune(k,zeta,s,false);

 if not numpage then setvisualpage(0) else setvisualpage(1);
 if numpage then numpage:=false else numpage:=true;

 jj:=jj+pas;
 invjulien(jj,date,heure);


end;

ch:=readkey;

closegraph;

end;

(*****************************************************************)


procedure graphiques;
var
 quitter : boolean;
 choix   : integer;
 option  : tableau;
begin
 quitter:=false;
 while not quitter do
  begin
   window(1,1,80,25);
   cadre;
   titre('EPHEMERIDES ASTRONOMIQUES - Graphiques');
   option[1]:='Aspect Lune';
   option[2]:='Phases de la Lune';
   option[3]:='Quitter';

   gotoxy(4,4);
   choixligne(3,option,choix);

   case choix of
    1 : asp_lune;
    2 : anim_lune;
    3 : quitter:=true;
   end;
  end;
end;

end.


end.

