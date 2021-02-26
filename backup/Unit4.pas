unit Unit4;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,AST_FIC, ExtCtrls,AST_GEN,ast_moon,AST_SUN,ast_plan,ast_dive;

type

  { TForm4 }

  TForm4 = class(TForm)
    Button2: TButton;
    Label12: TLabel;
    Touche10: TButton;
    Touche11: TButton;
    Touche12: TButton;
    Touche13: TButton;
    Touche14: TButton;
    Touche15: TButton;
    Touche16: TButton;
    Touche17: TButton;
    Touche7: TButton;
    ListBox1: TListBox;
    Label1: TLabel;
    Button1: TButton;
    Timer1: TTimer;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Touche8: TButton;
    Touche9: TButton;
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Touche10Click(Sender: TObject);
    procedure Touche11Click(Sender: TObject);
    procedure Touche12Click(Sender: TObject);
    procedure Touche13Click(Sender: TObject);
    procedure Touche14Click(Sender: TObject);
    procedure Touche15Click(Sender: TObject);
    procedure Touche16Click(Sender: TObject);
    procedure Touche17Click(Sender: TObject);
    procedure Touche7Click(Sender: TObject);
    procedure Touche8Click(Sender: TObject);
    procedure Touche9Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form4: TForm4;
  choix		: integer;
  planete       : byte;
  mode_messier  : boolean;
  memo_mode_messier : boolean;
implementation

{$R *.lfm}

procedure TForm4.FormCreate(Sender: TObject);
begin
 ListBox1.items.add('Soleil');
 ListBox1.items.add('Lune');
 ListBox1.items.add('Mercure');
 ListBox1.items.add('Venus');
 ListBox1.items.add('Mars');
 ListBox1.items.add('Jupiter');
 ListBox1.items.add('Saturne');
 ListBox1.items.add('Uranus');
 ListBox1.items.add('Neptune');
 ListBox1.items.add('Pluton');
 ListBox1.items.add('Objet Messier');
 ListBox1.items.add('Objet NGC');

 label1.visible:=false;
 Button1.visible:=false;
 timer1.enabled:=false;
 label11.visible:=false;

 label2.visible:=false;
 label3.visible:=false;
 label4.visible:=false;
 label5.visible:=false;
 label6.visible:=false;
 label7.visible:=false;
 label8.visible:=false;
 label9.visible:=false;
 label10.visible:=false;
 label11.visible:=false;

 label12.caption:='31';
 mode_messier:=true;
 memo_mode_messier:=true;
 end;

procedure TForm4.Button2Click(Sender: TObject);
begin
  {*Bouton valider*}
 label12.caption:='';
 label1.visible:=true;
 Button1.visible:=true;

end;


procedure TForm4.ListBox1Click(Sender: TObject);
begin
 label2.visible:=false;
 label3.visible:=false;
 label4.visible:=false;
 label5.visible:=false;
 label6.visible:=false;
 label7.visible:=false;
 label8.visible:=false;
 label9.visible:=false;
 label10.visible:=false;
 label11.visible:=false;
 label11.caption:='';
 if ListBox1.Selected[0] then
 begin
  label1.caption:='Soleil';
  choix:=2;
 end;
 if ListBox1.Selected[1] then
 begin
  label1.caption:='Lune';
  choix:=1;
 end;
if ListBox1.Selected[2] then
 begin
  label1.caption:='Mercure';
  planete:=1;
  choix:=3;
 end;
if ListBox1.Selected[3] then
 begin
  label1.caption:='Vénus';
   planete:=2;
   choix:=3;
 end;
if ListBox1.Selected[4] then
 begin
  label1.caption:='Mars';
   planete:=3;
   choix:=3;
 end;
if ListBox1.Selected[5] then
 begin
  label1.caption:='Jupiter';
   planete:=4;
   choix:=3;
 end;
if ListBox1.Selected[6] then
 begin
  label1.caption:='Saturne';
   planete:=5;
   choix:=3;
 end;
if ListBox1.Selected[7] then
 begin
  label1.caption:='Uranus';
   planete:=6;
   choix:=3;
 end;
if ListBox1.Selected[8] then
 begin
  label1.caption:='Neptune';
   planete:=7;
   choix:=3;
 end;
if ListBox1.Selected[9] then
 begin
  label1.caption:='Pluton';
   planete:=8;
   choix:=3;
 end;
if ListBox1.Selected[10] then
 begin
  label1.caption:='Messier';
  mode_messier:=true;
  if mode_messier<>memo_mode_messier then
   begin
     label12.caption:='31';
     memo_mode_messier:=true;
   end;
  label11.caption:= InputBox('Objet de Messier', 'Entrez le numéro', label12.caption );

  label11.visible:=true;
  choix:=4;
 end;
 if ListBox1.Selected[11] then
 begin
  label1.caption:='NGC';
  mode_messier:=false;
  if mode_messier<>memo_mode_messier then
   begin
     label12.caption:='6960';
     memo_mode_messier:=false;
   end;
  label11.caption:= InputBox('Objet NGC', 'Entrez le numéro', label12.caption);

  label11.visible:=true;
  choix:=5;
 end;

 label1.visible:=true;
 Button1.visible:=true;


 end;

procedure TForm4.Button1Click(Sender: TObject);
begin
  button1.visible:=false;
  timer1.enabled:=true;
end;

procedure TForm4.Timer1Timer(Sender: TObject);
var
 longitude2	: real;


 vdate	: str10;
 vheure,vheure2,alpha,anglehor2	: str8;
 delta	: real;
 chaine	: string;
 jjulien,jjulien0,alpha1,tsl1	: real;
 tslocal	: real;
 z		: real;
 hauteur	: real;
 interm		: real;
 lambda,beta,vpi: real;
 ch		: char;
 anglehor	: real;
 ecartheure	: integer;
 vr1,vr2,vr3,vr4,vr5,vr6,vr7: real;
 choix2		: integer;
 nummessier	: byte;
 numNGC         : integer;
 h              : integer;
 erreur         : integer;

begin

label2.visible:=true;
 label3.visible:=true;
 label4.visible:=true;
 label5.visible:=true;
 label6.visible:=true;
 label7.visible:=true;
 label8.visible:=true;
 label9.visible:=true;
 label10.visible:=true;
 label11.visible:=true;

   ecartheure:=-1*Global_DecalageTu;

   chaine:=FormatDateTime('dd/mm/yyyy hh:mm:ss',Now);
   vdate:=copy(chaine,1,10);
   vheure:=copy(chaine,12,8);
   label2.caption:=chaine;

   val(copy(vheure,1,2),h,erreur);
   h:=h+ecartheure;
   str(h:2,chaine);

   vheure:=chaine+copy(vheure,3,6);

   label3.caption:=vdate +' '+vheure;

   jjulien:=julien(vdate,vheure);

   str(jjulien:0:7,chaine)  ;
   label4.caption:=chaine;
   jjulien0:=julien(vdate,'00:00:00');

   tsg(jjulien0,vheure,interm);
   heuredecmin(interm,vheure2);

   chaine:='TSG = '+vheure2;
   label5.caption:=chaine;

   tsl(jjulien0,vheure,4.8,tslocal);
   heuredecmin(tslocal,vheure2);


   label6.caption:='TSL = '+vheure2;
   if choix=4 then val(label11.caption,nummessier,erreur);
   if choix=5 then val(label11.caption,numNGC,erreur);

   case choix of
    1 : coordlune(vdate,vheure,lambda,beta,vpi,delta,alpha);
    2 : calc_soleil(jjulien,alpha,delta,vr1,vr2,vr3,vr4,vr5,vr6,vr7);
    3 : orbites(jjulien,planete,vr1,vr2,vr3,vr4,delta,vr5,alpha);
    4 : renvoi_coordMessier(NumMessier,alpha,delta);
    5 : renvoi_coordNGC(NumNGC,alpha,delta);
   end;

   str(delta:5:2,chaine);

   label10.caption:='Coord Objet alpha=' + alpha + ' / delta= '+ chaine;
   Equatorial_to_Horizontal(jjulien0,vheure,0,alpha,global_latitude,global_longitude,delta,hauteur,z);   {Unité Ast_Gen}

   heuremindec(alpha,alpha1);
   heuremindec(vheure2,tsl1);
   anglehor:=-alpha1+tsl1;
   if anglehor<0 then anglehor:=anglehor+24;

   heuredecmin(anglehor,anglehor2);

   str(hauteur:5:1,chaine);
   label7.caption:='Hauteur = '+ chaine;

   str(z:5:1,chaine);
   label8.caption:='Azimut = '+ chaine ;


   label9.caption:='Angle Horaire : '+ anglehor2;


end;

procedure TForm4.Touche10Click(Sender: TObject);
begin
   label12.caption:=label12.Caption+'4';
end;

procedure TForm4.Touche11Click(Sender: TObject);
begin
   label12.caption:=label12.Caption+'5';
end;

procedure TForm4.Touche12Click(Sender: TObject);
begin
   label12.caption:=label12.Caption+'6';
end;

procedure TForm4.Touche13Click(Sender: TObject);
begin
   label12.caption:=label12.Caption+'1';
end;

procedure TForm4.Touche14Click(Sender: TObject);
begin
   label12.caption:=label12.Caption+'2';
end;

procedure TForm4.Touche15Click(Sender: TObject);
begin
   label12.caption:=label12.Caption+'3';
end;

procedure TForm4.Touche16Click(Sender: TObject);
begin
  label12.caption:=label12.Caption+'0';
end;

procedure TForm4.Touche17Click(Sender: TObject);
begin
  {*Bouton valider*}
 label12.caption:='';
 label1.visible:=true;
 Button1.visible:=true;
end;
procedure TForm4.Touche7Click(Sender: TObject);
begin
   label12.caption:=label12.Caption+'7';
end;

procedure TForm4.Touche8Click(Sender: TObject);
begin
   label12.caption:=label12.Caption+'8';
end;

procedure TForm4.Touche9Click(Sender: TObject);
begin
   label12.caption:=label12.Caption+'9';
end;

end.


