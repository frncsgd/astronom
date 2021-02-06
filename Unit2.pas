unit Unit2;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,AST_SUN,AST_GEN;

type
  TForm2 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Edit1: TEdit;
    Button2: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form2: TForm2;

implementation

uses Unit1;

{$R *.lfm}

procedure TForm2.Button1Click(Sender: TObject);
begin
  Form2.Hide;
  Form1.Show
end;

procedure TForm2.FormCreate(Sender: TObject);

var
 chaine : string;
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

 edit1.text:=FormatDateTime('dd/mm/yyyy hh:mm:ss',Now);
 end;

procedure TForm2.Button2Click(Sender: TObject);
var
 heure,alpha                 : string[8];
 date                        : string[10];
 delta,rayon,p,b0,l0,num_crn,parallaxe,angsize,secondes : real;
 chaine                      : string;
 degres,minutes              : word;

begin

date:=copy(edit1.text,1,10);
heure:=copy(edit1.text,12,8);
chap15(date,heure,alpha,delta,rayon,p,b0,l0,parallaxe,angsize);
num_crn:=numero_crn(date,heure);
angsize:=angsize/2;
conversion_angle(angsize,degres,minutes,secondes);

label2.visible:=true;
label3.visible:=true;
label4.visible:=true;
label5.visible:=true;
label6.visible:=true;
label7.visible:=true;
label8.visible:=true;
label9.visible:=true;
label10.visible:=true;


label2.caption:= 'Alpha = ' + alpha;

str(delta:8:2,chaine);
label3.caption:='Delta = ' + chaine;

str(rayon:8:5,chaine);
label4.caption:='Rayon = '+ chaine + ' UA ';

str(P:5:2,chaine);
label5.caption:='P = ' + chaine +' °';

str(B0:5:2,chaine);
label6.caption:='b0 = ' + chaine +' °';

str(L0:6:2,chaine);
label7.caption:='l0 = ' + chaine +' °';

str(num_crn:5:1,chaine);
label8.caption:='CRN = ' + chaine;

str(3600*parallaxe:5:2,chaine);
label9.caption:='Parall= '+ chaine +' secondes';

str(minutes:2,chaine);
label10.caption:='Demi Diam.App.= ' + chaine + ' min.' ;
str(secondes:5:2,chaine);
label10.caption:=label10.caption+chaine+'"';

end;


end.
