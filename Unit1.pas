unit Unit1;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, StdCtrls, Menus, Grids, Calendar,ExtCtrls,
  Unit2,Unit3,Unit4,AST_MESS,AST_FIC, AST_SUN,Ast_Moon,Ast_Plan,AST_DEEP,Ast_star;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    tata: TMenuItem;
    Menu21: TMenuItem;
    Edit1: TEdit;
    smenu211: TMenuItem;
    smenu1: TMenuItem;
    Lune1: TMenuItem;
    Plantes1: TMenuItem;
    ObjetsMessier1: TMenuItem;
    CielProfond1: TMenuItem;
    EditionPB0L01: TMenuItem;
    EditionRotationsSynodiques1: TMenuItem;
    EditionParallaxeset12diamtres1: TMenuItem;
    Label2: TLabel;
    Edit2: TEdit;
    Label3: TLabel;
    Edit3: TEdit;
    Etoilesvoisines1: TMenuItem;
    Visibilitpourunedate1: TMenuItem;
    Hauteuretazimuthuninstant1: TMenuItem;
    Timer1: TTimer;
    Label5: TLabel;
    Divers: TMenuItem;
    Tempsrel1: TMenuItem;
    PositionLune1: TMenuItem;
    PhasesLune1: TMenuItem;
    EditionEphmrides1: TMenuItem;
    EditionSynthseAnnuelle1: TMenuItem;
    DonnesPhysiques1: TMenuItem;
    ElmentsdesOrbites1: TMenuItem;
    Ephmrides1: TMenuItem;
    LongitudesHliocentriques1: TMenuItem;
    HauteurAzimutpouruninstant1: TMenuItem;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure tataClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure smenu211Click(Sender: TObject);
    procedure Hauteuretazimuthuninstant1Click(Sender: TObject);
    procedure ConversionMessierdat1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Tempsrel1Click(Sender: TObject);
    procedure smenu1Click(Sender: TObject);
    procedure EditionPB0L01Click(Sender: TObject);
    procedure EditionRotationsSynodiques1Click(Sender: TObject);
    procedure EditionParallaxeset12diamtres1Click(Sender: TObject);
    procedure PositionLune1Click(Sender: TObject);
    procedure PhasesLune1Click(Sender: TObject);
    procedure EditionEphmrides1Click(Sender: TObject);
    procedure EditionSynthseAnnuelle1Click(Sender: TObject);
    procedure DonnesPhysiques1Click(Sender: TObject);
    procedure ElmentsdesOrbites1Click(Sender: TObject);
    procedure Ephmrides1Click(Sender: TObject);
    procedure LongitudesHliocentriques1Click(Sender: TObject);
    procedure Conversionfichier1Click(Sender: TObject);
    procedure HauteurAzimutpouruninstant1Click(Sender: TObject);
    procedure Visibilitpourunedate1Click(Sender: TObject);
    procedure Etoilesvoisines1Click(Sender: TObject);
    procedure Conversion(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.Button1Click(Sender: TObject);
var
 erreur : integer;
 fdat : file of site_info;
 vdat : site_info;
begin

 val(edit1.text,global_latitude,erreur);
 val(edit2.text,global_longitude,erreur);
 val(edit3.text,Global_DecalageTu,erreur);

 assignfile(fdat,'site.dat');
 rewrite(fdat);
   vdat.latitude:=global_latitude;
   vdat.longitude:=global_longitude;
   vdat.decalagehor:=Global_DecalageTu;
   write(fdat,vdat);
   closefile(fdat);

end;

procedure TForm1.Button2Click(Sender: TObject);
begin
 MessageDlg('au revoir',mtInformation, [mbOK], 0);

 close;
end;

procedure TForm1.MenuItem1Click(Sender: TObject);
begin
  conversionMessier;
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
begin
  conversiondeep;
end;

procedure TForm1.tataClick(Sender: TObject);
begin
 MessageDlg('au revoir',mtInformation, [mbOK], 0);
 close;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
 fdat : file of site_info;
 vdat : site_info;
 chaine : string;
begin

 assignfile(fdat,'site.dat');
 try
 reset(fdat);
  except
   rewrite(fdat);
   vdat.latitude:=45.374076;
   vdat.longitude:=4.790087;
   vdat.decalagehor:=2;
   write(fdat,vdat);
   closefile(fdat);
   reset(fdat);
   end;
  read(fdat,vdat);
 global_latitude:=vdat.latitude;
 global_longitude:=vdat.longitude;
 Global_DecalageTu:=vdat.decalagehor;

 str(vdat.latitude:2:2,chaine);
 edit1.text:=chaine;
 str(vdat.longitude:2:2,chaine);
 edit2.text:=chaine;
 str(vdat.decalagehor,chaine);
 edit3.text:=chaine;
 closefile(fdat);

 label5.caption:=DateTimeToStr(Now);
 Button1.visible:=false;

 end;

procedure TForm1.smenu211Click(Sender: TObject);
begin
  {Form1.Hide;}
  Unit2.Form2.Show
end;

procedure TForm1.Hauteuretazimuthuninstant1Click(Sender: TObject);
begin
Unit3.Form3.Show
end;

procedure TForm1.ConversionMessierdat1Click(Sender: TObject);
begin
 conversionmessier;
 MessageDlg('Conversion OK',mtInformation, [mbOK], 0);

end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
 label5.caption:=FormatDateTime('dd/mm/yyyy hh:mm:ss',Now);
end;

procedure TForm1.Tempsrel1Click(Sender: TObject);
begin
Unit4.Form4.Show
end;

procedure TForm1.smenu1Click(Sender: TObject);
var
 inputstring : string;
 mois        : integer;
 annee       : integer;
 erreur      : integer;
 annee_ch    : str10;
 mois_ch     : string[2];
begin

 annee_ch:=copy(FormatDateTime('dd/mm/yyyy hh:mm:ss',Now),7,4);
 mois_ch:=copy(FormatDateTime('dd/mm/yyyy hh:mm:ss',Now),4,2);

 InputString:= InputBox('Ephémérides mensuelles', 'Saisir année', annee_ch);
 val(inputstring,annee,erreur);
 InputString:= InputBox('Ephémérides mensuelles', 'Saisir mois (mettre 0 si année complète)', mois_ch);
 val(inputstring,mois,erreur);
 if mois<>0 then
  begin
   edit_soleil(annee,mois,global_latitude,global_longitude)
  end
 else
 begin
   mois:=1;
   while mois<=12 do
begin
  edit_soleil(annee,mois,global_latitude,global_longitude);
  mois:=mois+1;
  end;

  end;
end;

procedure TForm1.EditionPB0L01Click(Sender: TObject);
begin
 edition_pbl;;
end;

procedure TForm1.EditionRotationsSynodiques1Click(Sender: TObject);
var
 inputstring : string;
 annee       : word;
 erreur      : integer;
 annee_st    : str10;
begin
 annee_st:=copy(FormatDateTime('dd/mm/yyyy hh:mm:ss',Now),7,4);
 InputString:= InputBox('Rotation Synodiques', 'Saisir année', annee_st);
 val(inputstring,annee,erreur);
 edit_crn(annee);
end;

procedure TForm1.EditionParallaxeset12diamtres1Click(Sender: TObject);
var
 chaine      : string;
 annee       : word;
 pas         : word;
 annee_st    : string[4];
 erreur      : integer;
 begin
  annee_st:=copy(FormatDateTime('dd/mm/yyyy hh:mm:ss',Now),7,4);

chaine:= InputBox('Parallaxe', 'Saisir année', annee_st);
val(chaine,annee,erreur);


chaine:=InputBox('Parallaxe', 'Pas en jours', '1');
val(chaine,pas,erreur);

edit_parallaxe(annee,pas);
end;

procedure TForm1.PositionLune1Click(Sender: TObject);
begin
 pos_lune;
end;

procedure TForm1.PhasesLune1Click(Sender: TObject);
begin
 phases;
end;

procedure TForm1.EditionEphmrides1Click(Sender: TObject);
begin
edition_lune;
end;

procedure TForm1.EditionSynthseAnnuelle1Click(Sender: TObject);
var
 chaine      : string;
 annee       : word;
 pas         : word;
 erreur      : integer;
 annee_st                 : str10;
 begin

  annee_st:=copy(FormatDateTime('dd/mm/yyyy hh:mm:ss',Now),7,4);

chaine:= InputBox('Parallaxe', 'Saisir année',annee_st);
val(chaine,annee,erreur);
 edit_synthese(annee);
end;

procedure TForm1.DonnesPhysiques1Click(Sender: TObject);
begin
 physiques;
end;

procedure TForm1.ElmentsdesOrbites1Click(Sender: TObject);
begin
elements;

end;

procedure TForm1.Ephmrides1Click(Sender: TObject);
begin
 edition_planete;
 
end;

procedure TForm1.LongitudesHliocentriques1Click(Sender: TObject);
begin
  edition_longhelio;
end;

procedure TForm1.Conversionfichier1Click(Sender: TObject);
begin
ConversionDeep;
end;

procedure TForm1.HauteurAzimutpouruninstant1Click(Sender: TObject);
begin
Hautaz;
end;

procedure TForm1.Visibilitpourunedate1Click(Sender: TObject);
begin
 Messier2;
end;

procedure TForm1.Etoilesvoisines1Click(Sender: TObject);
begin
 NearStars;
end;

procedure TForm1.Conversion(Sender: TObject);
begin
   ConversionBright;
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
 button1.visible:=true;
end;

procedure TForm1.Edit2Change(Sender: TObject);
begin
button1.visible:=true;
end;

procedure TForm1.Edit3Change(Sender: TObject);
begin
button1.visible:=true;
end;

end.
