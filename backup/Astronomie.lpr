program Astronomie;

{$MODE Delphi}

uses
  Forms, Interfaces,
  Unit1 in 'Unit1.pas' {Form1},
  Unit2 in 'Unit2.pas' {Form2},
  Unit3 in 'Unit3.pas' {Form3},
  Unit4 in 'Unit4.pas' {Form4},
  Ast_Gen in 'AST_GEN.PAS',
  Ast_Fic in 'AST_FIC.PAS',
  Ast_Sun in 'AST_SUN.PAS',
  Ast_Mess in 'AST_MESS.PAS',
  Ast_dive in 'ast_dive.pas',
  Ast_Moon in 'ast_moon.pas',
  Ast_plan in 'ast_plan.pas',
  Ast_Deep in 'AST_DEEP.PAS',
  Ast_Star in 'Ast_star.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TForm3, Form3);
  Application.CreateForm(TForm4, Form4);
  Application.Run;
//end.
