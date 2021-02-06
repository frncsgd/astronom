
Unit AST_FIC;

{$MODE Delphi}

Interface

type str8  = string[8];
     str10 = string[10];

 
 Type Site_Info = record
  latitude    : real;
  longitude   : real;
  decalagehor : integer;
 end;

 Type Bright_cat = record
  Numero	: integer;
  ad		: string[8];
  delta		: real;
  magnitude	: real;
  magnitude_abs : real;
  typestar	: string[2];
  numconst	: integer;
  lettgrec	: string[4];
  nomconst	: string[3];
  libelle       : string[40];
 end;

Type ficobj = record
 Nom   		: string[10];
 Alpha 		: string[8];
 delta 		: real;
 magnitude	: real;
 typeGal	: string[4];
 commentaire	: string[20];
 constellation  : string[20];
end;

var
  Global_Latitude	: real;
  Global_longitude	: real;
  Global_DecalageTu     : integer; 


implementation

end.
