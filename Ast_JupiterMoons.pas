Unit Ast_JupiterMoons;

{$MODE Delphi}

{
  Delphi code to calculate the position of Jupiter's moons and shadows.
  Original Javascript Copyright 2009, 2013 by Akkana Peck --
  please share and enjoy under the terms of the GPL v2 or later.

  Equations come from Jean Meeus, Astronomical Formulae for Calculators.

  Adapté de javascript vers pascal avec l'outil https://www.codeconvert.ai/javascript-to-pascal-converter
  Source du code Java : view-source:https://tetesenlair.net/jupiter.html
}

interface

uses AST_GEN,AST_PLAN,AST_SUN,SysUtils, Math, DateUtils,Ast_Fic;

type
  { object that has .x and .y }
  TXYCoord =  record
    x: Double;
    y: Double;
  end;

  { Data structure for moon positions and events }
  TMoonData = record
    moonx: Double;
    moony: Double;
    shadowx: Double;
    shadowy: Double;
    transit: Boolean;
    farside: Boolean;
    eclipse: Boolean;
    procedure Clear;
  end;

  TJupiter = class
  private
    const NUM_MOONS = 4;
    const p = pi/180;
    var
      curdate: TDateTime;        { javascript Date object equivalent }
      d: Double;                 { days since epoch, 1899 Dec 31 12h ET }
      
      { Angles of each of the Galilean satellites, in radians,
        expressed relative to each satellite's inferior conjunction: }
      moonAngles: array[0..NUM_MOONS-1] of Double;
      { And their distances from the planet: }
      moonDist: array[0..NUM_MOONS-1] of Double;

      { variables we may want later for moon calcs: }
      psi: Double;
      
      delta: Double; { Earth-Jupiter distance }
      De: Double;    { planetocentric ang. dist. of earth from jup. equator }
      G: Double;
      H: Double;

      { latitudes of systems I and II: }
      lambda1: Double;
      lambda2: Double;

    { Internal helpers }
    function angle(a: Double): Double;
    function oangle(a: Double): Double;
    function getJulianDate(d: TDateTime): Double;
    function daysBetween(d1, d2: TDateTime): Double;

  public
    constructor Create;
    destructor Destroy; override;
    function getDate: TDateTime;
    procedure setDate(initDate: TDateTime);
    function getMoonXYData(whichmoon: Integer): TMoonData;
    function getRedSpotXY(spot_in_deg: Double): TXYCoord;
    function getJovianPointX(long_in_deg: Double; systm: Integer): Double;
    property CurrentDate: TDateTime read curdate;
  end;

{ Global helper functions }
function dist(x, y: Double): Double;
function prettytime(tothrs: Double): string;
Procedure upcomingEvents(jup: TJupiter; date: TDateTime; tothrs: Double);
function endsWith(const str, suffix: string): Boolean;
function pluralize(num: Integer; const word: string): string;

{ Utility functions for cloning (mimicking JS logic) }
function clone(const source: TMoonData): TMoonData;

implementation

{ TXYCoord }

//constructor TXYCoord.Create(AX, AY: Double);
//begin
//  x := AX;
//  y := AY;
//end;

{ TMoonData }

procedure TMoonData.Clear;
begin
  moonx := 0.0;
  moony := 0.0;
  shadowx := 0.0;
  shadowy := 0.0;
  transit := False;
  farside := False;
  eclipse := False;
end;

{ TJupiter }

constructor TJupiter.Create;
var
  i: Integer;
begin
  inherited;
  for i := 0 to NUM_MOONS - 1 do
  begin
    moonAngles[i] := NaN;
    moonDist[i] := NaN;
  end;
end;
destructor TJupiter.Destroy;
begin
  inherited Destroy;
end;

function TJupiter.getDate: TDateTime;
begin
  Result := curdate;
end;

{
  Convert an angle (in radians) so that it's between 0 and 2*PI:
}
function TJupiter.angle(a: Double): Double;
begin
  if (a < 10000) then
    Exit(oangle(a));
    
  a := a - 2.0 * Pi * Int(a / 2.0 / Pi);
  if (a < 0) then
    a := a + 2.0 * Pi;
  Result := a;
end;

function TJupiter.oangle(a: Double): Double;
begin
  while (a > 2 * Pi) do
    a := a - 2.0 * Pi;
  while (a < 0) do
    a := a + 2.0 * Pi;
  Result := a;
end;

procedure TJupiter.setDate(initDate: TDateTime);
var
  V, M, N, J, A, B, K, R, r_vec, lambda,deltaT: Double;
  year,month : word;
  day : word;
  begin
  { Calculate the position of Jupiter's central meridian,
    and the corresponding moonAngle and moonDist arrays;
    and system I and system II longitudes;
    psi, the Jupiter's phase angle (always btw. -12 and 12 degrees):
    and De, the planetocentric angular distance of the earth
    from the equator of Jupiter. }

  { First, get the number of days since 1899 Dec 31 12h ET. }
  curdate := initDate;
  { JS: getJulianDate(initDate) - 2415020 }
  d := getJulianDate(initDate) - 2451545.0; { days since January 1.5, year 2000 }

  {Appliquer une correction DeltaT
  cf  https://github.com/cran/galisats/blob/master/R/galilean_satellites.R
  calculate delta-T (in seconds) for any year between 0 to 3000
  aussi dans https://github.com/soniakeys/meeus}
  
  DecodeDate(curdate, year, month,day);         //récupérer l'année courante
   
    if (year < 500) then
	 begin
        deltat := 10583.6 - 1014.41 * (year / 100) + 33.78311 * power(year / 100,2) - 5.952053 * power(year / 100,3) - 0.1798452 * power(year / 100,4) + 0.022174192 * power(year / 100,5) + 0.0090316521 * power(year / 100,6);
     end
	 else if (year < 1600) then
	  begin
        deltat := 1574.2 - 556.01 * ((year - 1000) / 100) + 71.23472 * power((year - 1000) / 100,2) + 0.319781 * power((year - 1000) / 100,3) -0.8503463 * power((year - 1000) / 100,4) - 0.005050998 * power((year - 1000) / 100,5) +0.0083572073 * power((year - 1000) / 100,6);
	 end
     else if (year < 1700) then
	 begin
        deltat := 120 - 0.9808 * (year - 1600) - 0.01532 * power(year - 1600,2) +power(year - 1600,3) / 7129;
    end
	 else if (year < 1800) then
	 begin
        deltat := 8.83 + 0.1603 * (year - 1700) - 0.0059285 * power(year - 1700,2) + 0.00013336 * power(year - 1700,3)- power(year - 1700,4) / 1174000;
    end
	 else if (year < 1860) then
	  begin
        deltat := 13.72 - 0.332447 * (year - 1800) + 0.0068612 * power(year - 1800,2) +0.0041116 * power(year - 1800,3) - 0.00037436 * power(year - 1800,4) +0.0000121272 * power(year - 1800,5) - 0.0000001699 * power(year - 1800,6) +0.000000000875 * power(year - 1800,7);
      end
	  else if (year < 1900) then
	  begin
        deltat := 7.62 + 0.5737 * (year - 1860) - 0.251754 * power(year - 1860,2) + 0.01680668 * power(year - 1860,3) - 0.0004473624 * power(year - 1860,4) + power(year - 1860,5) / 233174;
      end
	  else if (year < 1920) then
	  begin
        deltat := -2.79 + 1.494119 * (year - 1900) - 0.0598939 * power(year - 1900,2) +0.0061966 * power(year - 1900,3) - 0.000197 * power(year - 1900,4);
      end
	  else if (year < 1941) then
	  begin
        deltat :=  21.20 + 0.84493 * (year - 1920) - 0.076100 * power(year - 1920,2) +0.0020936 * power(year - 1920,3);
      end
	  else if (year < 1961) then
	  begin
        deltat :=  29.07 + 0.407 * (year - 1950) - power(year - 1950,2) / 233 +power(year - 1950,3) / 2547;
      end else if (year < 1986) then
	  begin
        deltat :=  45.45 + 1.067 * (year - 1975) - power(year - 1975,2) / 260 -power(year - 1975,3) / 718;
     end
	 else if (year < 2005) then
	 begin
        deltat :=  63.86 + 0.3345 *(year- 2000) - 0.060374 * power(year - 2000,2) +0.0017275 * power(year - 2000,3) + 0.000651814 * power(year - 2000,4) +0.00002373599 * power(year - 2000,5);
    end
	else if (year < 2050) then
	begin
        {75.074584000000002 secondes}
        deltat :=  62.92 + 0.32217 * (year - 2000) + 0.005589 * power(year - 2000,2);

    end 
	else if (year < 2150) then
	begin
        deltat :=  -20 + 32 * power((year - 1820) / 100,2) - 0.5628 * (2150 - year);
    end
	else 
	begin
        deltat :=  -20 + 32 * power((year - 1820) / 100,2);
    end;


   d:=d+(deltat/(3600*24));

{V := 172.74*p + .00111588*p*d
	M := 357.529*p + .9856003*p*d
	sV := math.Sin(V)
	N := 20.02*p + .0830853*p*d + .329*p*sV
	J := 66.115*p + .9025179*p*d - .329*p*sV}

  { Argument for the long-period term in the motion of Jupiter: }
  V := angle((172.74* + 0.00111588 * d) * p);
  
  { Mean anomalies of Earth and Jupiter: }
  M := angle((357.529 + 0.9856003 * d) * p);
  N := angle((20.02 + 0.0830853 * d + 0.329 * Sin(V)) * p);
  
  { Diff between the mean heliocentric longitudes of Earth & Jupiter: }
  J := angle((66.115 + 0.9025179 * d - 0.329 * Sin(V)) * p);
  
  { Equations of the center of Earth and Jupiter: }
  A := angle((1.915 * Sin(M) + 0.020 * Sin(2 * M)) * p);
  B := angle((5.555 * Sin(N) + 0.168 * Sin(2 * N)) * p);
  
  K := angle(J + A - B);
  
  { Distances are specified in AU: }
  { Radius vector of the earth: }
  R := 1.00014 - 0.01671 * Cos(M) - 0.00014 * Cos(2 * M);
  { Radius vector of Jupiter: }
  r_vec := 5.20872 - 0.25208 * Cos(N) - 0.00611 * Cos(2 * N);
  
  { Earth-Jupiter distance: }
  delta := Sqrt(r_vec * r_vec + R * R - 2 * r_vec * R * Cos(K));
  
  { Phase angle of Jupiter (always btw. -12 and 12 degrees): }
  psi := ArcSin(R / delta * Sin(K));
  
  { Longitude of system 1: }
  {lambda1 := angle((268.28 + 877.8169088 * (d - delta / 173)) * p + psi - B);}
  { Longitude of system 2: }
  {lambda2 := angle((290.28 + 870.1869088 * (d - delta / 173)) * p + psi - B);}

  { calculate the angles of each of the satellites: }
  moonAngles[0] := angle((163.8069 + 203.4058646 * (d - delta / 173)) * p + psi - B);
  moonAngles[1] := angle((358.414 + 101.2916335 * (d - delta / 173)) * p + psi - B);
  moonAngles[2] := angle((5.7176 + 50.234518 * (d - delta / 173)) * p + psi - B);
  moonAngles[3] := angle((224.8092 + 21.48798 * (d - delta / 173)) * p + psi - B);
  {u1 := 163.8069*p + 203.4058646*p*dd + ψ - B
	u2 := 358.414*p + 101.2916335*p*dd + ψ - B
	u3 := 5.7176*p + 50.234518*p*dd + ψ - B
	u4 := 224.8092*p + 21.48798*p*dd + ψ - B
	}
  
  { and the planetocentric angular distance of the earth
    from the equator of Jupiter: }
  lambda := angle((34.35 + 0.083091 * d + 0.329 * Sin(V)) * p + B);
  {λ := 34.35*p + .083091*p*d + .329*p*sV + B}

  De := ((3.12 * Sin(lambda + 42.8 * p)
         - 2.22 * Sin(psi) * Cos(lambda + 22.0 * p)
         - 1.3 * (r_vec - delta) / delta
         * Sin(lambda - 100.5 * p))
        * p);
		{DS := 3.12 * p * math.Sin(λ+42.8*p
		DE := DS - 2.22*p*math.Sin(ψ)*math.Cos(λ+22*p) -
		1.3*p*(r-Δ)/Δ*math.Sin(λ-100.5*p)}
  
  G := angle(( 331.18+ 50.310482 * (d - delta / 173)) * p);
  H := angle((87.45 + 21.569231 * (d - delta / 173)) * p);
  {G := 331.18*p + 50.310482*p*dd
   H := 87.45*p + 21.569231*p*dd}

  { Calculate the distances before any corrections are applied: }
  moonDist[0] := 5.9057 - 0.0244 * Cos(2 * (moonAngles[0] - moonAngles[1])); {r1 := 5.9057 - .0244*c212}
  moonDist[1] := 9.3966 - 0.0882 * Cos(2 * (moonAngles[1] - moonAngles[2])); {r2 := 9.3966 - .0882*c223}
  moonDist[2] := 14.9883 - 0.0216 * Cos(G); {r3 := 14.9883 - .0216*cG}
  moonDist[3] := 26.3627 - 0.1939 * Cos(H); {r4 := 26.3627 - .1939*cH}
      
  { apply some first-order correction terms to the angles: }
  moonAngles[0] := angle(moonAngles[0] + Sin(2 * (moonAngles[0] - moonAngles[1])) * 0.473 * p); {c1 := .473 * p * s212}
  moonAngles[1] := angle(moonAngles[1] + Sin(2 * (moonAngles[1] - moonAngles[2])) * 1.065 * p); {c2 := 1.065 * p * s223}
  moonAngles[2] := angle(moonAngles[2] + Sin(G) * 0.165 * p); {c3 := .165 * p * sG}
  moonAngles[3] := angle(moonAngles[3] + Sin(H) * 0.843 * p);{c4 := .843 * p * sH}
end;

function TJupiter.daysBetween(d1, d2: TDateTime): Double;
begin
  { In Delphi, subtracting TDateTime gives days as a Double }
  Result := d2 - d1;
end;

function TJupiter.getJulianDate(d: TDateTime): Double;
var
  epoch: TDateTime;
begin
  { JS: new Date("Jan 1 0:00 PST 1970") -> 1970-01-01 08:00:00 UTC }
  { We calculate JD based on this specific reference point }
  epoch := EncodeDateTime(1970, 1, 1, 8, 0, 0, 0);
  Result := (daysBetween(epoch, d) + 2440587.83333333333);
end;

{
  Returns the moon position in units of Jupiter radii.
  Also calculate the shadows, and whether the moon is eclipsed
  by Jupiter's shadow or transiting in front of Jupiter.
}
function TJupiter.getMoonXYData(whichmoon: Integer): TMoonData;
var
  r: Double;
  moondata: TMoonData;
  diskdist: Double;
  s: string; { Used for logging/debugging in original JS }
  xy: TXYCoord;
  atmoslop: Double;

  function getShadowXY(ang: Double): TXYCoord;
  var
    moonSunAngle: Double;
    res: TXYCoord;
  begin
    moonSunAngle := ang - psi;
    res.x := r * Sin(moonSunAngle);
    res.y := r * Cos(moonSunAngle) * Sin(De);
    Result := res;
  end;

begin
  r := moonDist[whichmoon];
  moondata.Clear;

  moondata.moonx := r * Sin(moonAngles[whichmoon]);
  moondata.moony := r * Cos(moonAngles[whichmoon]) * Sin(De);

  { Is the moon directly in front of or behind Jupiter's disk?
    Then this distance will be <= 1. }
  diskdist := dist(moondata.moonx, moondata.moony);

  s := 'moon ' + IntToStr(whichmoon);
  s := s + #10'Dist = ' + FloatToStr(r);
  s := s + #10'moonAngle = ' + FloatToStr(moonAngles[whichmoon]);
  s := s + ' = ' + FloatToStr(moonAngles[whichmoon] * 180.0 / Pi);
  s := s + #10'Jup phase angle = ' + FloatToStr(psi);
  s := s + ' = ' + FloatToStr(psi * 180.0 / Pi);

  moondata.shadowx := NaN;
  moondata.shadowy := NaN;

  { See whether the moon is on the near side of the planet: }
  if (moonAngles[whichmoon] < Pi * 0.5) or (moonAngles[whichmoon] > Pi * 1.5) then
  begin
    { Is it transiting? Leave a little slop, consider a moon
      transiting when it's just starting its transit. }
    if (diskdist < 0.9) then
      moondata.transit := True;

    { Since the moon is on the near side, check for shadows
      cast by the moon on the planet. }
    s := s + #10'Near side of the planet';
    moondata.farside := False;

    xy := getShadowXY(moonAngles[whichmoon]);
    moondata.shadowx := xy.x;
    moondata.shadowy := xy.y;

    { Is it hitting the planet? If not, set coords to NaN.
      Some day, ought to check for moons eclipsing other moons }
    if (moondata.shadowx < -1.0) or (moondata.shadowx > 1.0) then
    begin
      moondata.shadowx := NaN;
      moondata.shadowy := NaN;
    end;
  end
  { Is the moon blocked by the planet, so it's invisible? }
  else if (diskdist < 1.0) then
  begin
    moondata.farside := True;
    moondata.moonx := NaN;
    moondata.moony := NaN;
    s := s + #10'Blocked by the planet';
  end
  { Otherwise, it's on the far side.
    See if it's eclipsed by the planet's shadow. }
  else
  begin
    moondata.farside := True;
    s := s + #10'Far side of the planet';

    { See if a moon 180 degrees away from this moon's position,
      at the same distance, would cast a shadow on the planet.
      If so, the actual moon is eclipsed. }
    atmoslop := 0.9;
    xy := getShadowXY(angle(moonAngles[whichmoon] + Pi));
    moondata.eclipse := (dist(xy.x, xy.y) < atmoslop);
    
    s := s + #10'Actual moon at (' + FloatToStr(moondata.moonx) + ', ' + FloatToStr(moondata.moony) + ')';
    s := s + #10'Fake shadow at (' + FloatToStr(xy.x) + ', ' + FloatToStr(xy.y) + ')';
    s := s + #10'Dist from center = ' + FloatToStr(Sqrt(xy.x * xy.x + xy.y * xy.y));
    if (moondata.eclipse) then
      s := s + #10'Eclipse of moon ' + IntToStr(whichmoon) + '!';
  end;

  Result := moondata;
  end;

//CM( System I) =   156.84 + 877.8169147 * jd + correction
//CM( System II) =  181.62 + 870.1869147 * jd + correction
//CM( System III) = 138.41 + 870.4535567 * jd + correction *
//where 'jd' is the Julian Day, and the 'correction' is computed as follows:

//jup_mean = (jd - 2455636.938) * 360. / 4332.89709
//eqn_center = 5.55 * sin( jup_mean)
//angle = (jd - 2451870.628) * 360. / 398.884 - eqn_center
//correction = 11 * sin( angle)
//            + 5 * cos( angle)
//         - 1.25 * cos( jup_mean) - eqn_center

{
  The Great Red Spot, currently at longitude 61 in system II
}
function TJupiter.getRedSpotXY(spot_in_deg: Double): TXYCoord;
var
  spotlong: Double;
  coord: TXYCoord;
begin
  spotlong := angle(lambda2 - spot_in_deg * p);
  
  { See if the spot is visible: }
  if (spotlong > Pi * 0.5) and (spotlong < Pi * 1.5) then
  begin
    coord.x := NaN;
    coord.y := NaN;
  end
  else
  begin
    coord.x := Sin(spotlong);
    coord.y := 0.42; { completely random wild-assed guess }
  end;
  
  Result := coord;
end;

{
  You might also want to get the location of some arbitrary
  other position on the planet, e.g. the Great Northern Spot.
}
function TJupiter.getJovianPointX(long_in_deg: Double; systm: Integer): Double;
var
  lambda: Double;
  longInRad: Double;
begin
  if (systm = 1) then lambda := lambda1 else lambda := lambda2;
  longInRad := angle(lambda - long_in_deg * p);

  { See if the point is visible: }
  if (longInRad > Pi * 0.5) and (longInRad < Pi * 1.5) then
    Result := NaN
  else
    Result := Sin(longInRad);
end;

{ Global Functions }

function dist(x, y: Double): Double;
begin
  Result := Sqrt(Power(x, 2) + Power(y, 2));
end;

function prettytime(tothrs: Double): string;
var
  hrs, days: Integer;
  pt: string;
begin
  if (tothrs < 24) then
    pt := FloatToStr(tothrs) + ' heures'
  else
  begin
    hrs := Trunc(tothrs) mod 24;
    days := Trunc(tothrs) div 24;
    pt := IntToStr(days) + ' jour';
    if (days <> 1) then
      pt := pt + 's';

    if (hrs = 1) then
      pt := pt + ', 1 heure'
    else if (hrs > 0) then
      pt := pt + ', ' + IntToStr(hrs) + ' heures';
  end;
  Result := pt;
end;

{ 
  Deep clone equivalent for Delphi records. 
  In Delphi, record assignment is a copy.
}
function clone(const source: TMoonData): TMoonData;
begin
  Result := source;
end;

{
  Build a table of upcoming moon events for a given interval.
}
procedure upcomingEvents(jup: TJupiter; date: TDateTime; tothrs: Double);
var
  saveDate: TDateTime;
  moonnames: array[0..3] of string;
  d: TDateTime;
  lastmoondata: array[0..3] of TMoonData;
  moondata: TMoonData;
  mins, whichmoon: Integer;
  nshadows, ntransits: Integer;
  thisevent: string;
  hasLastData: array[0..3] of Boolean;
  fichier : TextFile; // Utilisation du type standard TextFile

  // Variables pour Ast_Plan / Ast_Gen
  vr1, vr2, vr3, vr4, vr5, vr6, vr7, delta, jj, deltaS: Double;
  alpha, alphaS: str8;
  vdate, vheure: string;
  IsVisible: Boolean;

const
  DateFormatChars = 'dd/mm/yyyy';
  TimeFormatChars = 'hh:nn:ss';
begin
  AssignFile(fichier, 'galileens.txt');
  Rewrite(fichier);

  saveDate := jup.getDate;

  try
    // Écriture de l'en-tête directement dans le fichier
    WriteLn(fichier, 'Le ', FormatDateTime('dd/mm/yyyy hh:nn', date),' !!Les heures sont en temps universel!!');
    WriteLn(fichier, 'Ephémérides des lunes de Jupiter pour les prochain(e)s : ', prettytime(tothrs), ':');
    WriteLn(fichier, '');

    moonnames[0] := 'Io';
    moonnames[1] := 'Europe';
    moonnames[2] := 'Ganymède';
    moonnames[3] := 'Callisto';

    for whichmoon := 0 to 3 do
      hasLastData[whichmoon] := False;

    // Boucle principale (optimisée en mémoire)
    for mins := -30 to Trunc(tothrs * 60) - 1 do
    begin
      d := IncMinute(date, mins);
      jup.setDate(d);

      thisevent := '';
      nshadows := 0;
      ntransits := 0;

      for whichmoon := 0 to 3 do
      begin
        moondata := jup.getMoonXYData(whichmoon);

        if hasLastData[whichmoon] then
        begin
          if not IsNaN(moondata.shadowx) then Inc(nshadows);
          if moondata.transit then Inc(ntransits);

          // Construction de la chaîne d'événement locale à cette minute
          if IsNaN(moondata.moonx) and not IsNaN(lastmoondata[whichmoon].moonx) then
            thisevent := thisevent + FormatDateTime('dd/mm hh:nn', d) + ': ' + moonnames[whichmoon] + ' disparaît' + sLineBreak
          else if not IsNaN(moondata.moonx) and IsNaN(lastmoondata[whichmoon].moonx) then
          begin
            if not moondata.eclipse then
              thisevent := thisevent + FormatDateTime('dd/mm hh:nn', d) + ': ' + moonnames[whichmoon] + ' réapparaît' + sLineBreak;
          end
          else if moondata.transit and not lastmoondata[whichmoon].transit then
            thisevent := thisevent + FormatDateTime('dd/mm hh:nn', d) + ': ' + moonnames[whichmoon] + ' : début du transit' + sLineBreak
          else if not moondata.transit and lastmoondata[whichmoon].transit then
            thisevent := thisevent + FormatDateTime('dd/mm hh:nn', d) + ': ' + moonnames[whichmoon] + ' : fin du transit' + sLineBreak
          else if moondata.eclipse and not lastmoondata[whichmoon].eclipse then
            thisevent := thisevent + FormatDateTime('dd/mm hh:nn', d) + ': ' + moonnames[whichmoon] + ' : début éclipse' + sLineBreak
          else if not moondata.eclipse and lastmoondata[whichmoon].eclipse then
            thisevent := thisevent + FormatDateTime('dd/mm hh:nn', d) + ': ' + moonnames[whichmoon] + ' : quitte l''éclipse' + sLineBreak;

          if IsNaN(moondata.shadowx) and not IsNaN(lastmoondata[whichmoon].shadowx) then
            thisevent := thisevent + FormatDateTime('dd/mm hh:nn', d) + ': ' + moonnames[whichmoon] + ' : l''ombre disparaît' + sLineBreak
          else if not IsNaN(moondata.shadowx) and IsNaN(lastmoondata[whichmoon].shadowx) then
            thisevent := thisevent + FormatDateTime('dd/mm hh:nn', d) + ': ' + moonnames[whichmoon] + ' : l''ombre apparaît' + sLineBreak;
        end;

        lastmoondata[whichmoon] := moondata;
        hasLastData[whichmoon] := True;
      end;

      // Vérification de visibilité uniquement si un événement a eu lieu (gain CPU)
      if thisevent <> '' then
      begin
        vdate := FormatDateTime(DateFormatChars, d);
        vheure := FormatDateTime(TimeFormatChars, d);
        jj := julien(vdate, vheure);

        orbites(jj, 4, vr1, vr2, vr3, vr4, delta, vr5, alpha);
        calc_soleil(jj, alphaS, deltaS, vr1, vr2, vr3, vr4, vr5, vr6, vr7);
        IsVisible := visible(jj, alpha, delta, alphaS, deltaS);

        if IsVisible then
        begin
          // Si plusieurs phénomènes simultanés, on l'écrit
          if (nshadows + ntransits > 1) then
            WriteLn(fichier, pluralize(ntransits, 'transit'), ', ', pluralize(nshadows, 'ombre'));

          // Écriture immédiate du bloc d'événements
          Write(fichier, thisevent);
        end;
      end;
    end;

  finally
    CloseFile(fichier);
  end;

  lastmoondata[0].clear;
  lastmoondata[1].clear;
  lastmoondata[2].clear;
  lastmoondata[3].clear;
  moondata.clear;

end;

function endsWith(const str, suffix: string): Boolean;
begin
  Result := (Length(str) >= Length(suffix)) and 
            (Copy(str, Length(str) - Length(suffix) + 1, Length(suffix)) = suffix);
end;

function pluralize(num: Integer; const word: string): string;
begin
  if (num = 1) then
    Result := '1 ' + word
  else if endsWith(word, 's') then
    Result := IntToStr(num) + ' ' + word + 'es'
  else
    Result := IntToStr(num) + ' ' + word + 's';
end;

end.

