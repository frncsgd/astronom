Unit Ast_JupiterMoons;

{
  Delphi code to calculate the position of Jupiter's moons and shadows.
  Original Javascript Copyright 2009, 2013 by Akkana Peck --
  please share and enjoy under the terms of the GPL v2 or later.

  Equations come from Jean Meeus, Astronomical Formulae for Calculators.

  Adapté de javascript vers pascal avec l'outil https://www.codeconvert.ai/javascript-to-pascal-converter
  Source du code Java : view-source:https://tetesenlair.net/jupiter.html
}

interface

uses SysUtils, Math, DateUtils;

type
  { object that has .x and .y }
  TXYCoord = record
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
function upcomingEvents(jup: TJupiter; date: TDateTime; tothrs: Double): string;
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
  V, M, N, J, A, B, K, R, r_vec, lambda: Double;
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
  d := getJulianDate(initDate) - 2415020; { days since 1899 Dec 31 12h ET }

  { Argument for the long-period term in the motion of Jupiter: }
  V := angle((134.63 + 0.00111587 * d) * Pi / 180);
  
  { Mean anomalies of Earth and Jupiter: }
  M := angle((358.476 + 0.9856003 * d) * Pi / 180);
  N := angle((225.328 + 0.0830853 * d + 0.33 * Sin(V)) * Pi / 180);
  
  { Diff between the mean heliocentric longitudes of Earth & Jupiter: }
  J := angle((221.647 + 0.9025179 * d - 0.33 * Sin(V)) * Pi / 180);
  
  { Equations of the center of Earth and Jupiter: }
  A := angle((1.916 * Sin(M) + 0.020 * Sin(2 * M)) * Pi / 180);
  B := angle((5.552 * Sin(N) + 0.167 * Sin(2 * N)) * Pi / 180);
  
  K := angle(J + A - B);
  
  { Distances are specified in AU: }
  { Radius vector of the earth: }
  R := 1.00014 - 0.01672 * Cos(M) - 0.00014 * Cos(2 * M);
  { Radius vector of Jupiter: }
  r_vec := 5.20867 - 0.25192 * Cos(N) - 0.00610 * Cos(2 * N);
  
  { Earth-Jupiter distance: }
  delta := Sqrt(r_vec * r_vec + R * R - 2 * r_vec * R * Cos(K));
  
  { Phase angle of Jupiter (always btw. -12 and 12 degrees): }
  psi := ArcSin(R / delta * Sin(K));
  
  { Longitude of system 1: }
  lambda1 := angle((268.28 + 877.8169088 * (d - delta / 173)) * Pi / 180 + psi - B);
  { Longitude of system 2: }
  lambda2 := angle((290.28 + 870.1869088 * (d - delta / 173)) * Pi / 180 + psi - B);

  { calculate the angles of each of the satellites: }
  moonAngles[0] := angle((84.5506 + 203.4058630 * (d - delta / 173)) * Pi / 180 + psi - B);
  moonAngles[1] := angle((41.5015 + 101.2916323 * (d - delta / 173)) * Pi / 180 + psi - B);
  moonAngles[2] := angle((109.9770 + 50.2345169 * (d - delta / 173)) * Pi / 180 + psi - B);
  moonAngles[3] := oangle((176.3586 + 21.4879802 * (d - delta / 173)) * Pi / 180 + psi - B);
  
  { and the planetocentric angular distance of the earth
    from the equator of Jupiter: }
  lambda := angle((238.05 + 0.083091 * d + 0.33 * Sin(V)) * Pi / 180 + B);

  De := ((3.07 * Sin(lambda + 44.5 * Pi / 180)
         - 2.15 * Sin(psi) * Cos(lambda - 24.0 * Pi / 180)
         - 1.31 * (r_vec - delta) / delta
         * Sin(lambda - 99.4 * Pi / 180))
        * Pi / 180);
  
  G := angle((187.3 + 50.310674 * (d - delta / 173)) * Pi / 180);
  H := angle((311.1 + 21.569229 * (d - delta / 173)) * Pi / 180);

  { Calculate the distances before any corrections are applied: }
  moonDist[0] := 5.9061 - 0.0244 * Cos(2 * (moonAngles[0] - moonAngles[1]));
  moonDist[1] := 9.3972 - 0.0889 * Cos(2 * (moonAngles[1] - moonAngles[2]));
  moonDist[2] := 14.9894 - 0.0227 * Cos(G);
  moonDist[3] := 26.3649 - 0.1944 * Cos(H);
  
  { apply some first-order correction terms to the angles: }
  moonAngles[0] := angle(moonAngles[0] + Sin(2 * (moonAngles[0] - moonAngles[1])) * 0.472 * Pi / 180);
  moonAngles[1] := angle(moonAngles[1] + Sin(2 * (moonAngles[1] - moonAngles[2])) * 1.073 * Pi / 180);
  moonAngles[2] := angle(moonAngles[2] + Sin(G) * 0.174 * Pi / 180);
  moonAngles[3] := angle(moonAngles[3] + Sin(H) * 0.845 * Pi / 180);
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
  moondata.clear;
end;

{
  The Great Red Spot, currently at longitude 61 in system II
}
function TJupiter.getRedSpotXY(spot_in_deg: Double): TXYCoord;
var
  spotlong: Double;
  coord: TXYCoord;
begin
  spotlong := angle(lambda2 - spot_in_deg * Pi / 180.0);
  
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
  longInRad := angle(lambda - long_in_deg * Pi / 180.0);

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
function upcomingEvents(jup: TJupiter; date: TDateTime; tothrs: Double): string;
var
  saveDate: TDateTime;
  interval: Integer;
  upcoming: string;
  moonnames: array[0..3] of string;
  d: TDateTime;
  lastmoondata: array[0..3] of TMoonData;
  moondata: TMoonData;
  verbose: Boolean;
  mins, whichmoon: Integer;
  nshadows, ntransits: Integer;
  thisevent: string;
  hasLastData: array[0..3] of Boolean;
  fichier : text;
begin

  assignfile(fichier,'galileens.txt');
  rewrite(fichier);
  saveDate := jup.getDate;
  
  interval := 1; { minutes }
  upcoming := 'Ephémérides des lunes de Jupiter pour les prochain(e)s : '
              + prettytime(tothrs) + ':' + #13#10#13#10;

  moonnames[0] := 'Io';
  moonnames[1] := 'Europe';
  moonnames[2] := 'Ganymède';
  moonnames[3] := 'Callisto';

  d := date;
  for whichmoon := 0 to 3 do
    hasLastData[whichmoon] := False;

  verbose := False;

  for mins := -30 to Trunc(tothrs * 60) - 1 do
  begin
    d := IncMinute(date, mins);
    jup.setDate(d);
    
    if (verbose) then
      upcoming := upcoming + #13#10 + DateTimeToStr(d) + #13#10;

    { Keep track of how many moons are involved in events }
    nshadows := 0;
    ntransits := 0;

    thisevent := '';
    for whichmoon := 0 to 3 do
    begin
      moondata := jup.getMoonXYData(whichmoon);
      
      if (verbose) then
      begin
        upcoming := upcoming + ' (' + IntToStr(whichmoon) + '):' + #13#10;
        { JSON.stringify omitted for brevity in verbose mode, but logic preserved }
      end;

      if (hasLastData[whichmoon]) then
      begin
        { Count total events }
        if not IsNaN(moondata.shadowx) then
          Inc(nshadows);
        if (moondata.transit) then
          Inc(ntransits);

        if IsNaN(moondata.moonx) and not IsNaN(lastmoondata[whichmoon].moonx) then
          thisevent := thisevent + DateTimeToStr(d) + ': '
                       + moonnames[whichmoon] + ' disparaît' + #13#10
        else if not IsNaN(moondata.moonx) and IsNaN(lastmoondata[whichmoon].moonx) then
        begin
          if not moondata.eclipse then
            thisevent := thisevent + DateTimeToStr(d) + ': '
                         + moonnames[whichmoon] + ' réapparaît' + #13#10;
        end
        else if moondata.transit and not lastmoondata[whichmoon].transit then
          thisevent := thisevent + DateTimeToStr(d) + ': ' + moonnames[whichmoon]
                       + ' : début du transit' + #13#10
        else if not moondata.transit and lastmoondata[whichmoon].transit then
          thisevent := thisevent + DateTimeToStr(d) + ': ' + moonnames[whichmoon]
                       + ' : fin du transit' + #13#10
        else if moondata.eclipse and not lastmoondata[whichmoon].eclipse then
          thisevent := thisevent + DateTimeToStr(d) + ': ' + moonnames[whichmoon]
                       + ' : début éclipse' + #13#10
        else if not moondata.eclipse and lastmoondata[whichmoon].eclipse then
          thisevent := thisevent + DateTimeToStr(d) + ': ' + moonnames[whichmoon]
                       + ' : quitte l''éclipse' + #13#10;

        if IsNaN(moondata.shadowx) and not IsNaN(lastmoondata[whichmoon].shadowx) then
          thisevent := thisevent + DateTimeToStr(d) + ': ' + moonnames[whichmoon]
                       + ' : l''ombre disparaît' + #13#10
        else if not IsNaN(moondata.shadowx) and IsNaN(lastmoondata[whichmoon].shadowx) then
          thisevent := thisevent + DateTimeToStr(d) + ': ' + moonnames[whichmoon]
                       + ' : l''ombre apparaît' + #13#10;
      end;

      { Logic for cloning: In Delphi, record assignment copies data }
      lastmoondata[whichmoon] := clone(moondata);
      hasLastData[whichmoon] := True;
    end; { end loop over whichmoon }

    if (thisevent <> '') and (nshadows + ntransits > 1) then
      upcoming :=   pluralize(ntransits, 'transit')
                  + ', ' + pluralize(nshadows, 'ombre')  + #13#10;
    upcoming := upcoming + thisevent;


  end;
  writeln(fichier,upcoming);
  close(fichier);
  if (saveDate <> 0) then
    jup.setDate(saveDate);
    Result := 'OK';
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

